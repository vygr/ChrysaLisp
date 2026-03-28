# Task Farming in ChrysaLisp

ChrysaLisp is fundamentally designed as a distributed, message-passing, MIMD
(Multiple Instruction, Multiple Data) operating system. To take full advantage
of this architecture, the OS provides a powerful set of **Task Farming** classes
located in `lib/task/`.

These classes abstract away the complexities of deploying, monitoring, and
self-healing distributed workloads across a cluster of nodes. They empower
developers to write applications that are "formless and shapeless," seamlessly
scaling from a single core to a massive network of machines.

There are three primary Farm classes, each serving a distinct structural
topology: **`Farm`**, **`Global`**, and **`Local`**.

## 1. The `Farm` Class (Fixed-Size Distributed Pool)

**Source:** `lib/task/farm.inc`

The `Farm` class maintains a strictly defined number of worker tasks distributed
anywhere across the network. If a worker crashes, drops off the network, or
times out, the `Farm` automatically respawns it to maintain the requested pool
size. This is used for workloads where file-system locality does not matter
(e.g., pure computation).

### Constructor

```vdu
(Farm fnc_create fnc_destroy size)
```

* **`fnc_create`**: A callback to spawn a new task.

* **`fnc_destroy`**: A callback to clean up when a task dies or is retired.

* **`size`**: The exact number of tasks to maintain.

### How it operates

On initialization, the `Farm` invokes `fnc_create` `size` times. Periodically,
the application calls `(. farm :refresh timeout)`. The farm checks the
timestamps and network presence of all workers. If a worker has vanished or
exceeded the timeout, `fnc_destroy` is called, followed by `fnc_create` to heal
the pool.

### Real-World Examples

* **`apps/demos/raymarch/app.lisp` & `apps/science/mandelbrot/app.lisp`**: Both
  use `(Farm create destroy (* 2 (length (lisp-nodes))))`. By requesting twice
  as many workers as there are available nodes, they ensure the network remains
  highly saturated with rendering jobs, maximizing throughput. Pure math
  rendering doesn't need disk access, so it can go anywhere.

* **`apps/games/chess/app.lisp`**: Uses `(Farm create destroy 1)` to maintain
  exactly one resilient background chess engine task.

## 2. The `Global` Class (Node Singleton Daemon)

**Source:** `lib/task/global.inc`

The `Global` class ensures that **exactly one instance** of a task is running on
*every* known node in the ChrysaLisp network.

### Constructor

```vdu
(Global fnc_create fnc_destroy)
```

### How it operates

Unlike `Farm` which manages a specific *number* of tasks, `Global` maps tasks
1:1 to network nodes. When `(. global :refresh timeout)` is called, it polls
`(lisp-nodes)`. If a new node joins the network, `fnc_create` is triggered for
that specific node. If a node leaves, `fnc_destroy` cleans up its tracking data.

### Real-World Examples

* **`apps/system/netmon/app.lisp` & `apps/system/netspeed/app.lisp`**: These
  system monitors need to query every computer/core in the cluster. They use
  `Global` to automatically deploy a tiny reporting child task to every node. As
  new nodes boot up and join the network, they immediately appear on the monitor
  without user intervention.

## 3. The `Local` Class (File-System Bound / Local Pool)

**Source:** `lib/task/local.inc`

The `Local` class is designed to keep the pool of workers **inside the same file
system**. In a distributed OS where nodes can span physical machines, tasks that
require disk access (like compiling code or reading text files) will fail if
sent to a remote node without shared storage. `Local` restricts execution to
local resources while still offering elastic scaling.

### Constructor

```vdu
(Local fnc_create fnc_destroy [herd_max herd_init herd_growth])
```

* **`herd_max`**: Maximum number of workers allowed (default `+max_int`).

* **`herd_init`**: Initial number of workers to spawn (default `1`).

* **`herd_growth`**: How many new workers to spawn when the pool is starved
  (default `1`).

### How it operates

It starts with `herd_init` workers confined to local nodes. When a worker
successfully reports back, the application registers it via `(. farm :add_node
node)`. If the farm determines it still has work to do and hasn't hit
`herd_max`, it utilizes `herd_growth` to spawn more workers. This creates a pool
that scales up to match local hardware capabilities without ever leaking
file-dependent tasks to remote machines.

### Real-World Examples

* **`lib/asm/asm.inc` (The VP Assembler)**: Uses `(Local (const create) (const
  destroy) max_workers init_workers new_workers)`. When you run `make`, the
  build system needs to read `.vp` files and write object files to disk. `Local`
  guarantees these compile jobs stay on the machine that actually holds the
  source code.

* **`lib/task/cmd.inc` (`pipe-farm`)**: System commands like `docs`, `imports`,
  `grep`, and `wc` use `pipe-farm`. Since these commands process local text and
  source files, they internally wrap a `Local` farm to map shell commands
  efficiently across local cores without breaking due to missing files on remote
  nodes.

---

## Controlling Task Distribution & Strategies

As a developer, you have two primary ways to influence *where* your tasks execute: **Application-Directed Placement** and **Kernel-Assisted Placement**.

### The `create` Callback Signature

To understand distribution, you must look at the `create` callback provided to
the Farms:

* **`Farm`**: `(create key val network_nodes)` - Receives a list of *all* nodes.

* **`Local`**: `(create key val worker_nodes)` - Receives a list of currently
  active *local worker* nodes.

* **`Global`**: `(create key now)` - `key` is actually the target Node ID string
  itself.

### Strategy 1: Edge Biasing (Application-Directed)

When a `Farm` or `Local` pool calls your `create` function, you can slice or
filter the `nodes` list before picking a destination.

In `lib/asm/asm.inc` and `apps/science/mesh/app.lisp`, you will see this exact
pattern:

```vdu
(defun create (key val nodes)
    ; bias to go out to the edge nodes
    (defq nodes (slice nodes (/ (length nodes) 2) -1))
    (open-task "path/to/child.lisp" (elem-get nodes (random (length nodes)))
        +kn_call_child key (elem-get select +select_task)))
```

By slicing the node list in half and taking the upper bound (`-1`), the
application biases workloads (like compilation or rendering) to the outer
"leaves" of the network topology.

### Strategy 2: ChrysaLisp "Downhill" Load Balancing (Kernel-Assisted)

You don't always need to perfectly calculate where a task should go.
ChrysaLisp's Kernel supports emergent load balancing.

When you use `+kn_call_child` in your `create` function (via `open-task` or
`open-child`), you are opting into the kernel's fluid distribution.

1. You target a node (e.g., a random node from the provided list).

2. The receiving kernel checks its own `task_count` against its neighbors.

3. If a neighbor is less loaded, the kernel forwards the *task creation request*
   to that neighbor instead of spawning it locally.

4. This repeats until the request finds a "local minimum" (a valley in the
   network load) and spawns there.

Because of this, simply picking a random node:

```vdu
(elem-get nodes (random (length nodes)))
```

...is actually highly effective. The application scatters the "seeds" randomly,
and the ChrysaLisp kernel acts like gravity, rolling those seeds down into the
least-loaded pockets of the CPU cluster.

### Standard Lifecycle Management

Regardless of the strategy, a well-behaved `create` and `destroy` pair looks
like this:

```vdu
(defun create (key val nodes)
    ; 1. Pick a node (randomly, or biased)
    (defq target_node (elem-get nodes (random (length nodes))))
    ; 2. Ask the kernel to launch the child task
    (open-task "my_child_app.lisp" target_node +kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
    ; 1. If we have a tracked child, send it a termination message (empty string)
    (when (defq child (get :child val)) (mail-send child ""))
    ; 2. If the task had an active job attached, recycle the job back into our queue!
    (when (defq job (get :job val))
        (push jobs job)
        (undef val :job :timestamp)))
```

Coupled with a timer triggering `(. farm :refresh retry_timeout)`, this simple
lifecycle ensures your application scales elegantly, respects file-system
boundaries when needed, and survives node failures seamlessly.