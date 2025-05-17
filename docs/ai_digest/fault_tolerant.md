# Fault-Tolerant Distributed Processing in ChrysaLisp

ChrysaLisp's architecture is inherently designed to support robust distributed
processing. A core tenet of its philosophy is that distributed applications can be
structured as dynamic, restartable trees of tasks. When failures occur—be it a VP
node crashing or a communication link dropping—the system provides mechanisms to
detect these failures, isolate affected sub-trees, potentially restart or rebuild
those components, and allow the overall application to continue or gracefully
degrade. This document explores this philosophy using the Raymarch, Mandelbrot,
NetSpeed, and NetMon applications as case studies, illustrating how `lib/task/`
classes facilitate fault tolerance.

## Core Philosophy: Everything is a Restartable Tree Node

The ChrysaLisp approach to distributed computing views applications not as
monolithic entities but as collections of cooperating tasks. These tasks can be
spawned across a network of VPs, forming a logical processing tree.

* **Root Task:** An application typically starts with a main or root task.

* **Worker/Child Tasks:** This root task can spawn child tasks to perform specific
  sub-operations. These children might, in turn, spawn their own workers, forming
  deeper branches.

* **Dynamic Reconfiguration:** The key to fault tolerance is the ability to treat
  any part of this tree as potentially transient. If a node hosting a set of
  worker tasks fails, the parent task (or a management layer) can detect this and
  attempt to recreate that branch of the processing tree on other available nodes.

* **Ignoring Old Branches:** When a part of the tree is restarted, new mailboxes
  and task identifiers are typically used. This naturally leads to any messages
  from the old, now-orphaned branch being ignored, as the parent is no longer
  listening on the old reply channels. Orphaned tasks are encouraged to
  self-terminate after a period of inactivity.

This "sea of nodes" concept, where the network topology can change and tasks must
adapt, is central to ChrysaLisp's robustness.

## Key `lib/task/` Components for Fault Tolerance

1. **`Local` / `Farm` Classes (`lib/task/local.inc`, `lib/task/farm.inc`)**

    * **Purpose:** Manage a pool of worker tasks, distributing a set of independent
      jobs among them. `Farm` is a higher-level abstraction often built upon
      `Local`.

    * **Mechanism:**

        * **`create` callback:** Invoked to launch a new worker task when needed
          (e.g., initially, or when a worker fails). This callback is responsible
          for starting the worker script (e.g., an app-specific worker or a
          generic one like `lib/task/cmd.lisp`) on an available node.

        * **`destroy` callback:** Invoked when a worker task is deemed lost or
          needs to be shut down. Crucially for fault tolerance, this callback
          often **requeues the job** that the failed worker was processing.

        * **`:refresh` method:** Periodically called to check the liveness of
          worker tasks (often based on timestamps of last communication). If a
          worker is unresponsive (times out), `refresh` can trigger the `destroy`
          (and subsequently `create`) cycle.

        * **Job Dispatch:** Sends individual work units to available worker tasks.

    * **Fault Tolerance Aspect:** This is the primary mechanism for handling worker
      task failures. If a worker crashes or becomes unresponsive, the Farm detects
      this (via `:refresh` and timeouts), its job is requeued (via `destroy`), and
      a new worker is spawned (via `create`) to eventually pick up the requeued
      job or new jobs.

2. **`Global` Class (`lib/task/global.inc`)**

    * **Purpose:** Similar to `Farm`, but typically designed to maintain one worker
      task per *known node* in the network. Useful for tasks that need to run
      ubiquitously, like monitoring services.

    * **Mechanism:**

        * **`create` callback:** Invoked when a new node is detected in the network,
          to launch a worker on that node.

        * **`destroy` callback:** Invoked when a node is no longer detected, to
          clean up the associated worker.

        * **`:refresh` method:** Scans known nodes and updates its internal map,
          triggering `create` or `destroy` as nodes appear or disappear.

    * **Fault Tolerance Aspect:** Adapts to the dynamic topology of the VP network.
      If a node hosting a `Global` worker goes down, `destroy` is called. If a new
      node (or the same node rebooted) appears, `create` is called.

3. **`Pipefarm` Library (`lib/task/pipefarm.inc`)**

    * **Purpose:** To execute a list of *independent command-line jobs* in parallel,
      distributing them using a farm of worker tasks.

    * **Mechanism:**

        * Uses a `Local` farm internally.

        * Worker tasks are instances of `lib/task/cmd.lisp`.

        * `dispatch-job` sends command strings to workers.

        * Includes a `retry_timeout` for jobs. If a worker doesn't complete its
          command within this time, its job is effectively requeued by the
          underlying farm's `destroy`/`create` mechanism when the worker is
          refreshed.

    * **Fault Tolerance Aspect:** Leverages the `Local` farm's resilience for worker
      crashes. The `lib/task/cmd.lisp` worker itself also contributes by catching
      errors from the commands it runs.

4. **`lib/task/cmd.lisp` (Generic Command Worker)**

    * **Purpose:** A general-purpose worker for executing arbitrary command-line
      strings.

    * **Mechanism:**

        * Waits for a message containing a command string, a reply key, and a reply
          mailbox.

        * Executes the command string using `(pipe-run ...)`.

        * Crucially, it wraps the `pipe-run` in a `(catch ...)` block. This allows
          it to capture any Lisp errors that occur during the command's execution
          (including if the command itself fails to launch or errors out).

        * Captures all stdout (and stderr if `pipe-run` itself errors) into a
          string.

        * Prepends the `reply_key` to this output and sends it back.

        * Features an **inactivity timeout**: if it doesn't receive a job for a set
          period, it self-terminates, preventing orphaned workers from lingering
          indefinitely.

    * **Fault Tolerance Aspect:** Robustly executes commands, captures their output
      and errors, and self-cleans if orphaned.

5. **`Pipe` Class (`lib/task/pipe.inc`)**

    * **Purpose:** Manages a sequential pipeline of commands, where each stage can
      potentially run on a different VP node.

    * **Mechanism:** Splits the command line, launches each command as a task, and
      wires their stdin/stdout.

    * **Fault Tolerance Aspect:**

        * During construction, if a command in the pipeline fails to launch, it
          attempts to send abort signals to already launched commands.

        * The `:read` method can detect stream closures, indicating a command may
          have terminated or errored.

        * Individual commands within the pipe are responsible for their own error
          handling. If a piped command is itself run via `pipe-run` within
          `lib/task/cmd.lisp` (e.g. if it's a stage in a `pipe-farm` job), its
          errors can be captured.

6. **Mailbox Management & Stale Messages**

    * ChrysaLisp mailbox IDs (`net_id`s) are unique and non-repeating. When a
      mailbox is freed and a new one allocated (even for the "same" logical purpose
      after a restart), it gets a new ID.

    * This is a key strategy for handling failures: if a parent task restarts a
      worker (or a set of workers in a farm) and expects replies, it should use a
      *new reply mailbox*. Any messages from old, potentially failed or slow
      workers, sent to the *old* mailbox ID will be ignored or will fail delivery
      if the old mailbox has been explicitly freed. This prevents processing stale
      or erroneous results.

## Application Case Studies

### 1. Raymarch & Mandelbrot Demos (Fault-Tolerant Compute Farms)

* **Shared Goal:** Perform computationally intensive image rendering by dividing
  the work into tiles and processing them in parallel.

* **Mechanism:**

    * Both applications use the `Farm` class (likely via `Local`) to manage a pool
    of worker tasks.

    * The `create` callback in the main application starts app-specific worker
    tasks (`apps/raymarch/child.lisp`, `apps/mandelbrot/child.lisp`) on
    available nodes. These workers are responsible for rendering a single tile.

    * The main app maintains a queue of jobs (tiles to render). `dispatch-job`
    sends a tile's parameters to an available worker.

    * Workers compute the tile and send the resulting pixel data (or an error) back
    to a **reply mailbox** specified by the main app.

* **Fault Tolerance Strategies:**

    * **Worker Task Failure:**

        * If a worker task crashes or its node becomes unreachable, the `Farm`'s
          `:refresh` method (called periodically, as seen in `event_loops.md` for
          Raymarch's main loop) detects the unresponsiveness based on a timeout
          (e.g., `retry_timeout`).

        * The `Farm` invokes the `destroy` callback. For these applications,
          `destroy` **requeues the job** (the tile parameters) back into the main
          application's job list.

        * The `Farm` then invokes the `create` callback to start a new worker,
          which will eventually pick up the requeued job or other pending jobs.

    * **Ignoring Stale Messages (especially relevant for Mandelbrot):**

        * The Mandelbrot demo allows user interaction (zooming, panning), which
          triggers a recalculation. The `reset` function in `apps/mandelbrot/app.lisp`
          is critical:

            * It calls `(. farm :close)` to shut down existing workers.

            * It **recreates the `+select_reply` mailbox** using `(mail-free-mbox)`
              and `(mail-alloc-mbox)`.

            * It then re-initializes the `farm` and repopulates the `jobs` queue for
              the new view.

            * By changing the reply mailbox, any in-flight results from workers
              processing tiles for a *previous* view (which might be slow or from a
              now-defunct worker) are sent to an old, potentially non-existent or
              ignored mailbox. Only results for the *current* computation, sent to the
              *new* reply mailbox, are processed. This neatly handles the "old branch
              withering" concept.

        * **Orphaned Worker Self-Termination:** Both Raymarch and Mandelbrot child
          worker scripts (`child.lisp`) implement an inactivity timeout using
          `mail-timeout` on their own. If they don't receive a job message for a
          specified period, they assume they are orphaned and exit gracefully (`setq
          running :nil`). This prevents zombie worker tasks from accumulating.

### 2. NetSpeed (Illustrates `Global` for Per-Node Benchmarking)

* **Goal:** Run a standardized benchmark on all currently available VP nodes to
  measure their processing speed.

* **Mechanism (Hypothetical, based on typical `Global` usage):**

    * The main NetSpeed application would instantiate a `Global` farm.

    * The `create` callback for `Global` would be triggered whenever a new VP node
      joins the network. This callback would start `apps/netspeed/child.lisp` on
      the new node. The child task would run a series of VP instruction blocks and
      time their execution.

    * The `destroy` callback would be triggered if a node leaves the network,
      cleaning up resources associated with that node's worker.

    * The main app would collect and display benchmark results from all active
      children.

* **Fault Tolerance Strategies:**

    * **Dynamic Network Adaptation:** The `Global` farm automatically adapts to
      nodes appearing and disappearing. If a node running a benchmark worker
      crashes, `Global`'s `:refresh` will eventually detect its absence and call
      `destroy`. If it (or a new node) comes online, `create` will spawn a new
      worker.

    * **Worker Robustness:** `apps/netspeed/child.lisp` would perform the actual
      benchmark. Errors within its execution would ideally be caught and reported,
      or if it crashes, the `Global` farm would handle its disappearance.

      The focus here is less on requeueing a "job" (as the job is just "run on
      this node") and more on maintaining an up-to-date set of benchmarkers across
      the live network.

### 3. NetMon (Illustrates `Global` for Per-Node Monitoring)

* **Goal:** Monitor system statistics (task count, memory usage) on all VP nodes.

* **Mechanism:**

    * Uses `Global` as described in `event_loops.md`.

    * The `create` callback (called by `Global`) starts `apps/netmon/child.lisp` on
      newly detected nodes. The `key` passed to `create` is effectively the
      `node_id`.

    * The main app periodically iterates through the children managed by `Global`
      and sends them a poll request (a simple message to their main mailbox).

    * Child tasks, upon receiving the poll, gather local kernel statistics and send
      them back to the main app's reply mailbox.

* **Fault Tolerance Strategies:**

    * **Dynamic Network Adaptation:** Like NetSpeed, `Global` ensures `NetMon`
      adapts to network changes.

    * **Child Task Self-Termination:** The `apps/netmon/child.lisp` (as shown in
      `comms.md`) has an inactivity timeout. If the parent stops polling it (e.g.,
      because the parent itself crashed or the network link was severed), the child
      will eventually timeout and exit. This prevents orphaned monitoring agents.

    * **Parent-Side Timeout/Refresh:** The `Global` farm's `:refresh` method, when
      called by the parent `NetMon` app, checks for unresponsive children (nodes
      that disappeared). If a child's node is gone, `destroy` is called for its
      record. This ensures the parent isn't waiting indefinitely for replies from
      dead nodes.

### 4. General Command-Line Processing via `pipe-farm` (e.g., `grep -f`, `make` sub-commands)

* **Goal:** Execute multiple independent command-line tools in parallel across the
  network.

* **Mechanism:**

    * Commands like `grep -f` (file mode with multiple files) or `make` (when
      compiling multiple `.vp` files or generating documentation for multiple
      commands) use `pipe-farm`.

    * `pipe-farm` constructs a list of full command-line strings, each
      representing an independent job (e.g., `grep <pattern> <file1>`, `grep
      <pattern> <file2>`, or `cmd/grep.lisp -h`, `cmd/cat.lisp -h`).

    * It uses a `Local` farm of `lib/task/cmd.lisp` workers.

* **Fault Tolerance Strategies:**

    * **Worker (`cmd.lisp`) Failure:** If a `lib/task/cmd.lisp` worker crashes, the
      underlying `Local` farm (managed by `pipe-farm`) will detect this via its
      `refresh` and timeout mechanisms. The job (the command-line string) assigned
      to that worker will be requeued by the `destroy` callback, and a new
      `cmd.lisp` instance will be started by `create`.

    * **Individual Command Failure:** If a command *executed by* `lib/task/cmd.lisp`
      (e.g., `grep` itself) errors or produces stderr output, `lib/task/cmd.lisp`'s
      use of `pipe-run` (often with a `(catch ...)`) captures this output. The
      error message or stderr content is then returned as part of the result string
      to the `pipe-farm` caller.

    * **`cmd.lisp` Self-Termination:** If a `pipe-farm` operation is aborted
      prematurely or the main task dies, `cmd.lisp` workers will eventually
      self-terminate due to their inactivity timeout.

## Strategies for Building Fault-Tolerant Systems in ChrysaLisp

The examples above highlight several common strategies:

1. **Idempotent Workers & Job Requeueing:** For compute-bound tasks (`Raymarch`,
    `Mandelbrot`, `pipe-farm`), workers are designed to perform a discrete unit of
    work. If a worker fails, the `Farm`/`Local` manager requeues the *same job* for
    another worker. This assumes jobs are largely idempotent or that re-computation
    is acceptable.

2. **Dynamic Node Discovery and Management:** For services that need presence on
    all (or a selection of) nodes (`NetMon`, `NetSpeed`), the `Global` class
    automatically manages worker lifecycles as the network topology changes.

3. **Timeouts for Liveness and Progress:**

    * Farms use timeouts within their `:refresh` logic to detect unresponsive
      workers.

    * Individual workers (like `lib/task/cmd.lisp` or application-specific children)
      often have inactivity timeouts to self-terminate if orphaned.

    * Applications can use `(mail-timeout ...)` to implement their own detection of
      overdue replies.

4. **Isolation via Fresh Mailboxes:** When retrying operations or restarting
    components (like the Mandelbrot `reset`), allocating *new* reply mailboxes is a
    simple and effective way to ensure that only responses pertinent to the current
    attempt are processed, effectively isolating the new "branch" of the processing
    tree from stale messages from previous, failed attempts.

5. **Graceful Degradation & Error Reporting:** Instead of entire system collapse,
    failures are often localized. `lib/task/cmd.lisp` reporting command errors, or a
    farm continuing with remaining workers while restarting failed ones, allows the
    system to make progress where possible and report issues rather than halt.

## Conclusion

ChrysaLisp provides a powerful and flexible toolkit for building fault-tolerant
distributed applications. By embracing the philosophy of tasks as restartable tree
nodes and leveraging classes like `Farm`, `Global`, and `Pipefarm`, developers can
create systems that are resilient to common distributed computing failures. The
design patterns seen in applications like Raymarch (requeueing jobs, worker
self-termination) and Mandelbrot (recreating mailboxes to ignore stale messages)
demonstrate practical approaches to achieving robustness. The system's emphasis on
asynchronous messaging, dynamic service discovery, and explicit task lifecycle
management empowers developers to build applications that can indeed "sail on the
choppy waters" of a dynamic, distributed environment.
