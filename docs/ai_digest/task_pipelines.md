# Task Pipelines

ChrysaLisp implements a powerful, distributed pipeline system inspired by Unix
pipes, but engineered specifically for a massively parallel, MIMD architecture.

By utilizing the underlying message-passing microkernel, pipelines in ChrysaLisp
seamlessly stream data between independent tasks, regardless of whether those
tasks reside on the same CPU core or across a network of connected machines.

## Pipeline Operators

To balance maximum cluster utilization with minimizing communication overhead,
ChrysaLisp provides two distinct pipeline operators.

These operators give developers fine-grained control over physical task
placement across the network topology:

* **`|` (The Distribution Operator)**

* **`!` (The Grouping/Pinning Operator)**

### The `|` Operator: Emergent Distribution

The standard pipe operator `|` is designed to maximize parallel distribution and
CPU utilization.

When the pipeline encounters a `|`, it requests the kernel to launch the
following task using the `+kn_call_child` mode.

This triggers ChrysaLisp's emergent load-balancing protocol:

* The kernel evaluates the task load of the target node and its immediate
  network neighbors.

* If a neighbor is less loaded, the task creation request automatically "slips"
  downhill to the less busy node.

* The task ultimately spawns in a "local minimum" load valley.

Crucially, the search for this valley begins at the node of the *previous* task
in the pipeline (the data producer).

This ensures that while tasks are distributed for optimal CPU utilization, they
remain physically *near* the tasks they are communicating with, keeping network
routing hops short and latency low.

### The `!` Operator: Node Pinning

The `!` operator is designed for communication-intensive pipeline segments where
network latency or bandwidth constraints would bottleneck performance.

When the pipeline encounters a `!`, it requests the kernel to launch the
following task using the `+kn_call_open` mode.

This forces the kernel to bypass load balancing entirely.

The new task is strictly pinned to the *exact same node* as the previous task in
the pipeline.

This allows developers to securely group highly interactive pipeline stages onto
a single core (or hardware node).

Tasks grouped this way utilize hyper-fast local memory message passing.

The grouped block as a whole can still be distributed dynamically across the
cluster by surrounding it with standard `|` operators.

## Architectural Mechanics

The elegance of this system stems from ChrysaLisp's location-transparent
networking and the sequential chaining of Ephemeral Node Identities (`node_id`).

* When a pipeline is launched, the system instantiates the tasks sequentially
  from left to right (producer to consumer).

* The first task in the pipeline is launched relative to the node that initiated
  the pipeline command.

* As each task successfully launches, it replies with its unique, newly
  incarnated `net_id` (which contains its specific `node_id`).

* The pipeline manager takes this specific `node_id` and uses it as the launch
  target for the *next* task in the chain.

* The separator (`|` or `!`) preceding a task simply dictates whether the
  receiving kernel treats this target `node_id` as a flexible starting point for
  load balancing (`|`), or as a strict execution directive (`!`).

## Example Execution

Consider the following command line execution:

```vdu
(pipe-run "generate_data | filter_data ! transform_data | output_results")
```

Because of ChrysaLisp's sequential launch resolution, this creates the following
execution topology:

* `generate_data` is launched *near* the terminal/caller node (via load
  balancing).

* `filter_data` is launched *near* `generate_data` (via load balancing).

* `transform_data` is pinned to the *exact same node* as `filter_data`
  (bypassing load balancing).

* `output_results` is launched *near* `transform_data` (via load balancing).

This topology achieves optimal cluster distribution while ensuring the tight
`filter -> transform` loop suffers zero network overhead, perfectly taking the
shape of its computational requirements.