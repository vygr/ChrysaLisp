# Trashes command

The `trashes` command is a static analysis tool designed to calculate the
transitive register clobber state (the registers that are overwritten or
"trashed") for any given Virtual Processor (VP) function. Rather than relying
solely on source-code annotations, the tool performs symbolic register-trace
analysis directly on the compiled VP binary objects (located in `obj/vp/`).

By tracking register data flow, stack frames, and method dispatch paths,
`trashes` determines the precise footprint of register usage throughout the call
graph, ensuring compiler safety and allowing the assembler to optimize registers
across function boundaries.

## Symbolic Register-Trace Engine

At the core of the tool is `analyze-function`, which simulates the execution of
a compiled function's instruction stream. It maintains a symbolic state of
active registers and stack slots to track when a register is modified, spilled,
or restored.

### Register and Stack State Tracking

The tracer tracks register and stack lifecycles through several mechanisms:

* **State Mapping:** A register map (`reg_map`) tracks the symbolic origin of
  each register. When a register is initialized, it is mapped to itself.

* **Copies:** Register-to-register copies (`emit-cpy-rr`, `emit-cpy-ff`)
  transfer the symbolic state from the source register to the destination
  register.

* **Stack Spills and Reloads:** The tool models stack allocation (`emit-alloc`,
  `emit-free`) and push/pop operations (`emit-push`, `emit-pop`). When a
  register is spilled to the stack (`emit-cpy-ri`) and later reloaded into the
  same register (`emit-cpy-ir`), the tracer detects that its original value is
  restored and removes it from the active clobber set.

* **Register Modification:** Any instruction that modifies a register (without a
  valid restore path) invalidates its symbolic state and adds it to the active
  `trace_set`.

## Control Flow and State Merging

To handle complex control flow, `analyze-function` employs a multi-path tracing
model:

* **Path Splitting:** When the tracer encounters a conditional branch, it splits
  the execution path, spawning a new trace state at the target label.

* **Label Merging:** When multiple execution paths converge at a label
  (`emit-label`), the tracer merges their respective clobber sets (union).

* **Trace Termination:** Tracing along a path terminates when it reaches a
  return (`emit-ret`) or an unconditional jump (`emit-jmp`), at which point its
  final clobber state is merged into the function's overall clobber set.

## Dependency Tracking & Topological Sorting

To resolve transitive clobbers (where function `A` calls `B`, thereby inheriting
`B`'s clobbers), the tool must analyze functions in leaf-to-root order. This is
achieved using a topological sort (`tsort`) powered by `get-dependencies`.

### 1. Static Call Resolution

Static calls (`emit-call-p`, `emit-jmp-p`) are resolved directly to their target
function names using the local label map.

### 2. Bounded Virtual Call Resolution

For virtual method calls (`emit-call-i`, `emit-jmp-i`, `emit-call-r`,
`emit-jmp-r`), the compiler cannot determine the single target at compile time.
Instead, the dependency tracker uses the class database (`*class_db*`) to
perform bounded resolution:

* It extracts the static class bound `c` and method name `m` from the
  instruction.

* It finds all descendants (subclasses) of `c` in the class hierarchy using
  `get-descendants`.

* For each subclass, it resolves the specific implementing function using
  `resolve-method-function`.

* These implementing functions are registered as active dependencies of the
  caller, forcing them to be sequenced and analyzed first.

#### Dependency Resolution Logic

```file
cmd/trashes.lisp "(defun get-dependencies" ""
```

## Propagation & Convergence

Once the dependency order is established by `tsort`, `propagate-trashes`
processes each function sequentially.

### 1. Direct Propagation

During the initial pass, when `analyze-function` encounters a call to a
dependency that has already been analyzed, it retrieves its clobber set directly
from `db` and applies those clobbers to the caller's active state.

### 2. The Convergence Loop

To handle cyclical call graphs or recursive structures, `propagate-trashes`
executes an iterative convergence loop. The loop repeatedly runs over the
analyzed functions, union'ing clobber states across all call sites until the
sizes of all clobber sets stabilize (no further changes are detected).

```file
cmd/trashes.lisp ";converge remaining" ""
```

## Verification & Linting

The `trashes` command includes a linting mode (`-l` / `--lint`) that serves as a
verification tool against the source code documentation.

When run in linting mode, the tool:

1. Performs complete symbolic register tracing to calculate the active clobber
   set of each function.

2. Queries the documentation database (`*doc_db*`) to retrieve the
   developer-written `;trashes` comments from the source files.

3. Compares the documented register set with the calculated register set.

4. Emits a warning for any mismatches, ensuring that assembly comments are
   verified against the actual compiled machine behavior.

### Example Output (Linting Mode)

```vdu
Guest> trashes obj/vp/class/nums/lisp_add -l
WARNING: Mismatch in class/nums/lisp_add
  Documented: :r1-:r5, :r7-:r14, :f0-:f15
  Calculated: :r0-:r14, :f0-:f15
```
