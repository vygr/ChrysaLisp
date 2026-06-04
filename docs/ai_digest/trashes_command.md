# Trashes Command

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
  return (`emit-ret`) or an unconditional jump (`emit-jmp-p`), at which point
  its final clobber state is merged into the function's overall clobber set.

## Unified Inner Call Tracing

In compiled ChrysaLisp VP binary files, labels are renamed during the
compilation pre-pass to an auto-incrementing integer prefix (e.g., `_0`, `_1`,
`_2`).

The standard function header contains three sequential labels at the very
beginning of the instruction stream:

1. `fn_start` (compiled as `_0`)

2. `fn_name_start` (compiled as `_1`)

3. `fn_entry` (compiled as `_2`)

This means the actual executable entry point of any compiled function always
corresponds to the `_2` label.

The dependency tracker and symbolic tracer utilize a unified inner call tracing
format to handle both main entry points and local subroutines:

* **Main Entry Points:** External static and virtual calls target the main entry
  point by appending the `:_2` suffix to the function name (e.g.,
  `class/func/print:_2`).

* **Local Subroutines:** Internal subroutine calls (`emit-call`) within the same
  file are represented using the `parent:label` format (e.g.,
  `class/func/print:local_sub`).

This unified representation allows `tsort` to build a single, comprehensive
dependency graph containing both the main function entry points and their
internal local subroutines.

## Dependency Tracking & Topological Sorting

To resolve transitive clobbers (where function `A` calls `B`, thereby inheriting
`B`'s clobbers), the tool must analyze functions in leaf-to-root order. This is
achieved using a topological sort (`tsort`) powered by `get-dependencies`.

### 1. Static Call Resolution

Static calls (`emit-call-p`, `emit-jmp-p`) are resolved directly to their target
function names using the local label map and are appended with the `:_2` entry
point suffix.

### 2. Bounded Virtual Call Resolution

For virtual method calls (`emit-call-i`, `emit-jmp-i`, `emit-call-r`,
`emit-jmp-r`), the compiler cannot determine the single target at compile time.
Instead, the dependency tracker uses the class database (`*class_db*`) to
perform bounded resolution:

* It extracts the static class bound `c` and method name `m` from the
  instruction.

* It retrieves all implementing subclass overrides of `m` using
  `resolve-virtual-methods`.

* These implementing functions are registered with their `:_2` entry points as
  active dependencies of the caller, forcing them to be sequenced and analyzed
  first.

## Propagation & Convergence

Once the dependency order is established by `tsort`, `propagate-trashes`
processes each function and subroutine in the unified dependency list.

### 1. Direct Propagation

During the initial pass, when `analyze-function` encounters a call to a
dependency that has already been analyzed, it retrieves its clobber set directly
from `db` and applies those clobbers to the caller's active state.

### 2. Optimization: Memoized Instruction Caching

To avoid costly disk I/O and redundant file-parsing overhead, the tool utilizes
a memoized function `get-function-insts`:

```file
cmd/trashes.lisp "(defun get-function-insts" ""
```

This guarantees that each compiled VP object is opened and parsed only once during the lifetime of the command execution, accelerating the analysis.

### 3. Optimization: Selective Re-Analysis

During the convergence loop, a function is only re-analyzed if at least one of its direct dependencies (its `call_list`) was updated in the previous pass. 

```file
cmd/trashes.lisp ";converge remaining by re-running" "(setq changed_set next_changed))"
```

### 4. Optimization: Full-Clobber Short-Circuit

If a function's clobber set reaches the maximum available registers (32
registers, calculated via `(const (length +all_regs))`), its state cannot grow
any larger. The convergence loop detects this and bypasses any further
re-analysis of the function, providing an efficient short-circuit path.

## Verification, Linting, and Auto-Fixing

The `trashes` command provides verification and automatic correction features
via the linting (`-l` / `--lint`) and write-back (`-w` / `--write`) options.

### Linting Mode

When run in linting mode, the tool:

1. Performs complete symbolic register tracing to calculate the active clobber
   set of each function.

2. Queries the documentation database (`*doc_db*`) to retrieve the
   developer-written `;trashes` comments from the source files.

3. Compares the documented register set with the calculated register set.

4. Emits a warning for any mismatches, ensuring that assembly comments are
   verified against the actual compiled machine behavior.

### Automatic Write-Back (-w / --write)

The `-w` option extends the verification pipeline by automatically correcting
outdated or incorrect documentation directly within the source code:

* **Implied Linting:** Enabling the write-back option (`-w`) automatically
  implies and activates the linting (`-l`) mode.

* **In-Place Correction:** When a mismatch is detected, the tool uses the
  documentation database to locate the exact file path and line number of the
  incorrect `;trashes` comment.

* **Indentation Preservation:** The tool loads the source file into a `Document`
  buffer, measures the leading whitespace of the target line, removes the old
  comment, and writes back the updated list of calculated registers under the
  same indentation.

* **Safe Descending Processing:** To prevent modifications from shifting lines
  and invalidating downstream target coordinates, file updates are sorted and
  executed in descending line-number order.

### Example Output (Linting Mode)

```vdu
Guest> trashes obj/vp/class/nums/lisp_add -l
WARNING: Mismatch in class/nums/lisp_add
  Documented: :r1-:r5, :r7-:r14, :f0-:f15
  Calculated: :r0-:r14, :f0-:f15
```