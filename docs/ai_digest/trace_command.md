# Trace Command

The `trace` command is a static analysis tool designed to calculate the
transitive register clobber state (the registers that are overwritten or
"trashed") for any given Virtual Processor (VP) function. Rather than relying
solely on source-code annotations, the tool performs symbolic register-trace
analysis directly on the compiled VP binary objects (located in `obj/vp/`).

By tracking register data flow, stack frames, and method dispatch paths, `trace`
determines the precise footprint of register usage throughout the call graph,
ensuring compiler safety and allowing the assembler to optimize registers across
function boundaries.

## Symbolic Register-Trace Engine

At the core of the tool is `analyze-function`, which simulates the execution of
a compiled function's instruction stream. It maintains a symbolic state of
active registers and stack slots to track when a register is modified, spilled,
or restored.

### Register and Stack State Tracking

The tracer tracks register and stack lifecycles through several mechanisms:

* **State Mapping:** A register map (`vpmap`) tracks the symbolic origin of each
  register. When a register is initialized, it is mapped to itself.

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

## In-Context Subroutine Evaluation (Local Calls)

By design, the Virtual Processor (VP) assembler restricts the `emit-call`
instruction to local targets within the same compiled function block. It is
physically impossible to branch outside the local function using `emit-call`.

Because of this structural boundary, the symbolic tracer resolves local
subroutines inline in-context. This is achieved by maintaining an abstract
`call_stack` list of return addresses within each path's trace state:

* **On `emit-call`:** The tracer retrieves the target label's PC, pushes the
  current `*pc*` onto the path's `call_stack`, and jumps directly to the
  subroutine.

* **On `emit-ret`:** If the path's `call_stack` is not empty, the tracer pops
  the return address and continues execution after the caller. If the
  `call_stack` is empty, it is a real return from the main function, so the
  tracer merges the clobber state and terminates the path.

This in-context evaluation tracks register modifications and restorations within
local subroutines, avoiding the need to treat internal subroutines as separate
dependency nodes.

## Dependency Tracking & Topological Sorting

To resolve transitive clobbers (where function `A` calls `B`, thereby inheriting
`B`'s clobbers), the tool must analyze functions in leaf-to-root order. This is
achieved using a topological sort (`tsort`) powered by `get-dependencies`.

Because local subroutines are fully resolved in-context during execution, they
are excluded from the dependency graph. This eliminates the need to append `:_2`
or other entry suffixes to functions, simplifying both the dependency tracking
and propagation logic.

### 1. Static Call Resolution

Static calls (`emit-call-p`, `emit-jmp-p`) are resolved directly to their target
function names (such as `"sys/mem/copy"`) using the local label map.

### 2. Bounded Virtual Call Resolution

For virtual method calls (`emit-call-i`, `emit-jmp-i`, `emit-call-r`,
`emit-jmp-r`), the compiler cannot determine the single target at compile time.
Instead, the dependency tracker uses the class database (`*class_db*`) to
perform bounded resolution:

* It extracts the static class bound `c` and method name `m` from the
  instruction.

* It retrieves all implementing subclass overrides of `m` using
  `resolve-virtual-methods`.

* These implementing functions are registered directly as active dependencies of
  the caller, forcing them to be sequenced and analyzed first.

#### Dependency Resolution Logic

```file
cmd/trace.lisp "(defun get-dependencies" ""
```

## Propagation & Convergence

Once the dependency order is established by `tsort`, `propagate-trashes`
processes each function in the dependency list.

### 1. Direct Propagation

During the initial pass, when `analyze-function` encounters a call to a
dependency that has already been analyzed, it retrieves its clobber set directly
from `db` and applies those clobbers to the caller's active state.

### 2. Optimization: Memoized Instruction Caching

To avoid costly disk I/O and redundant file-parsing overhead, the tool utilizes
a memoized function `get-function-insts`:

```file
cmd/trace.lisp "(defun get-function-insts" ""
```

This guarantees that each compiled VP object is opened and parsed only once
during the lifetime of the command execution, accelerating the analysis.

### 3. Optimization: Selective Re-Analysis and Double-Buffering

During the convergence loop, a function is only re-analyzed if at least one of
its direct dependencies (its `call_list`) was updated in the previous pass.

To optimize memory usage, `changed_set` and `next_changed` are declared outside
the loop, cleared in-place with `(. next_changed :empty)`, and swapped at the
end of each pass.

```file
cmd/trace.lisp ";converge remaining by re-running" "(defq t_set changed"
```

### 4. Optimization: Full-Clobber Short-Circuit

If a function's clobber set reaches the maximum available registers (32
registers, calculated via `(const (length +all_regs))`), its state cannot grow
any larger. The convergence loop detects this and bypasses any further
re-analysis of the function, providing an efficient short-circuit path.

## Verification, Linting, and Auto-Fixing

The `trace` command provides verification and automatic correction features
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
Guest> trace obj/vp/class/nums/lisp_add -l
WARNING: Mismatch in class/nums/lisp_add
  Documented: :r1-:r5, :r7-:r14, :f0-:f15
  Calculated: :r0-:r14, :f0-:f15
```