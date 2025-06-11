# The ChrysaLisp VP Function: A Unified Binary Primitive for Code and Polymorphism

Document detailing the VP function format and the unified calling mechanism.

## 1. Abstract

In conventional systems, a firm distinction exists between executable functions
and the data structures that support object-oriented polymorphism, such as
virtual method tables (vtables). This separation introduces complexity into the
compiler, linker, and runtime. ChrysaLisp fundamentally rejects this dichotomy
by employing a single, self-describing binary format for all callable entities.
**A VP class vtable is not a special data structure; it is, at a binary level, a
VP function with a zero-length code section.**

This unifying design, combined with a multi-stage "linkerless" build process,
eliminates the need for a traditional system linker, enables sub-second
full-system rebuilds, and results in a runtime where static function calls and
virtual method dispatch are mechanically almost identical and maximally
efficient. A key optimization in this process is the **consolidation and
stripping** of symbolic dependency information, resulting in a minimal boot
image.

## 2. The Anatomy of the VP Function Block

Every compiled unit in ChrysaLisp, be it a standalone function or a class
vtable, adheres to a single, consistent binary layout. This structure is defined
by `+fn_header` in `lib/asm/lisp.inc`. The core principle is that a function is
a block of data containing a descriptive header whose fields are **offsets from
the start of the block itself**.

**On-Disk `.vp` File Layout:**

```
+---------------------------------+  <-- Function Address (F)
|          FN_HEADER              |
| - uint64 ln_fnode               |  (For linking into lists by the loader)
| - ushort length (total size)    |
| - ushort entry                  | ---------> relative offset to code start
| - ushort links                  | ---------> relative offset to links table
| - ushort paths                  | ---------> relative offset to paths table
| - ushort stack (req'd size)     |
| - offset pathname (symbolic)    |
+---------------------------------+
|                                 |
|         EXECUTABLE CODE         |  <-- Address (F + entry offset)
|         (VP Instructions)       |
|                                 |
+---------------------------------+
|                                 |
|           LINKS TABLE           |  <-- Address (F + links offset)
|   (Parallel to Paths Table)     |
|                                 |
+---------------------------------+
|                                 |
|           PATHS TABLE           |  <-- Address (F + paths offset)
|   (Symbolic names for links)    |
|                                 |
+---------------------------------+
```

*   **Header:** Provides metadata, including offsets to the other sections.

*   **Executable Code:** A sequence of VP instructions. This is the "body" of
    the function.

*   **Links Table:** A table of placeholders for pointers. For a normal
    function, these will point to external dependencies.

*   **Paths Table:** A table of null-terminated strings that holds the symbolic
    names for the entries in the `links` table. `paths[i]` is the name for the
    function that will be referenced by `links[i]`.

## 3. The Unifying Principle: The VTable as a Code-less Function

The architectural elegance of ChrysaLisp is revealed in its implementation of a
class vtable. **A vtable is simply a VP function with a zero-length code
section, where the `entry` offset points directly to the `links` table.**

The `def-class` function in `lib/asm/class.inc` generates this structure:

1.  It calls `(def-func 'class/my_widget/vtable)` to begin creating a standard
    function block.

2.  It emits **zero** VP instructions.

3.  For each method (e.g., `:draw`), it adds the method's implementation
    function path (e.g., `'gui/view/draw'`) to the `*links*` list and the
    method's name (`":draw"`) to the `*paths*` list.

4.  When `def-func-end` is called, it sets the `entry` offset in the header to
    be identical to the `links` offset.

The resulting binary layout for a vtable-function is identical in structure to a
normal function, allowing the boot process to treat them the same way.

## 4. The Three-Stage Lifecycle of a Linkage

A pointer in a `links` table (whether for a static call or a vtable slot)
evolves through three distinct states from source code to a running system.

### Stage 1: Compile Time - The Symbolic Blueprint

*   **Actor:** `asm` tool (`asm.lisp`).

*   **State:** The `.vp` file on disk contains symbolic dependencies. Each
    function has its own `paths` table containing the string names of its
    dependencies. If ten different functions call `sys_mem:alloc`, the string
    `"sys/mem/alloc"` is duplicated in ten separate files.

### Stage 2: Pre-Linking - The `boot-image` Optimizer

*   **Actor:** The `boot-image` function (`lib/boot/image.inc`).

*   **Process:** This tool performs two critical optimizations:

    1. **Path Consolidation and Stripping:** It scans every function's `paths`
        table and builds a **single, global string table** at the end of the
        boot image, containing every unique path name exactly once. It then
        **patches the `paths` offset in every function's header** to point to
        the correct location within this new global table. The original,
        per-function path tables are completely discarded, shrinking the image.

    2. **Link Resolution:** Using the original path information (before it's
        discarded), it finds the final offset of each target function within the
        image. It then **patches the `links` table** with a *relative offset*
        from the link's own address to the header of the target function.

*   **Result:** The on-disk `boot_image` is a compact binary with relative code
    offsets and a single, consolidated string table.

**Optimized `boot_image` Layout on Disk:**

```
+----------------------------------+
| Func A Header (paths -> to global)|
| Func A Code                      |
| Func A Links (rel. offsets)      | <-- Original paths table STRIPPED
+----------------------------------+
| VTable B Header (paths -> global)|
| VTable B Links (rel. offsets)    | <-- Original paths table STRIPPED
+----------------------------------+
| ... etc ...                      |
+----------------------------------+
|        GLOBAL PATHS TABLE        | <-- SINGLE, deduplicated string table
|    "sys_mem:alloc"               |
|    ":draw"                       |
|    ...all other unique paths...  |
+----------------------------------+
```

#### Stage 3: Load Time - Final Wiring

*   **Actor:** `sys/load/init`.

*   **Process:** This is the first code to run and is carefully written to be
    self-contained, making no external calls until its work is done.

    1. It receives the `image_base_address` where the OS was loaded.

    2. It iterates through every function block's `links` table.

    3. For each slot, it reads the `relative_offset`, calculates the target's
        absolute memory address
        (`absolute_address = link_slot_address + relative_offset`), and **writes
        this absolute address back into the `links` slot.**

*   **Result:** The system is now live. Every `links` table and every vtable has
    been transformed into a flat, in-memory array of direct, absolute function
    pointers.

## 5. The Unified Call Mechanism in Action

This architecture ensures that by the time any application code runs, all
symbolic resolution is complete.

*   **Static Call (`f-call`)**: A call to a known function like
    `(f-call 'sys_mem :alloc ...)` is resolved by the `boot-image` and `loader`
    into an absolute memory address. The final machine code performs a direct
    `call [absolute_address_of_sys_mem_alloc]`.

*   **Virtual Call (`v-call`)**: A virtual method call like
    `(v-call 'my_widget :draw ...)` is resolved at runtime with maximum
    efficiency:

    1. **Get VTable Address:** The address of the object's vtable-function is
        retrieved from its header: `mov r1, [my_widget_instance + obj_vtable]`.
        `r1` now points to the header of a function like `class/button/vtable`.

    2. **Get Method Pointer:** The compiler knows the method index for `:draw`
        (e.g., `0`). It generates code to load the pointer from the
        already-patched vtable:
        `mov r2, [r1 + entry_offset + (method_index * ptr_size)]`. This is a
        single memory read that loads the absolute address of the correct
        `:draw` implementation.

    3. **Call:** The final instruction is an indirect call: `call r2`.

## 6. Conclusion

The ChrysaLisp VP function format is the architectural lynchpin of the system.
By treating functions and vtables as the same binary primitive, it leverages a
single set of tools for compilation and loading. The `boot-image` optimizer is
essential, performing both link resolution and string table consolidation to
produce a compact, efficient binary. The load-time wiring is a trivial,
single-pass operation.

The result is an architecture of radical simplicity and performance. There is no
complex linker, and no runtime overhead for virtual method dispatch beyond a
single memory indirection, making it as fast as a standard C++ vtable call while
being part of a far more dynamic and flexible system. This unified concept is a
prime example of ChrysaLisp's core philosophy: achieving superior performance
and robustness through elegant, first-principles design.