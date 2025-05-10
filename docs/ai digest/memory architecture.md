## ChrysaLisp Memory Architecture: From Low-Level Allocation to Lisp Objects

ChrysaLisp employs a manual memory management scheme based on reference counting, deliberately eschewing an automatic garbage collector. This design choice prioritizes performance, predictable latency, and a smaller runtime footprint, at the cost of requiring careful management of object lifecycles, especially concerning circular references. The memory architecture is layered, starting from a fundamental system memory allocator, building up to a sophisticated VP (Virtual Processor) class library, which then forms the basis for all Lisp language objects and the interpreter itself.

---

### Level 1: The Foundation - System Memory Allocation (`sys_mem` and `sys_heap`)

At the lowest level, ChrysaLisp's memory management is handled by the static `sys_mem` class. This class is the primary interface for all memory allocation and deallocation requests within the system.

1.  **`sys_mem` - The Central Allocator:**
    *   **Tiered Heap Management:** `sys_mem` doesn't typically allocate directly from the host OS for every small request. Instead, it manages an array of `sys_heap` instances. As seen in `sys/mem/class.vp` (`:statics_init`), it initializes a series of heaps, each responsible for a different range of cell sizes (e.g., starting from `mem_cell_min_size` and doubling up to `mem_cell_max_size`). This is a common strategy for **pool allocation** or **slab allocation**, designed to reduce fragmentation and improve the speed of allocating commonly sized objects.
    *   **Allocation Strategy (`sys_mem :alloc`):** When a request for memory is made via `sys_mem :alloc`, it first adds `sys_mem_header_size` to the requested amount. Then, it iterates through its managed `sys_heap` instances to find the smallest heap whose `hp_heap_cellsize` can satisfy the adjusted request. The allocation is then delegated to that specific `sys_heap`.
    *   **Memory Header (`sys_mem_header`):** Each block of memory allocated by `sys_mem` is prepended with a `sys_mem_header`. A crucial field in this header is `heap` (a pointer), which stores a reference back to the specific `sys_heap` instance from which this block was allocated. This is vital for `sys_mem :free` to know which heap to return the memory to.
    *   **Other Operations:** `sys_mem` also provides `calloc` (alloc + zero fill), `realloc`, `recalloc` (realloc + zero new space), `fill`, `copy`, `avail` (to query free memory), and `collect` (to trigger collection on all its heaps).

2.  **`sys_heap` - The Pool/Block Allocator:**
    *   **Block-Based Allocation:** Each `sys_heap` instance is responsible for a specific cell size. When its own free list of cells is exhausted, it allocates a larger chunk of memory (an `hp_block`) from the host OS using `host_os :pii_mmap` (as seen in `sys_heap :alloc`).
    *   **Cell Carving:** This `hp_block` is then carved up into multiple fixed-size `hp_cell`s. Each `hp_cell` contains a `block` pointer back to its parent `hp_block` and a `next` pointer for linking into a free list.
    *   **Free Lists:** `sys_heap` maintains a primary free list (`hp_heap_free_flist`) of available `hp_cell`s. When a cell is freed via `sys_heap :free` (which is an inline call to `hp-freecell`), it's added to this list.
    *   **Collection (`sys_heap :collect`):** This method performs two main passes:
        1.  It iterates through the global `hp_heap_free_flist`, moving each free cell to the local free list of its parent `hp_block` and incrementing the block's `freecnt`. If a block's `freecnt` equals its `cellcnt` (meaning all its cells are free), the entire `hp_block` is unmapped using `host_os :pii_munmap`, returning memory to the OS.
        2.  It then iterates through all remaining `hp_block`s and splices their local free lists (if any) onto the global `hp_heap_free_flist`, consolidating available cells.
    *   **Initialization (`sys_heap :init`):** Sets up the `cellsize` for this heap (aligned to `+ptr_size`) and calculates an appropriate `blocksize` (rounded up to `ld_page_size`) to hold a number of these cells.

---

### Level 2: The VP Object System and Lifecycle

Built upon `sys_mem`, the ChrysaLisp VP class library defines a common object model. All objects in this system inherit, directly or indirectly, from the base `obj` class.

1.  **The `obj` Base Class (`class/obj/class.inc`, `class/obj/class.vp`):**
    *   **Structure:** Every `obj` instance (and thus every object in the system) begins with:
        *   `vtable` (pptr): A pointer to the object's virtual method table. This enables polymorphism.
        *   `count` (uint): The reference counter for this object.
    *   **Core Lifecycle Methods:**
        *   **Creation:**
            *   Typically initiated by a static `ClassName :create` method.
            *   This calls `sys_mem :alloc` to get raw memory (which includes space for the `sys_mem_header` plus the object's actual size). The address returned to the object system is after the `sys_mem_header`.
            *   Then, the instance method `ClassName :init` is called. This method is responsible for:
                *   Setting the `vtable` pointer.
                *   Initializing the `count` to 1.
                *   Initializing any class-specific fields.
                *   Crucially, `:init` methods typically call their superclass's `:init` method (`s-call 'ClassName :init ...`) to ensure the entire inheritance chain is initialized.
        *   **Reference Counting:**
            *   `obj :ref (object)`: Increments the `count` field of the object.
            *   `obj :deref (object)`: Decrements the `count`. If `count` becomes 0, it calls `obj :destroy(object)`.
            *   `obj :ref_if` and `obj :deref_if` are null-safe versions.
        *   **Destruction:**
            *   `obj :destroy (object)`: This is the entry point for object destruction when its reference count reaches zero.
            *   It calls the object's virtual `:deinit` method. `ClassName :deinit` is responsible for:
                *   Releasing any resources held by the object (e.g., dereferencing other objects it holds pointers to, freeing other memory).
                *   Calling its superclass's `:deinit` method (`s-jump 'ClassName :deinit ...` for tail call optimization or `s-call`).
            *   After `:deinit` completes for the entire chain, `obj :destroy` calls `sys_mem :free(object)` to return the object's memory (including its `sys_mem_header`) to the `sys_mem` allocator.

2.  **Inheritance and Specialization:**
    *   Subclasses (e.g., `array`, `num`, `list`) inherit the `obj` structure and extend it with their own fields.
    *   They override virtual methods like `:deinit`, `:print`, `:hash`, `:type` to provide specialized behavior.
    *   For example, `list :deinit` will iterate through its elements and call `obj :deref` on each object pointer it holds before calling the `array :deinit` (its superclass). `array :deinit` in turn frees its dynamic buffer if it wasn't using the small internal `e0-e3` buffer, before calling `obj :deinit`.

---

### Level 3: Lisp Objects as VP Class Instances

A fundamental design principle in ChrysaLisp is that **Lisp data types are directly implemented as instances of VP classes.** There isn't a separate layer of Lisp "cells" or "tagging" on top of the VP objects for common types.

1.  **Direct Mapping:**
    *   **Numbers (`num`, `fixed`, `real`):** A Lisp number like `123` or `1.5` is an instance of the `num` or `fixed` VP class, respectively. The actual numeric value is stored in the `value` field (e.g., `num_value`) defined in their respective `def-struct`.
    *   **Strings (`str`):** A Lisp string `"hello"` is an instance of the `str` VP class. The character data is pointed to by `str_data` (or stored in the flexible array member part of the object), and its length is stored in `str_length`. Strings are immutable.
    *   **Symbols (`sym`):** A Lisp symbol like `'my-var` is an instance of the `sym` VP class, which inherits from `str`. Symbols are interned (guaranteed unique instances for the same name) using a global hash table, typically managed within `sys/statics/statics_sym_intern`.
    *   **Arrays (`array` and its typed variants `nums`, `fixeds`, `reals`, `path`):**
        *   A Lisp `(array 1 2 3)` or `(nums 10 20)` is an instance of the `array` or `nums` VP class.
        *   These classes store a `begin` pointer to a dynamically allocated (or small internal `e0-e3`) buffer, along with `length` and `capacity`.
        *   For typed arrays like `nums`, the `begin` buffer holds raw numeric values.
    *   **Lists (`list`):** A Lisp `(list 'a 1 "b")` is an instance of the `list` VP class.
        *   `list` inherits from `array`. Its `begin` buffer stores an array of `obj*` (pointers to other VP objects).
        *   `list` methods are overridden to perform reference counting (`obj :ref`, `obj :deref`) on the objects they store when elements are added, removed, set, or when the list itself is copied or destroyed. For example, `list :clear` iterates and dereferences all elements before calling `array :clear`. `list :set_elem` dereferences the old element at an index before storing a new one (assuming the new one is already ref'd by the caller).
    *   **Hash Maps (`hmap`):** A Lisp environment or a user-created hash map is an instance of the `hmap` VP class. It manages buckets of key-value pairs, where keys are typically `sym` objects and values are `obj*`. `hmap` methods also handle reference counting for stored values.
    *   **Functions and Macros (Lisp level):** When a Lisp `(lambda ...)` or `(macro ...)` is defined, it results in a Lisp `list` structure representing the closure (lambda/macro keyword, parameter list, body, and a reference to its lexical environment). This list itself is a `list` VP object. Compiled VP functions are distinct binary objects (see Level 5).

2.  **The Lisp Interpreter Instance (`lisp` class):**
    *   The Lisp interpreter itself runs as an instance of the `lisp` VP class (`class/lisp/class.inc`).
    *   This `lisp` object holds the state for a particular Lisp execution context:
        *   Pointers to its `stdin`, `stdout`, `stderr` stream objects.
        *   A pointer to its current `environment` (an `hmap` object).
        *   Pre-interned `sym` objects for common Lisp keywords (`sym_lambda`, `sym_nil`, `sym_t`, etc.) to speed up parsing and evaluation.
        *   Pointers to core FFI function objects (`func_ffi`, `func_lambda`).
        *   Its own evaluation `stack`, `nextsym` counter for `gensym`, etc.
    *   The methods of the `lisp` class (e.g., `:read`, `:repl_eval`, `:repl_apply`, `:lisp_add`) implement the Lisp language primitives and REPL functionality, operating on other VP objects.

---

### Level 4: Memory Management in Lisp Code

Given the direct mapping of Lisp objects to reference-counted VP objects and the absence of a GC:

1.  **No Automatic Garbage Collection:** This is the most significant aspect. Memory for objects is reclaimed only when their reference count drops to zero.
2.  **Implicit Reference Counting:** Most standard Lisp functions that create or manipulate objects handle reference counting correctly behind the scenes via their FFI implementations (which call the appropriate VP class methods like `:ref`, `:deref`, `:create`, `:destroy`).
    *   Example: `(list a b)` creates a new `list` object. The `list :lisp_list` FFI implementation ensures the returned list is properly initialized with a ref count of 1. If `a` and `b` are objects, the `list` will also hold references to them (their ref counts would have been incremented when added to the list, if the list constructor does this, or if they were already live).
    *   Example: When a Lisp function returns, objects created locally within its environment that are not returned or captured in closures will eventually have their reference counts decremented as the environment and its bindings are dismantled, leading to their deallocation if their count reaches zero.
3.  **Potential for Cycles and Manual Management:**
    *   **Cycles:** The primary challenge is circular references. If object A references object B, and object B references object A, their reference counts will never drop to zero even if no external references to A or B exist.
        *   Common sources of cycles mentioned in `lisp.md`:
            *   ` (push my-list my-list)`
            *   ` (elem-set my-list 0 my-list)`
            *   Binding an environment to a symbol within itself or a child environment.
    *   **User Responsibility:** Developers must be mindful of these potential cycles and break them manually if necessary (e.g., by setting one of the circular pointers to `:nil` before the objects go out of scope).
4.  **Stack vs. Heap for Lisp Execution:**
    *   **Lisp Function Calls:** When a Lisp function is called, a new environment (an `hmap` VP object) is typically created for its local bindings. This environment is pushed onto an environment chain. These environments are heap-allocated VP objects.
    *   **VP Stack:** The underlying VP execution uses a traditional stack (`:rsp`) for its own function calls, local VP variables (if not register-allocated), and saving registers. CScript variables defined with `def-vars` are allocated on this VP stack.
    *   **Lisp "Stack Frames":** The chain of Lisp environments acts like a logical call stack for symbol lookup.
    *   **Lisp Objects:** All Lisp data objects (`num`, `str`, `list`, etc.) are heap-allocated VP objects managed by `sys_mem`.

---

### Level 5: Boot Image and Compiled Function Objects

1.  **Compiled Functions as VP Objects:**
    *   Each compiled VP function (defined by `def-func` or `def-method`) is itself a structured block of memory, as detailed in `vp_functions.md` and `lib/asm/func.inc`. This block contains the machine code, a header with metadata (offsets to string pool, link table), the local string pool, and the external function path/link table.
    *   These function "objects" are not `obj` class instances in the typical sense (they don't have a `vtable` and `count` field at their very start for direct `obj :ref` calls), but they are self-contained units of code and data.
2.  **Boot Image (`lib/boot/image.inc`):**
    *   The `boot-image` process gathers all necessary compiled VP functions (including transitive dependencies).
    *   It lays them out contiguously in memory to form a single binary file.
    *   Crucially, it **relocates** the entries in each function's link table. Instead of being PC-relative offsets to path strings (as in standalone compiled files), they become offsets from the link entry itself to the *pathname string within the header of the target function* as it exists *within the boot image*.
    *   The `sys/load/init` function (guaranteed to be first in the boot image) uses this relocated information at runtime to resolve function calls within the loaded image.
    *   The boot image itself is loaded into memory, and `sys/load/init` sets up the initial state for execution.

---

### Summary

ChrysaLisp's memory architecture is a carefully layered system designed for control and performance:

*   **Low-Level:** `sys_mem` uses `sys_heap` pool allocators (which use `mmap`) to manage raw memory.
*   **VP Object System:** The `obj` class provides a reference-counted object model with a defined lifecycle (`:create`, `:init`, `:ref`, `:deref`, `:deinit`, `:destroy`).
*   **Lisp Integration:** Lisp data types are direct instances of these VP classes. The Lisp interpreter itself is a `lisp` VP class instance. Memory management for Lisp objects relies on the underlying VP reference counting.
*   **Developer Responsibility:** The absence of a GC means developers must be vigilant about breaking potential circular references to prevent memory leaks.
*   **Execution Model:** Compiled functions are packaged into a boot image with relocated links, allowing the system to load and run.

This architecture provides a high degree of control over memory, suitable for systems where performance and predictability are paramount, but it places a greater burden on the developer for correct memory management compared to garbage-collected languages.