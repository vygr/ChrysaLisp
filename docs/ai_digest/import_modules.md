# ChrysaLisp's Module and Import System: Pragmatic, Performant, and Powerful

The module and import system in ChrysaLisp is a direct reflection of its core
philosophies: it is simple, avoids unnecessary complexity, and is built from the
system's own powerful, low-level primitives. It forgoes a traditional,
heavyweight package system in favor of a flexible, lexically-scoped model that
prioritizes the raw performance of `eval` while still providing robust
encapsulation.

The entire system is built upon three cooperating mechanisms: the `(import)`
function, the `(include)` macro, and a disciplined module pattern using
environment manipulation.

## The Core Mechanisms

### 1. `(import)`: The REPL-as-Loader

The fundamental primitive for loading code is `(import)`. It is best understood
as a **cached Read-Eval-Print Loop (REPL) on a file**.

*   **Mechanism:** At its heart, `(import)` uses the `(repl stream name)`
    function. It opens the specified file path to create a `stream` and uses the
    path as the `name` for identification and error reporting. It then evaluates
    the contents of the file in the context of a specified environment.

*   **Cached Execution:** A key feature is its "idempotent" nature within an
    environment's lineage. Before evaluating a file, `(import)` checks if that
    exact file path has already been imported into the current environment or
    any of its parents. This is achieved by searching a list of included files
    (e.g., `*file_includes*` as seen in `apps/pcb/router.inc`). If the path is
    found, `(import)` does nothing. This prevents redundant code loading and
    evaluation, acting as a highly effective and simple caching mechanism.

*   **Targeted Environments:** `(import)` takes an optional environment
    argument. If omitted, it defaults to the current environment. This
    flexibility is crucial, as it allows `import` to be used for different
    purposes, from loading application libraries to populating the compile-time
    environment.

### 2. `(include)`: A Compile-Time Specialization

The `(include)` form is not a new primitive but a macro that provides a
specialized use case for `(import)`. It is analogous to the `#include` directive
in C.

*   **Context:** It is used exclusively within a `(within-compile-env)` block,
    as seen throughout the `class/**/*.vp` files.

*   **Target:** It is a convenience wrapper that calls `(import)` with the
    `*compile_env*` as its target environment.

*   **Function:** Its sole purpose is to populate the compilation environment
    with the definitions required by the VP assembler, such as class structures
    (`def-class`), method declarations (`dec-method`), and assembler macros. It
    ensures that the build system has the necessary information to translate
    Lisp-like assembler into VP machine code.

### 3. The Module Pattern: Encapsulation Through Scoping

ChrysaLisp provides a simple yet powerful module pattern for encapsulation,
which is used in files like `lib/collections/collections.inc` and
`apps/template/actions.inc`. It doesn't rely on a formal package system but on
direct manipulation of the lexical environment.

The pattern consists of three steps:

1.  **`(env-push)`**: A new, temporary `hmap` is created, with its parent set to
    the current environment. This establishes a private, temporary scope for the
    module's internal definitions.

2.  **`(export-symbols)` / `(export-classes)`**: The module explicitly defines
    which of its symbols should be visible to the outside world. The `export`
    function then programmatically copies the bindings for these symbols from
    the temporary child environment up into its parent.

3.  **`(env-pop)`**: The temporary environment, containing all private helper
    functions and variables, is completely discarded.

This disciplined "sandbox and export" approach achieves robust encapsulation,
preventing internal implementation details from polluting the parent scope,
without adding any complexity to the core `eval` mechanism.

## The System-Wide Environment Architecture

These mechanisms operate within a clear, hierarchical environment structure that is fundamental to the OS.

*   **The Environment Tree:** Every task in ChrysaLisp operates within its own
    branch of a global environment tree.

    * **`*root_env*`**: At the top is a single, shared `*root_env*`. All tasks
        inherit from this root. It contains the most fundamental primitives
        defined in `class/lisp/root.inc`.

    * **Task Branches**: When a new task is created, it gets its own
        `lisp_environment`, a child of the `*root_env*`. This ensures that
        bindings created by one task do not interfere with another, while still
        providing access to the shared global functions.

    * **Automatic Imports**: Every task automatically imports
        `class/lisp/task.inc` into its local environment, guaranteeing that
        essential concurrency primitives like `(task-sleep)` are always
        available.

*   **System Bootstrapping via Root Injection:** The GUI service is a prime
    example of this architecture in action. Upon initialization, it **imports
    the GUI class libraries directly into the `*root_env*`**. This makes the GUI
    framework a system-wide resource, instantly available to any application
    that subsequently runs on that node, without each application needing to
    import it individually.

## Future Extensions: The `(import-from)` Pattern

The existing `(import-from)` function demonstrates the inherent extensibility of
this system. It uses the "sandbox and transfer" model to provide more granular
control over imports. This pattern can be easily extended to create a more
sophisticated module system.

The two-phase process is key:

1.  **Sandbox:** Evaluate the module file into a new, temporary environment.

2.  **Transfer:** Programmatically copy bindings from the temporary environment
    to the current one, applying transformations along the way.

This allows for powerful extensions without any changes to the core interpreter:

#### Adding Prefixing

To manage potential name collisions, an `(import-from)` macro could easily
support a `:prefix` option.

**Usage:**

```vdu
(import-from "lib/math/vector.inc"
  :symbols '(vec-add vec-sub)
  :prefix "vec:")
```

This would result in `vec:vec-add` and `vec:vec-sub` being defined in the
current environment by constructing new symbols during the transfer phase.

#### Adding Renaming (Aliasing)

For convenience, a `:rename` option could allow aliasing.

**Usage:**

```vdu
(import-from "lib/math/vector.inc"
  :rename '((vec-add v+) (vec-sub v-)))
```

During the transfer phase, the function would look up the value of `vec-add` in
the temporary environment and bind it to the symbol `v+` in the current
environment.

## Conclusion

ChrysaLisp's module system is an example of its design philosophies. By
leveraging a cached, REPL-based file loader and direct manipulation of its
performant `hmap` environments, it provides powerful features like compile-time
inclusion, runtime encapsulation, and system-wide service injection. It is a
system that "knows itself," using its own Lisp primitives to build the very
tools it needs for organization and extension, resulting in a lean, fast, and
remarkably flexible architecture.