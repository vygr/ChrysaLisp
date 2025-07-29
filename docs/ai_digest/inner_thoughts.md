# The ChrysaLisp REPL

"Are you trying to tell me we are living INSIDE a compiler ???"

This is a sophisticated piece of engineering and understanding its nuances is
fundamental to understanding the entire system's performance and design
philosophy. This is a detailed document explaining the REPL pipeline and the
user-level macros that control it.

## The ChrysaLisp REPL: A JIT Compiler in Disguise

In ChrysaLisp, the REPL (Read-Eval-Print Loop) is not merely an interactive
shell; it is the **core execution engine for all Lisp code**. Whether loading a
file from disk with `(import ...)` or processing a line of code in the terminal,
every form passes through the same multi-pass pipeline. This pipeline is
designed to push as much computational work as possible into "compile time" (the
pre-evaluation phases), making the final runtime evaluation incredibly fast and
efficient.

It's most accurate to think of the REPL not as a simple loop, but as a **Read ->
Compile -> Link -> Evaluate -> Print** pipeline.

### The REPL Pipeline: A Step-by-Step Breakdown

#### Phase 1: Read (`lisp :read`)

This phase parses a stream of characters into an Abstract Syntax Tree (AST),
which in ChrysaLisp is a nested structure of `list` vector objects.

*   **Mechanism:** True to the system's philosophy, the parser is **iterative,
    not recursive**. It uses the task's `lisp_stack` (a heap-allocated `list`)
    as an explicit work-stack to manage nested parentheses, ensuring it can
    parse arbitrarily complex code with a small, constant machine stack
    footprint.

*   **Process:** It tokenizes the input stream, dispatching to specialized
    readers for different data types: `lisp :read_num` for numbers,
    `lisp :read_str` for strings, and `lisp :read_sym` for symbols, which are
    then interned. The result is a raw, nested `list` structure representing the
    code.

---

### The "Compile Time" Passes

Before the AST is executed, it passes through two powerful transformation and
optimization stages.

#### Phase 2: Macro Expansion (`lisp :repl_expand`)

This is the first compilation pass. It performs source-to-source transformation.

*   **Purpose:** To expand syntactic abstractions (macros) into more primitive
    forms. This allows the core language to remain small while providing rich,
    high-level syntax to the developer.

*   **Mechanism:** The expander performs a **top-down, single-pass, depth-first
    traversal** of the AST. When it encounters a list whose head is a symbol
    bound to a macro, it executes the macro's definition and **replaces the
    original macro form in the AST with the code returned by the macro**.

*   **Implication: Zero-Cost Abstractions:** The work a macro does is performed
    *only once*, during this phase. The runtime evaluator never sees the macro
    itself, only its final, expanded output.

    * **Example:** The familiar `(defun name (args) ...)` is not a primitive. It
        is a macro that expands into the more fundamental
        `(defq name (lambda (args) ...))`. This provides a convenient syntax
        with absolutely no runtime performance penalty.

#### Phase 3: Pre-Binding (`lisp :repl_bind`)

This is the second compilation pass, acting as an in-memory linker. It is the
key to ChrysaLisp's O(1) lookup performance.

*   **Purpose:** To resolve function symbols to their direct memory addresses
    before runtime.

*   **Mechanism:** After macro-expansion is complete, `repl_bind` traverses the
    AST. When it finds a symbol in a function-call position (the head of a
    list), it searches the current environment. If it finds a corresponding
    `func` object (a compiled VP function) or a Lisp `lambda`, it **replaces the
    symbol in the AST with a direct pointer to that function object.**

*   **Implication: Elimination of Runtime Lookups:** This is a profound
    optimization. A call like `(+ 1 2)` is transformed into a list where the
    head is no longer the *symbol* `+` but the actual memory address of the
    addition function. During evaluation, the system performs a direct jump to
    this address, completely bypassing any hash map lookup.

---

### Phase 4: Evaluation (`lisp :repl_eval`)

This is the "runtime" phase. Thanks to the preceding passes, its job is
dramatically simplified.

*   **Atoms:** Atoms (numbers, strings) evaluate to themselves.

*   **Symbols:** A symbol that was not pre-bound (e.g., a variable or a forward
    reference to a function) is looked up in the environment `hmap`. This lookup
    is O(1) due to the self-repairing `str_hashslot` cache.

*   **Lists (Forms):**

    1. The evaluator examines the head of the list.

    2. If the head is a pre-bound `func` object, it inspects its `func_type`. If
        it's a **special form** (`args_raw`), it calls the function with the
        unevaluated arguments. If it's a **regular function** (`args_eval`), it
        first recursively calls `repl_eval` on all arguments and then calls the
        function with the results.

    3. The call itself is a direct jump to the machine code address stored in
        the `func` object.

    4. If the head is a Lisp `lambda`, the evaluator creates a new lexical
        scope, binds the arguments, and evaluates the `lambda`'s body.

---

### The Developer's Toolbox: Controlling the Pipeline

ChrysaLisp provides powerful macros that give the developer explicit control
over this compilation pipeline.

#### `exec` - The Runtime REPL

```vdu
(defun exec (_) (eval (macrobind _)))
```

*   **What it does:** `exec` evaluates code that invokes the *entire* REPL
    pipeline at **runtime**. It is the tool for evaluating dynamically generated
    code. The form `(exec my-code)` becomes
    `(eval (prebind (macroexpand my-code)))` at runtime.

#### `const` - The Compile-Time Evaluator

```vdu
(defmacro const (_) (exec _))
```

*   **What it does:** The `(exec _)` call is in the body of the `const` macro,
    not its expansion template. This forces the full REPL pipeline to run on `_`
    **during the macro-expansion phase**. The *result* of this compile-time
    evaluation is what gets embedded in the AST.

*   **Why it's important:** It's the primary mechanism for constant folding.
    `(const (+ 10 20))` is replaced by the `num` object `30` in the final code,
    saving the runtime calculation.

*   **The Nuance:** `const` does not make its result "static" or immune to
    further evaluation. If `(const (list '+ 1 2))` is used, the result embedded
    in the AST is the literal *list* `(+ 1 2)`. The runtime evaluator will still
    see this list and evaluate it as a function call. `const` simply performs
    the list *construction* at compile time.

#### The `static-q*` Family: Compile-Time Quoting

These macros are essential for building complex, static ASTs and are the key to
the GUI's DSL.

*   **The Problem:** The standard quote (`'`) halts the macro-expander.
    `'(my-macro)` results in a list containing the symbol `my-macro`, not its
    expansion.

*   **`static-q`:** Solves this by running `macrobind` on its argument *first*,
    and then wrapping the fully expanded result in a `(quote ...)`. This creates
    a truly static data structure with all internal macros pre-expanded.

*   **`static-qq`:** The templating version. It does the same as `static-q` but
    creates a `(quasi-quote ...)` form, allowing for unquoting (`~`). This is
    the workhorse for most code-generating macros.

    *   **Example: The `case` Macro**

        ```vdu
        (defmacro case (key &rest body)
          ; ... complex logic to transform body into lists of keys and values ...
          (static-qq (eval (elem-get ',vals (ifn (find ,key ',keys) -2)))))
        ```

        The `case` macro does significant work at compile time to transform its
        input into two lists: `keys` and `vals`. It then uses `static-qq` to
        build a code template. The `,key` is a placeholder for the runtime key,
        but `',vals` and `',keys` are the *compile-time computed lists* embedded
        directly into the final AST. This is a perfect example of compile-time
        code generation.

*   **`static-qqp` (Prebind-Only): The Expert's Optimization** This is the most
    subtle and powerful of the family. It creates a `(quasi-quote ...)` form but
    **skips the `macroexpand` step**, only performing `prebind`.

    * **Why it's necessary: The `ui-*` Macros.** The GUI macros are a deeply
        nested DSL where parent widgets must expand first to provide a lexical
        context (the `_ui` variable) for their children.

    * The standard macro-expander works top-down. It expands `(ui-window ...)`
        which produces a form containing an unexpanded `(ui-flow ...)`. The
        expander then naturally descends into that and expands `ui-flow`.

    * If `ui-window` used `static-qq`, it would try to fully expand `ui-flow` in
        isolation, *before* the `_ui` context from its parent was created,
        causing the expansion to fail.

    * **`static-qqp` solves this.** It trusts the main, top-down expander to
        handle the recursive expansion correctly. Its only job is to perform the
        final `prebind` linking pass on the already-expanded AST before quoting
        it. This makes the compilation of the GUI DSL both possible and highly
        efficient, avoiding redundant expansion passes.

## Conclusion

The ChrysaLisp REPL is a masterclass in pragmatic system design, turning what is
an interactive tool in other languages into a powerful, multi-pass,
JIT-compiling engine that underpins the entire system's performance.