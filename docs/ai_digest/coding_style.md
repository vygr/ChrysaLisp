# ChrysaLisp coding guidelines

To write truly effective ChrysaLisp code is to understand its underlying
philosophies. This document explores the core idioms and patterns that flow from
its first-principles design. It is a guide not just to syntax, but to a way of
thinking, showing how aligning with the system's architecture leads naturally to
elegant and performant solutions.

## General Style and Naming Conventions

While the ChrysaLisp compiler is flexible and prioritizes programmer freedom, a
set of strong conventions has emerged to promote readability, maintainability,
and the prevention of common errors. Adhering to this style makes your code
instantly understandable to other ChrysaLisp developers.

### Naming Conventions: Communicating Intent

A simple set of prefix and separator conventions allows a developer to
understand the nature of a symbol at a glance.

*   **`function-names` and `macro-names` use hyphens `-`**.

    *   This clearly identifies callable code.

    *   Examples: `(defun my-helper-function ...)` `(defmacro ui-window ...)`

*   **`local_variables` and `:object_properties` use underscores `_`**.

    * This identifies data containers and distinguishes them from functions.
      Keywords (symbols starting with a colon) are the idiomatic choice for
      property keys.

    * Examples: `(defq my_local_var 0)` `(def this :ink_color +argb_red)`

*   **`+constants` are prefixed by a plus sign `+`**.

    * This signals a bind-time constant whose value will be baked into the
      compiled code, such as those created by `def-struct`.

    * Examples: `+max_retries`, `+argb_black`, `+my_msg_type`.

*   **`*globals*` are surrounded by asterisks `*` ("earmuffs")**.

    * This warns that the variable is a dynamic global or special variable that
        might be rebound and have wide-ranging side effects.

    * Examples: `*root_env*`, `*debug_mode*`.

#### The Shadowing Guideline: Avoid Redefining Functions Locally

ChrysaLisp allows you to define a local variable that has the same name as a global function or macro. The system assumes you have a good reason for doing this.

```vdu
;; LEGAL, BUT EXTREMELY BAD PRACTICE
(defun my-bad-function ()
  ;; 'list' is now a local variable, shadowing the global function.
  (defq list '(a b c))
  
  ;; This will now FAIL because 'list' is no longer a function.
  (print (list 1 2 3)))
```

**Guideline:** **Never use a local variable name that shadows an existing
function or macro.**

This is the most critical style rule for preventing subtle and confusing bugs.
The naming convention is your primary tool for avoiding this mistake. By naming
your local variables with underscores (`my_list`) and your functions with
hyphens (`(list ...)`), you will never accidentally conflict.

Tooling, such as the `Editor` app with syntax highlighting, also provides a
strong visual cue when this error occurs.

## Efficient State Management with `defq` and `setq`

A disciplined approach to state management is critical for writing
high-performance, idiomatic ChrysaLisp code. The core goal of this discipline is
to **minimize the number of forms the interpreter's evaluation loop must
process.** This is achieved by understanding the precise roles of `defq` and
`setq` and leveraging their capabilities to group related state operations.

### Core Principle: Local Set-or-Define vs. Global-Aware Mutate-Only

The fundamental distinction between `defq` and `setq` lies in how they interact
with lexical scopes. This directly impacts their performance and intended use.

*   **`(defq <symbol> <value> ...)`**: This is the **Local Set-or-Define
    Operator** and should be considered the default tool for all local variable
    manipulation.

    * For each symbol, `defq` performs a **local-only** search within the
        *current* environment (`hmap`).

    * If a binding for the symbol already exists in the current scope, its value
        is **mutated in-place**.

    * If no local binding exists, a **new binding is created** in the current
        environment.

    * Crucially, `defq` **will never search or modify a parent environment**.
        This makes it predictably fast and prevents accidental side effects on
        outer scopes.

*   **`(setq <symbol> <value> ...)`**: This is the **Global-Aware, Mutate-Only
    Operator**. It is a specific tool for intentionally modifying variables in
    outer scopes.

    * For each symbol, `setq` performs a search up the **entire environment
        chain**, starting from the current scope.

    * It **mutates** the first binding it finds, wherever it may be in the
        chain.

    * If no binding is found anywhere, it raises a `symbol_not_bound` **error**.
        `setq` can never create a new binding.

### The Performance Idiom: Fusing Operations to Minimize Forms

The primary stylistic and performance guideline is to **group logically related
state changes into the longest possible `defq` or `setq` statement.** A single
multi-binding form is more efficient than multiple single-binding forms because
it reduces the work for the interpreter.

The key optimization idiom is to **fuse local mutations into a necessary
`defq`**.

**1. For Semantic Clarity, `setq` is Preferred for Standalone Mutations**

When the primary goal is readability and clearly communicating intent, use
`defq` for creation and `setq` for mutation. `setq` unambiguously signals a
state change and, when operating on a known local variable, is just as fast as
`defq`.

```vdu
(defq counter 0)
(while (< counter 10)
    ;; Clear and idiomatic. 'setq' signals a mutation.
    ;; This is a fast, local operation.
    (setq counter (inc counter)))
```

**2. For Maximum Performance, Fuse Operations with `defq`**

If you are already introducing a new variable with `defq`, you should "fuse" any
nearby local `setq` operations into that single `defq` form to reduce the total
number of forms the interpreter must handle.

**Verbose Style (Multiple Interpreter Forms):**

```vdu
(defun process-data (data_list)
    (defq result (list))                  ; Form 1
    (setq *last-processed-id* (get-id data_list)) ; Form 2
    (setq *item-count* (+ *item-count* (length data_list))) ; Form 3
    ; ...
)
```

**Idiomatic Fused Style (Single Interpreter Form):**

```vdu
(defun process-data (data_list)
    ;; A single 'defq' handles all related local state logic.
    ;; The pairs are evaluated sequentially, allowing later bindings to use earlier ones.
    (defq result          (list)                          ; New binding
        *last-processed-id* (get-id data_list)              ; Mutates existing local
        *item-count*    (+ *item-count* (length data_list)))  ; Mutates existing local
    ; ...
)
```

This second style is preferred for its superior performance and conciseness.

**Conclusion**

*   **Use `defq` for all initial variable bindings.** Group related
    initializations into a single `defq` form.

*   **For simple, standalone mutations of a known local variable, `setq` is
    stylistically appropriate** as it clearly signals intent. There is no
    performance penalty in this specific case.

*   **When creating a new local variable with `defq`, seize the opportunity to
    fuse subsequent local `setq`s into that same `defq` form.** This is a key
    optimization pattern that reduces strain on the interpreter.

*   **Reserve `setq` for its two main use cases:**

    1. Simple, standalone local mutations where semantic clarity is desired.

    2. Intentional modification of variables in outer scopes, with full
        awareness of the associated environment search.

## The `bind` Operator: De-structuring and Pattern Matching

Beyond the simple assignment of `defq`, ChrysaLisp provides the powerful `bind`
operator. While `defq` is ideal for binding single values to symbols, `bind`
excels at de-structuring sequences, allowing a programmer to extract multiple
values from a list or array and bind them to local variables in a single,
declarative operation.

Mastering `bind` is essential for writing concise, readable, and efficient
ChrysaLisp code, especially when interacting with functions that return multiple
values or when processing nested data structures.

### Basic Syntax

```vdu
(bind '<pattern-list> <source-sequence>)
```

*   **`<pattern-list>`**: A *quoted literal list* that describes the "shape" of
    the data you expect. It contains symbols that will become local variables.
    Quoting the pattern is crucial; it tells the interpreter to treat the list
    as data to be matched, not as code to be executed.

*   **`<source-sequence>`**: The sequence containing the data to be
    de-structured.

The symbols introduced in the pattern list are created as new bindings in the
current lexical scope and benefit from the same O(1) lookup performance as
variables created with `defq`.

### Core Use Cases

*   **Handling Multiple Return Values**

    Many system functions return multiple results as a list. `bind` is the ideal
    way to receive these values without creating unnecessary temporary
    variables.

    ```vdu
    ;; Less Idiomatic: Creates a temporary 'bounds' variable.
    (defq bounds (. my_view :get_bounds))
    (defq x (first bounds) y (second bounds))

    ;; Idiomatic ChrysaLisp: Direct, clean, and avoids the temporary.
    (bind '(x y w h) (. my_view :get_bounds))
    ```

*   **De-structuring Nested Data**

    `bind` excels at cleanly unpacking complex, nested data structures. The
    pattern can be a nested list that mirrors the shape of the source data.

    ```vdu
    (defq data '(("Player1" (100 50)) (:score 5000)))
    
    ;; Clumsy and hard to read with defq and accessors.
    (defq player_name (first (first data)))
    (defq player_pos  (second (first data)))
    (defq score       (second (second data)))

    ;; Clean, declarative, and efficient with a single bind.
    (bind '(((player_name (x y)) (_ score))) data)
    ```

### Advanced Pattern Matching Keywords

`bind` supports the same powerful keywords as a function's lambda list, allowing
for sophisticated pattern matching.

*   **`_` (The "Ignore" Convention)** By convention, the underscore symbol is
    used in a pattern to consume a positional value that will not be used. It is
    a signal to the reader, but it **still performs a binding** to the `_`
    symbol. It is best used for ignoring single, interspersed values.

*   **`&rest <symbol>`** This keyword collects **all remaining** elements of the
    source sequence into a single new list and binds that list to the specified
    `<symbol>`.

*   **`&ignore` (The Terminator Keyword)** This is the most efficient way to
    ignore trailing values. When `&ignore` is encountered in a pattern, the
    `bind` operation **immediately terminates**. No further symbols in the
    pattern are processed, and no more values from the source sequence are
    consumed. This is the ideal tool when you only need the first *N* elements
    of a list.

**Example: Parsing a Regexp Match**

The `Regexp` library provides a perfect use case. The `matches` function returns
a list where each element is a sub-list containing the full match followed by
its capture groups.

```vdu
(defq line "INFO: v2 2024-05-15 data.zip")
(defq match (matches line "(\\w+): v(\\d) (\\S+) (\\S+)"))
;; match -> '(("INFO: v2 2024-05-15 data.zip" "INFO" "2" "2024-05-15" "data.zip"))

;; We only want the version
;; &ignore is perfect for this.
(bind '((_ _ version &ignore)) (first match))

;; ONLY 'version' is bound. The bind operation stopped immediately
;; after processing the version number, making this highly efficient.
(print version) ; -> "2"
```

**Conclusion**

* Use defq for simple, sequential creation and mutation of local variables.

* Use bind whenever you need to unpack values from a sequence. It makes code
  more declarative, less error-prone, and often more performant by avoiding
  temporary variables and unnecessary processing.

* Use &ignore inside a bind to efficiently discard all trailing elements of a
  sequence once you have extracted the values you need.

## Manipulating Object Properties: `def` and `set`

Explicitly scoped `defq` and `setq` !

In ChrysaLisp, object properties (instance variables) are stored as key-value
pairs in an `hmap`. The `def` and `set` operators are the primary tools for
manipulating this data. Their behavior is a direct parallel to `defq` and
`setq`, but they operate on explicit object `hmap`s rather than the implicit
lexical scope.

A crucial concept to understand is that **GUI widget trees ARE Lisp
environments.** The hmap chain of properties from a widget to its container
forms a runtime property inheritance hierarchy, which functions exactly like the
lexical environment chain that `setq` traverses.

The `defq` and `setq` forms COULD be provided as macros, but they are
implemented as VP functions for performance reasons. ! eg.

```vdu
(defmacro defq (&rest _)
    (reduce! (lambda (out (s v)) (push out (list 'quote s) v))
        (partition _ 2) (list `def `(env))))
```

### Operator Roles: Local Override vs. Inherited Mutation

*   **`(def <object> <key-expression> <value>)`**: The **Local Property
    Operator**.

    - It operates **only** on the `hmap` of the specified `<object>`.

    - It creates a new property or mutates an existing one directly on that
      object instance.

    - This is the standard way to **set or override a property for a specific
      widget**, making it independent of its parents. `def` will never search up
      the parent chain.

*   **`(set <object> <key-expression> <value>)`**: The **Inherited Property
    Mutator**.

    - It performs an `hmap :search`, starting with the `<object>` and **searching
      up the parent chain** until it finds the property `<key>`.

    - It then **mutates the value of that property on whichever ancestor it was
      found**.

    - This is a powerful tool for changing a shared property on a container,
      thereby affecting all children that inherit it. It should be used
      deliberately. If the property is not found anywhere in the inheritance
      chain, `set` will raise an error.

### Static vs. Dynamic Property Keys

A key feature of `def` and `set` is that their `<key-expression>` argument is
**evaluated at runtime**. This allows for powerful dynamic programming patterns.

*   **Static Properties (Most Common)**: When you know the name of the property
    at compile time, you should use a **quoted symbol**, typically a keyword
    (e.g., `:color`, `:text`). Keywords evaluate to themselves and are the
    idiomatic choice for property keys.

    ```vdu
    (defclass My-Button () (Button)
      (defmethod :on-click ()
        ;; Sets the :text and :color properties on the 'this' object.
        (def this :text "Clicked!" :color +red+)))
    ```

*   **Dynamic Properties (Advanced)**: When the property name is determined at
    runtime, you can use an expression that evaluates to a symbol. This is
    essential for writing generic, data-driven code.

    ```vdu
    (defun populate-from-data (widget data_list)
      ;; For each [prop-name prop-value] pair in the list...
      (each (lambda ((prop-name prop-value))
              ;; 'prop-name' is a variable holding a symbol.
              ;; It is evaluated to get the key before 'def' is called.
              (def widget prop-name prop-value))
        data_list))

    (defq my-widget (View))
    (defq config-data '((:color +red+) (:width 200)))
    (populate-from-data my-widget config-data)
    ```

**Conclusion**

*   **Always use `def` to set properties on an object instance.** This is the
    safe, expected, and most common operation. It ensures you are only modifying
    the state of the object you are explicitly targeting.

*   **Only use `set` when your explicit intent is to find and mutate a property
    higher up in the inheritance chain**, thereby affecting the object and
    potentially its siblings that also inherit that property.

This distinction is key to harnessing the power of ChrysaLisp's unified property
inheritance and lexical scoping model without introducing subtle bugs from
unintended side effects.

## Iteration over Recursion: A Core Discipline

In ChrysaLisp, there is a strong and architecturally-enforced preference for
**iteration over recursion**. Adhering to this discipline is not merely a
stylistic choice; it is fundamental to creating robust, high-performance, and
massively concurrent applications. This design choice creates a virtuous circle
where cooperative multitasking, efficient memory use, and fast symbol lookup all
reinforce each other.

### The "Why": Small Stacks and O(1) Caching

The preference for iteration stems from two core architectural principles:

*   **Cooperative Tasks and Small Stacks:** ChrysaLisp is built for massive
    concurrency, where tens of thousands of tasks can run on a single core. To
    make this possible, each task is given a small, fixed-size machine stack
    (typically 8KB). This makes tasks extremely memory-efficient, but it also
    makes deep recursion on the machine stack physically impossible—it would
    quickly lead to a stack overflow.

*   **Stable Scopes for O(1) Lookups:** A recursive style creates a deep chain
    of nested lexical environments (`hmap`s). This leads to frequent shadowing
    and un-shadowing of variables as the call stack grows and shrinks, causing
    constant invalidation and repair of the `str_hashslot` cache. An iterative
    style typically operates within a single, flat lexical scope. This keeps the
    `str_hashslot` cache stable, maximizing its effectiveness and ensuring
    consistent O(1) lookup performance.

### The Idiomatic Pattern: The Explicit Work Stack

Since recursion on the machine stack is not an option, the canonical ChrysaLisp
pattern for handling nested data structures is **iteration using a
heap-allocated `list` as an explicit work stack.**

This pattern is used pervasively throughout the system, from the Lisp parser to
the GUI compositor. It guarantees that a function's machine stack usage remains
minimal and constant, regardless of the complexity or depth of the data it is
processing.

**Example: The `flatten` function**

The `flatten` function is a perfect illustration of this idiom. It can process a
list of any depth without ever growing the machine stack.

```vdu
(defun flatten (lst)
	; (flatten list) -> list
	(defq out (list) stack (list lst 0))
    ; 'stack' is the explicit work list on the heap.
	
	(while (defq idx (pop stack) lst (pop stack))
		(some! (lambda (%0)
			(cond
				((list? %0) 
                    ; Found a sub-list. Don't recurse. Instead, push the
                    ; parent list and the next index back onto our work stack
                    ; to be processed later. Then, push the new sub-list.
                    (push stack lst (inc (!)) %0 0) 
                    :nil) ; Stop processing this list and restart the 'while' loop.
				((push out %0)))) ; It's an atom, collect it in the output.
            (list lst) :t idx)) 
    out)
```

By following this discipline, programmers work *with* the ChrysaLisp
architecture, not against it, resulting in code that is inherently stable,
scalable, and performant.

## Modules and Bind-Time Optimization

ChrysaLisp's evaluation model includes a powerful **pre-binding** stage that occurs before final execution. This architectural feature is central to the system's performance and enables a simple yet robust module system that combines information hiding with extreme efficiency. By understanding this process, developers can write code that is both highly readable and maximally performant.

### The `repl_bind` Pass: An Ahead-of-Time Optimization

Before a Lisp form is executed, it passes through the `lisp :repl_bind` stage.
This pass walks the macro-expanded code tree and attempts to resolve a
`function` or `constant` symbol to its definition in the current environment.

*   If a symbol's definition is found, the symbol in the code is **replaced with
    a direct pointer** to its binding (e.g., a function address or a constant's
    value).

*   This effectively means that at runtime, no `hmap` lookup is required. The
    call or access is a direct memory operation, which is the fastest possible
    way to execute.

This mechanism leads to a critical programming discipline: **Define functions
and variables before they are used.** Adhering to this "no forward references"
rule is a performance contract with the compiler. The `forward` command is
provided to help developers identify and fix violations of this guideline.

### Information Hiding: The `def`/`use`/`export` Pattern

The "no forward references" guide is leveraged to create a simple and effective
module system, used pervasively throughout the standard libraries.

1.  **Define Internals First:** All helper functions and private constants are
    defined at the top of a module file. Because they have not yet been
    exported, they are effectively "private" to the module's scope during
    compilation.

2.  **Use Internals to Build the Public API:** Public functions are defined
    next, freely using the "private" helpers. During the compilation of this
    file, `repl_bind` resolves all these internal calls to fast, direct
    pointers.

3.  **Export the Public Interface:** At the very end of the file, an
    `(export-symbols '(...))` form is used. This function takes a list of
    symbols and adds them and their definitions to a shared environment
    (typically the parent), making them visible to other modules that `import`
    this file.

**Example:**

```vdu
;;; In my-module.inc

;; 1. Private helper. Only visible within this file.
(defun _helper (x) (* x 2))

;; 2. Public function that uses the helper.
(defun my-public-api (z)
  ;; This call is resolved to a direct pointer at bind-time.
  (+ (_helper z) 1))

;; 3. Export ONLY the public function. _helper remains private.
(export-symbols '(my-public-api))
```

This pattern provides strong encapsulation without needing complex `private`
keywords or namespace syntax.

### Bind-Time Constants for Maximum Speed

A key feature of the pre-binding process is its ability to perform constant
folding, replacing symbols with their literal values in the final compiled code.

*   **The `+` Prefix Convention:** Symbols prefixed with a `+` (e.g.,
    `+max_retries`) are, by convention, treated as bind-time constants. When
    `repl_bind` encounters such a symbol, it will **replace the symbol entirely
    with that value**. The symbol itself never appears in the final executable
    code.

*   **Structures and `getf`/`setf`:** This pattern is most powerfully used by
    the `def-struct` (for VP assembler) and `structure` (for Lisp) macros. These
    macros define a family of constants representing the memory offsets of
    fields within a data structure.

    ```vdu
    ;; Defines +my_msg_type as the literal integer 24, among other constants.
    (structure +my_msg 0 (netid reply_id) (uint type))

    ;; A programmer writes this clean, symbolic code:
    (setf my_msg +my_msg_type +ev_type_action)
    ```

    At compile time, the `setf` macro is expanded. It evaluates `+my_msg_type`
    to its literal value, `24`. The final code generated is equivalent to
    `(set-field my_msg 24 4 +ev_type_action)`, which becomes a single,
    highly-optimized memory write instruction. This allows developers to work
    with high-level, symbolic field names while the system guarantees C-level
    performance for data access.

## The Path to Performance: A Three-Stage Optimization Strategy

ChrysaLisp is designed to offer a spectrum of performance, allowing developers
to choose the right balance between rapid development and raw execution speed.
The system's philosophy is not one of premature optimization, but rather of
providing a clear, structured path for when performance becomes critical. The
core mantra is: **"First, make it work. Then, make it right. Then, if you must,
make it fast."**

This progression typically follows three distinct stages, moving from the
highest level of abstraction to the most direct hardware control.

### Stage 1: The Lisp Prototype — Correctness and Clarity First

Every new piece of functionality should begin as a pure ChrysaLisp function.

*   **Primary Goal:** Algorithm correctness, clarity, and rapid development.

*   **Environment:** Full access to the high-level Lisp environment, its
    libraries, and the REPL for interactive testing.

*   **Performance:** The code is handled by the hyper-fast Lisp interpreter. For
    many tasks, especially those that are not in a tight loop or are I/O-bound,
    this level of performance is more than sufficient.

**Guideline:** Always start here. Prove your logic is sound. Create a baseline
for functionality and, if needed, performance. If the Lisp function meets your
performance requirements, your work is done. There is no need to proceed
further.

**Example: A Simple `sum-of-squares` Function**

```vdu
;; Stage 1: A clear, readable Lisp function.
(defun sum-of-squares (number_list)
  (reduce! (# (+ %0 (* %1 %1))) number_list 0))
```

This version is easy to write, understand, and debug. For most applications, its
performance would be excellent.

### Stage 2: The C-Script VP Function — Robust Performance

If benchmarking (e.g., using the `cmd/test` app) reveals that the Lisp function
is a bottleneck, the next step is to translate it into a C-Script VP function.

*   **Primary Goal:** To gain significant performance by eliminating Lisp
    interpreter overhead, while still maintaining a high level of abstraction
    and safety.

*   **Environment:** C-Script uses a more C-like syntax within a `(def-func)`
    block. You declare local variables with `def-vars`, and the assembler
    handles mapping them to stack locations or registers. You are protected from
    many of the complexities of manual register allocation.

*   **Performance:** C-Script compiles down to highly efficient VP code. It is
    significantly faster than the interpreted Lisp version and is the right
    choice for the vast majority of performance-critical application code.

**Guideline:** Move to this stage to prove your algorithm's performance at a
lower level. This is the ideal environment to refine the logic with the
assurance that you are not introducing the subtle bugs that can arise from
manual register management.

**Example: `sum-of-squares` as a C-Script Function**

```vdu
(def-func 'my-sum-of-squares)

    (def-vars
        (ptr list_obj)
        (pptr iter iter_end)
        (long sum val))

    (entry {list_obj})

    (assign {0} {sum})
    (call 'array :get_both {list_obj} {_, iter, iter_end})
    (loop-while {iter /= iter_end})
        (call 'num :get_value {*iter} {_, val})
        (assign {val * val + sum} {sum})
        (assign {iter + +ptr_size} {iter})
    (loop-end)
    (call 'num :create {sum} {list_obj})

    (exit {list_obj})
    (return)

(def-func-end)
```

### Stage 3: The Optimized VP Method — Maximum Velocity

For the most demanding parts of the system—core class methods, kernel
primitives, or routines in an extreme performance-critical loop—the final
optimization stage is to write a pure VP assembly method.

*   **Primary Goal:** To achieve the smallest footprint and highest possible
    execution speed by controlling the hardware at the most direct level
    possible.

*   **Environment:** You manually allocate registers using `(vp-def ...)` and
    write code using direct register symbols (`:r0`, `:r1`, etc.). You are
    responsible for managing register lifetimes and avoiding conflicts. The
    stack may be used sparingly or, in many cases, avoided entirely for leaf
    functions.

*   **Performance:** This is "bare metal" on the Virtual Processor. There is no
    abstraction layer left. Code written at this level forms the hyper-optimized
    engine upon which the rest of the system is built.

**Guideline:** Only proceed to this stage when you have an absolutely correct
and benchmarked C-Script version, and you have determined that even greater
performance is required. This level of optimization requires careful thought but
yields the best results. The core kernel and class libraries are the ultimate
expression of this stage. The `canvas :fpoly` method is a prime example of a
complex algorithm implemented at this level for maximum performance.

**Example: `sum-of-squares` as an Optimized VP Method**

```vdu
;; Stage 3: Direct register manipulation for maximum speed.
(def-method 'my-class :sum_of_squares)

    (vp-def (this iter iter_end sum val) '(:r8 :r9 :r10 :r11 :r12))

    (entry 'my-class :sum_of_squares `(,this))

    (assign '(0) `(,sum))
    (class/array/get_both this iter iter_end)
    (loop-while `(,iter /= ,iter_end))
        (assign `((,iter 0)) `(,val))
        (assign `((,val num_value)) `(,val))
        (vp-mul-rr val val)
        (vp-add-rr val sum)
        (vp-add-cr +ptr_size iter)
    (loop-end)
    (call 'num :create `(,sum) `(,sum))

    (exit 'my-class :sum_of_squares `(,this ,sum))
    (vp-ret)

(def-func-end)
```

### The Unifying Philosophy

This three-stage process embodies ChrysaLisp's pragmatic approach to
engineering. It provides a smooth "performance gradient," allowing a developer
to move from a high-level, safe environment to direct, low-level control only
when and where it is needed. This structured path empowers you to write code
that is readable, maintainable, and, when necessary, exceptionally fast.