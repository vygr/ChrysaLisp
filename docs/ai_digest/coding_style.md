# ChrisaLisp coding guidelines

To write truly effective ChrysaLisp code is to understand its underlying
philosophies. This document explores the core idioms and patterns that flow from
its first-principles design. It is a guide not just to syntax, but to a way of
thinking, showing how aligning with the system's architecture leads naturally to
elegant and performant solutions.

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
(defun process-data (data-list)
    (defq result (list))                  ; Form 1
    (setq *last-processed-id* (get-id data-list)) ; Form 2
    (setq *item-count* (+ *item-count* (length data-list))) ; Form 3
    ; ...
)
```

**Idiomatic Fused Style (Single Interpreter Form):**

```vdu
(defun process-data (data-list)
    ;; A single 'defq' handles all related local state logic.
    ;; The pairs are evaluated sequentially, allowing later bindings to use earlier ones.
    (defq result          (list)                          ; New binding
        *last-processed-id* (get-id data-list)              ; Mutates existing local
        *item-count*    (+ *item-count* (length data-list)))  ; Mutates existing local
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
    (defq bounds (. my-view :get_bounds))
    (defq x (first bounds) y (second bounds))

    ;; Idiomatic ChrysaLisp: Direct, clean, and avoids the temporary.
    (bind '(x y w h) (. my-view :get_bounds))
    ```

*   **De-structuring Nested Data**

    `bind` excels at cleanly unpacking complex, nested data structures. The
    pattern can be a nested list that mirrors the shape of the source data.

    ```vdu
    (defq data '(("Player1" (100 50)) (:score 5000)))
    
    ;; Clumsy and hard to read with defq and accessors.
    (defq player-name (first (first data)))
    (defq player-pos  (second (first data)))
    (defq score       (second (second data)))

    ;; Clean, declarative, and efficient with a single bind.
    (bind '(((player-name (x y)) (_ score))) data)
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
(defq matches (matches line "(\\w+): v(\\d) (\\S+) (\\S+)"))
;; matches -> '(("INFO: v2 2024-05-15 data.zip" "INFO" "2" "2024-05-15" "data.zip"))

;; We only want the version
;; &ignore is perfect for this.
(bind '((_ _ version &ignore)) (first matches))

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

    - It performs an `hmap:search`, starting with the `<object>` and **searching
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
    (defun populate-from-data (widget data-list)
      ;; For each [prop-name prop-value] pair in the list...
      (each (lambda ((prop-name prop-value))
              ;; 'prop-name' is a variable holding a symbol.
              ;; It is evaluated to get the key before 'def' is called.
              (def widget prop-name prop-value))
        data-list))

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
