# The Rocinante Primitives: The Workhorses of ChrysaLisp

To the Lisp veteran, the ChrysaLisp standard library can seem deceptively
sparse. Where are `mapcar`, `mapc`, `dolist`, `remove-if`, `find-if`, and the
mighty `loop` macro? The profound insight of the ChrysaLisp design is that these
are not missing; they are simply emergent properties of a smaller, more
powerful, and vastly more performant set of foundational primitives.

These core primitives—`each!`, `map!`, `some!`, `reduce!`, and `filter!`—are the
**Rocinante Primitives**. Like Don Quixote's faithful steed, they are not
flashy, but they are the tireless, reliable workhorses that carry the entire
system. They are the engine of idiomatic ChrysaLisp. To truly master the
language—to wield its forms like nun-chucks with speed, precision, and grace—one
must first learn to ride Rocinante.

This document serves as a comprehensive guide to these primitives, explaining
not just *what* they do, but *why* their specific design is a direct consequence
of the system's core philosophies, and how they unlock a new level of
performance and expressiveness.

## The Architectural Foundation of the Primitives

The Rocinante primitives are not arbitrary functions; they are the architectural
principles of ChrysaLisp made manifest at the language level. Their design is a
direct reflection of the system's core tenets.

*   **Philosophy 1: "Well, Don't Do That Then!" (Problem Avoidance)**
    Traditional list processing often involves creating many temporary,
    intermediate lists. For example, `(mapcar ... (reverse ...))` creates a
    full, reversed copy of a list just to iterate over it once. ChrysaLisp's
    response is, "Well, don't do that then!" The Rocinante primitives have
    integrated slicing and reversal capabilities, allowing them to iterate
    backwards over a sub-sequence of the original data *without allocating any
    intermediate copies*. This fundamental avoidance of unnecessary allocation
    is a key source of the system's performance and predictability.

*   **Philosophy 2: "Be Formless, Shapeless, Like Water" (Adaptability)** The
    primitives are designed to be polymorphic and adaptable. The core iterators
    (`each!`, `map!`, `reduce!`) don't just work on lists; they work on a **list
    of heterogeneous sequences**. A single `map!` call can seamlessly process a
    `str`, an `array` of integers, and a `list` of objects in parallel, adapting
    its behavior to the data it is given. It takes the "shape" of its inputs.

*   **Philosophy 3: "Know Thyself" (Cooperative Internals)** These primitives
    are not black boxes. They are intelligent tools built with an intimate
    knowledge of the system they inhabit.

    * **The Collector Pattern:** The optional `out-list` parameter in `map!` and
      `filter!` allows them to act as "collectors," appending results to an
      existing list. This is a conscious design choice that enables the creation
      of complex, single-pass data processing pipelines that minimize memory
      churn, a critical feature for a reference-counted system that aims to
      eliminate GC pauses.

    * **Stack Safety:** The pervasive use of iteration over recursion,
      exemplified by patterns using `some!`, is enabled by the system's
      knowledge of its own cooperative, small-stack environment. The primitives
      provide the tools to make the safe, iterative, high-performance path the
      most natural path for the developer.

## The Core Abstraction: The Multi-Sequence Slice

The single most important concept to internalize is that the Rocinante
primitives are **multi-sequence by default**. The fundamental unit of operation
is not a single element, but a "slice" of elements at a given index `(!)` across
all provided sequences.

Consider `map!`. Its signature is `(map! lambda seqs [out start end])`. The
lambda's arguments are populated directly with `(elem-get seq0 (!))`,
`(elem-get seq1 (!))`, and so on.

**Example: Processing Heterogeneous Sequences**

```vdu
(defq names    "ABC")                        ;; a 'str' sequence
(defq weights  (nums 10 20 30))              ;; a 'nums' sequence
(defq colors   (list :red :green :blue))     ;; a 'list' sequence
(defq results  (list))                       ;; the collector list

(map! (lambda (name weight color) (list name weight color))
      (list names weights colors)
      results)

;; results -> (("A" 10 :red) ("B" 20 :green) ("C" 30 :blue))
```

or !

```vdu
(defq names    "ABC")                        ;; a 'str' sequence
(defq weights  (nums 10 20 30))              ;; a 'nums' sequence
(defq colors   (list :red :green :blue))     ;; a 'list' sequence

(map! list (list names weights colors))

;; -> (("A" 10 :red) ("B" 20 :green) ("C" 30 :blue))
```

Notice how `map!` effortlessly handles a `str`, a `nums` array, and a `list`.
The lambda receives three direct arguments per iteration, not a list of
arguments. This design completely obviates the need for a `zip` function in most
cases, leading to cleaner, faster, and more memory-efficient code.

## A Deep Dive into Each Primitive

### `each!`

*   **Purpose:** The fundamental iterator for side-effects. It is the most
    efficient way to "do something" for each slice of elements when no result
    needs to be collected.

*   **Signature:** `(each! lambda seqs [start end])`

*   **Behavior:** Calls the lambda for each parallel slice of elements from the
    sequences. The return value of the lambda is discarded.

*   **Key Feature: Integrated Slicing & Reversal:** The optional `start` and
    `end` parameters allow `each!` to operate on a sub-sequence of the inputs
    without creating a temporary copy. If `start > end`, the iteration proceeds
    backwards.

    ```vdu
    ;; Process the last 5 elements of two lists in reverse order
    (each! (lambda (a b) (print a b)) (list list-a list-b) -1 -6)
    ```

### `map!`

*   **Purpose:** The workhorse for data transformation.

*   **Signature:** `(map! lambda seqs [out start end])`

*   **The Collector Pattern:** This is the canonical example of the collector
    pattern. By passing an existing list as `out`, you can build complex results
    across multiple steps without allocating intermediate lists.

    * **Traditional:** `(defq result2 (mapcar #'foo (mapcar #'bar list1)))` ->
      Creates an intermediate list.

    * **ChrysaLisp:** `(map! #'foo (map! #'bar (list) list1))` -> Creates one
      list. The second `map!` appends its results directly to the list created
      by the first.

### `filter!`

*   **Purpose:** The selective collector. It is the one primitive that operates
    on a **single sequence**.

*   **Signature:** `(filter! lambda seq [out start end])`

*   **Composable Collecting:** It participates beautifully in the collector
    pattern.

    ```vdu
    (defq results (list))
    (defq stream (file-stream "data.log"))

    ;; A two-stage pipeline with no intermediate lists:
    ;; 1. Collect all lines from the file that contain "ERROR".
    (filter! (lambda (line) (found? "ERROR" line)) results (lines! (lambda (x) x) stream))
    ;; 2. Now, collect all lines that contain "CRITICAL".
    (filter! (lambda (line) (found? "CRITICAL" line)) results (lines! (lambda (x) x) stream))
    ```

    This builds a single `results` list from two different filtering criteria in
    two passes over the file, without ever holding the whole file or an
    intermediate filtered list in memory.

### `reduce!`

*   **Purpose:** The accumulator. It reduces multiple sequences down to a single
    value.

*   **Signature:** `(reduce! lambda seqs init [start end])`

*   **Multi-Sequence Lambda:** Its lambda signature is
    `(lambda (accumulator elem0 elem1 ...))`. This allows for powerful folding
    operations across parallel data streams, such as calculating the dot product
    of two vectors in a single pass.

    ```vdu
    ;; Dot product of two 'nums' arrays
    (reduce! (lambda (acc v1 v2) (+ acc (* v1 v2)))
        (list vector-a vector-b) 0)
    ```

### `some!`: The Universal Iterator

This is the most versatile primitive, unifying searching, predicate logic, and
breakable loops.

*   **Signature:** `(some! lambda seqs [mode start end])`

*   **The "Continue-If-Eql" `mode`:** `some!` continues iterating as long as the
    lambda's result is `eql` to `mode` (which defaults to `:nil`). The moment a
    different value is returned, iteration stops and that value is returned.

*   **Replacing Predicates:** The standard Lisp predicate functions are trivial
    macros over `some!`:

    * `(some ...)` is `(some! ... :nil)` -> break on first non-nil.

    * `(every ...)` is `(some! ... :t)` -> break on first nil.

*   **Replacing Breakable Loops:** This is its most profound application. It
    provides a structured, performant, and stack-safe way to implement loops
    that need to terminate early.

**Example: Safe, Iterative Tree Traversal** Consider finding a widget by name in
a deep GUI tree.

*   **Recursive way (dangerous in ChrysaLisp):** Uses machine stack, risks
    overflow.

    ```vdu
    (defun find-widget-recursive (widget name)
        (if (eql (. widget :get_name) name)
            widget
            (some! (lambda (child) (find-widget-recursive child name)) (. widget :children))))
    ```

*   **The Rocinante Way (fast, constant stack):**

    ```vdu
    (defun flatten (lst)
        ; (flatten list) -> list
        (defq out (list) stack (list lst 0))
        (while (defq idx (pop stack) lst (pop stack))
            (some! (# (cond
                ((list? %0) (push stack lst (inc (!)) %0 0) :nil)
                ((push out %0)))) (list lst) :t idx)) out)
    ```

This pattern is fundamental to ChrysaLisp. It is fast, memory-efficient, and
completely safe from stack overflows, no matter how deep the data structure is.

### The `!` Special Form

The `!` is not a variable, nor a standard function. It is a **special form**
that provides a zero-overhead bridge to the iteration engine. When the
interpreter calls `!`, it directly reads the `lisp_seq_idx` from the current
task's state—the index being managed by the currently active Rocinante
primitive—and wraps it in a `num` object. This makes the loop index "ambiently"
available inside a lambda without cluttering the signature or incurring function
call overhead.

## Conclusion: Thinking in Slices

Mastering ChrysaLisp requires a mental shift. One must stop thinking in terms of
single-purpose functions like `mapcar` and `reverse`, and start thinking in
terms of **data-flow pipelines built from composable, multi-sequence
primitives**.

The Rocinante primitives are the tools of this new way of thinking. They
encourage you to ask:

*   Can I process these streams of data in parallel instead of zipping them
    first? (Use multi-sequence `map!`).

*   Can I avoid creating this temporary list? (Use a collector `out-list`).

*   Can I process this backwards without allocating a reversed copy? (Use
    `start > end` indices).

*   Can I express this search as a breakable iteration? (Use `some!`).

*   Can I process this huge file without reading it all into memory? (Use
    `lines!`).

By answering "yes" to these questions, you begin to leverage the full power of
ChrysaLisp's architecture. You write code that is not only more expressive and
concise but also fundamentally more performant and robust. This is the path to
wielding the forms with the fluid power and precision of nun-chucks.
