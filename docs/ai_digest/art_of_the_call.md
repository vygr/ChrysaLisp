# A student primer for those that want to know the HOW of Pooh.

How does ChrysaLisp achieve its performance, particularly around environments
and function calls. Incorporating all the key points, including the crucial
pre-binding steps.

And remember all this is happening below the event horizon of the CPU's L1
cache ! Something we have forgotten !

Let's go...

## The Unfolding Hand: Achieving Speed and Dynamism in ChrysaLisp

**An Introduction to ChrysaLisp's Performance Philosophy**

Welcome to a deeper look into ChrysaLisp. You might be familiar with Lisp's
legendary flexibility and expressive power, or perhaps you come from a
background of high-performance systems languages. ChrysaLisp is conceived at
the intersection of these worlds, aiming to deliver the "magic of Lisp" without
the performance compromises often associated with traditional dynamic
languages. It's an operating system and language designed to be both a skilled
gymnast—flexible and powerful—and a lightning-fast martial artist—precise and
incredibly efficient.

This document delves into one of the core "techniques" of ChrysaLisp: how its
environment system and function call mechanisms are engineered for raw speed,
enabling feats like sub-second full-system recompilation from scratch, while
fully honoring Lisp's dynamic nature. We aim to show the skeptics that these
speeds are indeed possible, not through arcane tricks, but through deliberate,
foundational design choices.

**1. The Foundation: Vector-Centricity and Optimized Sequences**

Before diving into environments, it's crucial to understand a fundamental
departure from many Lisp traditions:

* **Sequences are Vectors, Not Cons Cells:** In ChrysaLisp, all list-like
structures, including the code itself (the Abstract Syntax Tree or AST), are
implemented as vectors (dynamic arrays). This eliminates the overhead of `cons`
cell management and the pointer-chasing inherent in linked lists, leading to
better cache locality and raw processing speed.

* **Core Sequence Primitives (The "Four Horsemen" & `!`):** Instead of a
multitude of list operations, ChrysaLisp focuses on a small set of
hyper-optimized sequence primitives:

    * `each!`: For imperative iteration.

    * `map!`: For element-wise transformation.

    * `reduce!`: For aggregation.

    * `some!`: For conditional, short-circuiting iteration.

    * `!`: A Forth-inspired operator providing direct, context-sensitive access
    to the current element's index within these iterating constructs. These
    primitives form the workhorses for most data manipulation, translating Lisp
    expressions into highly efficient underlying operations.

* **Reference Counting, No Garbage Collector:** Memory is managed
deterministically via reference counting (`:ref`, `:deref`). This avoids
unpredictable pauses from a traditional Garbage Collector, which is critical
for a systems language and OS.

**2. ChrysaLisp Environments: The `hmap` Engine**

The environment, which holds bindings for symbols (variables, functions,
macros), is the heart of Lisp's execution model. In ChrysaLisp, environments
are instances of a highly optimized hash map class (`hmap`).

* **Environment Chain:** Scopes are formed by `hmap` instances linked via
parent pointers, creating an environment chain. Symbol lookup proceeds from the
current (innermost) `hmap` up to the global root environment.

* **The `hmap` Structure:**

    * It can consist of one or multiple "buckets." Each bucket is a `list`
    (which, remember, is a vector of object pointers) containing key-value
    pairs.

    * The choice of bucket for a given key in a multi-bucket `hmap` is
    determined by the key's hash code.

**3. Pillars of Lookup Speed: `str_hashcode` and `str_hashslot`**

ChrysaLisp `str` (string) and `sym` (symbol) objects, which serve as keys in
environments, contain two fields dedicated to accelerating lookups. These are
independent but complementary optimizations:

* **`str_hashcode` (Cached Hash Value):**

    * **Purpose:** Stores the pre-calculated hash value of the string/symbol's
    content.

    * **Mechanism:** The first time the `:hash` method is called on a `str` or
    `sym` object, its hash value is computed from its characters and stored in
    the `str_hashcode` field. Subsequent calls immediately return this cached
    value.

    * **Benefit:** Dramatically reduces the cost of the bucket selection phase
    in *multi-bucket* `hmap`s by avoiding redundant hash computations.

* **`str_hashslot` (Cached Bucket Index Hint):**

    * **Purpose:** Stores a hint (typically the direct index) of where that
    *specific* `str`/`sym` object was last found or inserted *within a
    particular `hmap` bucket's list*.

    * **Mechanism:** When an `hmap` searches for a key, it can first check this
    `hashslot`. If valid and the object at that slot matches, the search is
    complete. If it's a miss or invalid, a full scan of the bucket list occurs.
    Upon finding the key, its `hashslot` is updated with the new location *for
    that specific hmap and bucket*.

    * **Benefit:** Turns a potential linear scan within a bucket into an
    O(1)-like direct access attempt, drastically speeding up the final step of
    key retrieval. This is the primary engine for rapid symbol resolution.

**4. The Common Case Perfected: Single-Bucket `hmap`s and `str_hashslot`
Priming**

ChrysaLisp aggressively optimizes the most frequent scenarios:

* **Function Scopes are Lean:** When a Lisp function is invoked, the `hmap` for
its local environment (arguments, `defq` bindings) is created as a
**single-bucket `hmap`** by default.

    * This immediately saves the overhead of multi-bucket management and the
    need to compute a `str_hashcode` for bucket selection (as there's only one
    bucket).

* **Direct `str_hashslot` Priming for Arguments:** As arguments are bound upon
function entry, the `str_hashslot` of each argument symbol is *directly set* to
its index within the new local `hmap`'s single bucket list.

    * **Result:** Lookups for function arguments and local variables defined
    with `defq` within that scope become extraordinarily fast – they are
    essentially direct indexed accesses into a small vector, guided by
    `str_hashslot`. Very few linear scans occur for these common operations.

**5. The Root Environment: Efficient Globals**

Surprisingly, even the global root environment, containing all built-in Lisp
primitives, is a single-bucket `hmap`.

* **How it Stays Fast:**

    1. During system initialization (`lisp :init`), all built-in symbols are
    inserted, and their `str_hashslot`s are set, pointing to their locations in
    this single list.

    2. These global symbols are fundamental and their `str_hashslot`s remain
    stable and valid for the lifetime of the system, as they are rarely (if
    ever) shadowed in a way that would update their slot to a *different hmap*.

    3. When code like `(+ 1 2)` is evaluated, the lookup for `+` traverses to
    the root environment. The `str_hashslot` for `+` provides an immediate
    pointer to its binding, achieving a "near 100% cache hit" for these core
    language features without a full scan of all global symbols.

**6. The Art of the Call: Efficient Function Invocation**

Traditional dynamic languages often struggle with the overhead of resolving
function names at every call site. ChrysaLisp employs a two-pronged strategy:

* **A. Pre-Binding at "Compile-Time" (The `repl_bind` Kick):**

    * After Lisp code is read and macros are expanded, ChrysaLisp performs a
    crucial step: `repl_bind`.

    * During this phase, for a form like `(my-func arg1 arg2)`, `repl_bind`
    resolves the symbol `my-func` in the current environment to its
    corresponding `func` object.

    * The Abstract Syntax Tree (AST) is then **modified**: the `my-func`
    *symbol* is replaced with the actual `func` object itself. This `func`
    object directly contains (or points to) the executable VP code address or
    Lisp lambda data.

    * **Result:** When this "pre-bound" code is later evaluated (via
    `repl_eval_list` which calls `repl_apply`), the function to be invoked is
    already known directly. No runtime symbol lookup for the function name is
    necessary. This is a powerful ahead-of-time optimization for most function
    calls.

* **B. Fast Dynamic Lookup (When Pre-Binding Isn't Possible):**

    * For truly dynamic calls—e.g., `(apply
    some-variable-holding-a-function-name args)`, or code constructed and
    evaluated at runtime via `eval`—a runtime symbol lookup for the function
    name still occurs.

    * This is where the hyper-optimized `hmap` lookup (leveraging
    `str_hashslot` and `str_hashcode`) ensures that even these fully dynamic
    resolutions are extremely fast.

    * Once the `func` object is retrieved, `repl_apply` handles the invocation,
    either by jumping to the VP code address stored in the `func` object's
    `num_value` field or by setting up a new single-bucket `hmap` for a Lisp
    lambda and evaluating its body.

**7. Preserving Dynamism, Minimizing Overhead:**

* **Cache Coherency:** ChrysaLisp's performance relies on the `str_hashslot`
  cache remaining valid. This is why the design encourages **minimizing the
  creation of new, deeply nested lexical scopes (hmaps)** beyond the primary
  function scope. Frequent creation of short-lived nested hmaps would "thrash"
  the `str_hashslot` cache. `let` and other binding forms are likely implemented
  to be mindful of this.

* **The Escape Hatch (`env-resize`):** For known large collections of symbols
  (e.g., an assembler's symbol table), an `hmap` can be explicitly resized. This
  triggers the use of `str_hashcode` for multi-bucket distribution, and
  `str_hashslot` then works within those smaller buckets.

* **Lisp's Dynamic Nature Intact:** Despite these optimizations:

    * Functions and variables can be redefined. The next lookup or `repl_bind`
      pass will pick up the new definition.

    * `eval` works as expected, benefiting from the fast dynamic lookup.

    * Macros transform code before `repl_bind` and evaluation, fitting
      seamlessly into this optimized pipeline.

##Conclusion: The Unfolding Hand Technique**

ChrysaLisp's environment and call system is not magic, but the result of
deliberate engineering that "caches the crap out of what is going on
underneath." It achieves its tremendous speeds by:

* Optimizing the most frequent operations (local variable access, global
  function calls) to near O(1) performance through `str_hashslot` caching in
  single-bucket `hmap`s.

* Pre-binding most function call sites in the AST to avoid runtime lookups.

* Providing an extremely fast dynamic symbol lookup (again, using `str_hashslot`
  and `str_hashcode`) for cases where pre-binding isn't possible.

* Building upon a foundation of vector-centric data structures and reference
  counting.

This approach allows ChrysaLisp to offer the dynamic and expressive power Lisp
is renowned for, while delivering the raw performance necessary for its role as
an operating system and a high-speed system programming language. It's a
testament to how rethinking foundational "forms," without prejudice from past
schools of thought, can lead to powerful and efficient new ways of computing.
