# ChrysaLisp will not close over ANYTHING for you...

This concept is most important, and the explanation should be paramount. The
code serves as a minimal anchor for the idea.

"Closures are bad, Mkay"...

## The ChrysaLisp Philosophy of State: A Guide for Lisp Veterans

You MUST forget all you have learned... your only making matters worse by doing
so. Modern Lisp, modern solutions, don't live the past ! At least don't impose
the past on others !

### An Open Letter to the Experienced Lisper: We Will Not Capture Your Environment

If you are a seasoned Lisp programmer, your understanding of closures is one of
the most powerful tools in your arsenal. You expect a `lambda` to be a
"closure"—a function that captures and carries its defining lexical environment
with it, allowing it to reference variables from its birth scope, no matter
where or when it is eventually executed. This is the foundation of many powerful
Lisp idioms.

When you approach ChrysaLisp, you will find a system that looks familiar but
behaves differently at this fundamental level. Your intuition might tell you
something is missing or broken. The reality is that something has been
deliberately and purposefully replaced. This design choice is not an oversight;
it is the cornerstone of the entire OS, a choice made to achieve radical
simplicity, deterministic performance, and a system free from the overhead and
pauses of a tracing garbage collector.

This document explains this core architectural principle: ChrysaLisp's rejection
of implicit, environment-capturing closures in favor of an explicit,
object-oriented model for state management. This isn't a missing feature; it is
a re-evaluation of how behavior and state should interact in a high-performance
systems environment.

---

### 1. The Traditional Lisp Closure: The Familiar Pattern

In virtually all modern Lisps, the closure is the primary mechanism for creating
functions that retain state. The "make-adder" factory is the canonical
illustration of this concept:

```vdu
; A traditional Lisp closure
(defun make-adder (x)
  (lambda (y) (+ x y)))

(defvar add5 (make-adder 5))
(funcall add5 10) ; Returns 15
```

This works because the `lambda` is not just code; it's a **closure object**. It
is a package containing two things: a pointer to the function's code and a
reference to its defining lexical environment. The closure effectively carries
its "past" with it in a backpack.

The technology that makes this pattern safe and possible is the **tracing
Garbage Collector (GC)**. The GC is the runtime component that scans memory for
reachable objects. It sees that the `add5` function is still in use, follows its
reference to the captured environment where `x` was `5`, and ensures that
environment is not deallocated, even though the `make-adder` function has long
since returned. This is immensely powerful, but it comes at the cost of
non-deterministic pauses and a more complex runtime.

### 2. The ChrysaLisp Way: Decoupling Behavior from State

ChrysaLisp breaks this contract. It treats functions and state as separate
concerns, managed by different mechanisms. This is a core tenet of its design.

Functions and Macros are for *Behavior* !

In ChrysaLisp, a function is a pure blueprint for computation. It is stateless.

*   A Lisp-level `lambda` is just **data**—a simple `list` that contains the
    argument list and the code for the body.

*   A compiled `def-method` is just a **code pointer**—a raw entry point into
    the Virtual Processor's instruction set.

Critically, neither of these holds a reference to its defining environment. They
are memory-cheap and carry no historical baggage.

**The Interpreter holds the Environment**

The state of the lexical scope is held by the *interpreter instance* (the `lisp`
object), not the function. The interpreter's `lisp_environment` field points to
the current `hmap`, which in turn points to its parent, forming the lexical
chain.

When a ChrysaLisp function is executed, it operates on the environment that is
**currently active in the interpreter at the moment of the call**. It does not
bring its own captured environment to the party !

"leave your own angst at the door..."

This means the `make-adder` example from before would fail. When
`(make-adder 5)` returns, the temporary environment where `x` was bound to `5`
is popped from the environment stack and immediately deallocated. The returned
`lambda`, being just a list of instructions, has no memory of `x`. When it is
later called, the interpreter searches for `x` in the current global
environment, finds nothing, and fails with a `symbol_not_bound` error.

### 3. Why This Design? Performance, Determinism, and the Rejection of the GC

This design is a direct consequence of ChrysaLisp's architectural requirements
as a high-performance operating system.

*   **Avoiding the "Closure Tax":** In a traditional Lisp, the runtime must
    assume any `lambda` could become a closure. This means every function
    definition carries the potential overhead of allocating a closure object and
    managing an environment reference. ChrysaLisp eliminates this "closure tax."
    Functions are stateless and memory-cheap by default.

*   **Deterministic Memory Management:** This is the most important reason. The
    "time capsule" environments captured by classic closures have unpredictable
    lifetimes. This requires a complex tracing GC to manage. ChrysaLisp's
    strict, stack-like environment management (`env_push`/`env_pop`) ensures
    that when a scope is exited, its environment can be immediately and safely
    deallocated via simple reference counting. This leads to a highly
    predictable and efficient memory profile, free from GC pauses. This is the
    **"Well, Don't Do That Then!"** philosophy in action: instead of building a
    complex system to manage a problematic feature, it replaces the feature with
    a better alternative.

### 4. The ChrysaLisp Solution: Explicit and Selective State Capture with Objects

If functions are stateless, how does one create an entity that "grabs current
state and holds it for later use?"

**You use a class instance. Objects are ChrysaLisp's explicit, official, and
PERFORMANT mechanism for state encapsulation.**

This is the correct, idiomatic way to achieve the goal of the `make-adder`
pattern in ChrysaLisp:

1.  **Define the Shape of the State:** A `def-struct` declares the fields that
    will hold the state. For an adder, this would be a single field, `:x`.

2.  **Define the Behavior:** A `defclass` defines the methods that will operate
    on that state. The `:init` method explicitly captures the necessary values,
    and other methods like `:call` use the captured state.

3.  **Instantiate:** You create an instance of the class, `(Adder 5)`. The
    constructor runs and **selectively captures only the state it needs**,
    storing the value `5` in the new object's `:x` field.

This approach achieves the desired outcome with crucial architectural
advantages:

*   **Capture is Explicit and Selective:** The `Adder` object does not
    implicitly capture its entire environment. It explicitly copies *only* the
    value it was given into its own designated field. All other variables from
    the creating scope are allowed to be deallocated. This is VASTLY more
    memory-efficient.

*   **State is Encapsulated:** The state (`:x`) is owned by the object instance.
    The methods (`:init`, `:call`) are stateless functions that are given an
    instance to operate upon. This clean separation makes the system easier to
    reason about.

## Conclusion: A New Contract for the Lisp Programmer

ChrysaLisp asks the Lisp veteran to make a conceptual shift. It decouples
behavior from state at the most fundamental level. Yes, code IS data, but at
runtime data is not captured by default !

A traditional Lisp implicitly bundles behavior and state in a closure.
ChrysaLisp says that **functions are for behavior**, and **objects are for
state**.

This is not a limitation but a discipline. You are given more direct control.
You are not forced to pay the performance and memory tax of potential closure
capture on every function definition. Instead, you make a conscious design
choice: when you need to hold state, you explicitly create an object.

We are not capturing references for you because doing so would violate the core
principles of performance, simplicity, and deterministic memory management that
define ChrysaLisp. Instead, we provide a more powerful and explicit tool:
**classes**. By using them, you gain precise control over what state is
captured, allowing the rest of the past to be efficiently and immediately
released. You are given direct control over the state and lifetime of your
program's components, which is the foundational requirement of a true systems
language.