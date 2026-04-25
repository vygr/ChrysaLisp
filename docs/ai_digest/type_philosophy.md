# The ChrysaLisp Type Philosophy: Formless Polymorphism and Pure Templates

When evaluating programming languages, computer scientists invariably ask: *Is
it strongly or weakly typed? Is it static or dynamic? Is it strict or
permissive?*

For ChrysaLisp, applying these traditional labels yields a paradox. The system
allows you to build a complex, bare-metal OS kernel entirely in Lisp without
ever writing a type cast. Yet, if you pass the wrong object to a system function
during development, the interpreter will instantly halt with a strict signature
violation.

To understand the ChrysaLisp type system, one must look past standard taxonomy
and examine its guiding philosophy. ChrysaLisp is a **strongly typed,
dynamically evaluated, structurally polymorphic language built on pure
templates**. It embodies the Bruce Lee principle: *"Be formless, shapeless, like
water."*

"So What is it ?" Cat, Red Dwarf.

Here is the philosophy behind how ChrysaLisp thinks about types.

## 1. Strongly Typed Values, Formless Variables

At its core, ChrysaLisp is **strongly typed**. An object in memory never lies
about what it is. Because every object's first slot is a pointer to its vtable,
a `:num` is fundamentally distinct from a `:str`, and the Virtual Processor (VP)
handles them uniquely at the hardware level. There is no implicit, weak coercion
(like JavaScript treating `"5" + 1` as `"51"`).

However, while the *values* are strongly typed, the *variables* are completely
formless. A variable in ChrysaLisp is merely a symbol bound in a lexical
`:hmap`. It imposes no constraints on the data it holds. You do not declare a
variable as a `String` or an `Int`; you simply pour data into it. The variable
takes the shape of the data.

## 2. No Closures: Functions as Pure Templates

The most profound impact on ChrysaLisp's type philosophy stems from a
deliberate, performance-driven design choice: **ChrysaLisp `lambda`s do not
create closures.**

In traditional Lisp or Scheme, a closure captures its defining environment,
freezing state and type context. In ChrysaLisp, a function is a **pure AST
template**. When invoked, it is injected into, and executes exclusively within,
the environment of its caller.

Because functions have no "memory" of where they were defined, they cannot rely
on captured, statically known types. They must be universally adaptable to
whatever context they are dropped into. This forces the language into an
inherently **polymorphic** style. A ChrysaLisp function acts much like a C++
template: it blindly applies operations to its arguments, and as long as the
arguments support those operations at the moment of execution, the function
succeeds.

## 3. Radical Duck Typing: Behavior over Lineage

Because functions are pure templates, ChrysaLisp code rarely relies on
traditional class hierarchies or explicit type checking. While you *can* use
`(type-of)` or predicates like `(list?)` or `(str?)`, idiomatic ChrysaLisp
heavily discourages this in favor of **Structural Polymorphism** (Duck Typing).

The golden rule of ChrysaLisp is: ***If it splits like a sequence and cats like
a sequence, it's a sequence!***

You do not need to check if an object inherits from `:seq`. If you pass a
`:list`, an `:array`, a `:str`, a `:nums` (numeric vector), or a `:fixeds`
vector into a function, and that function calls `(slice obj 0 2)` or `(length
obj)`, it will simply work. The dot-syntax method dispatch `(. obj :method)`
routes the operation to the correct VP-level or Script-level vtable.

The developer focuses entirely on the *behavior* required by the algorithm, not
the *lineage* of the data.

## 4. Schrödinger’s Strictness: The Dual-Mode Paradigm

If ChrysaLisp relies on fluid duck typing, how does it prevent absolute chaos in
a concurrent operating system? It achieves this through a dual-mode philosophy
regarding strictness.

ChrysaLisp is **Strictly Typed during development, and Permissively Typed in
production.**

Through the `errorcase` macro, VP-level functions are littered with signature
checks:

```vdu
(errorif-lisp-args-sig 'error :r1 2)
...
(signature '(:str :num))
```

In `*build_mode* 1` (Debug), ChrysaLisp acts like a strict, statically typed
language. It rigorously validates that the caller provided exactly a string and
a number. If you pass an object, it halts with `wrong_types`.

But in `*build_mode* 0` (Release), the type system vanishes. The `errorcase`
macros compile to nothing. The system transitions to the "Well, Don't Do That
Then!" philosophy. It trusts that the polymorphic template logic you wrote and
tested in debug mode is mathematically sound. It removes the training wheels and
executes at bare-metal speeds.

## Conclusion

ChrysaLisp eschews the heavy boilerplate of static typing and the hidden runtime
overhead of dynamic type-tracking.

Instead, it offers a philosophical middle ground: **Template-driven
Polymorphism**. By stripping away closures, relying on O(1) vtable dispatch, and
erasing signature checks at compile time, it allows the developer to write
incredibly generic, highly reusable code that miraculously compiles down to
tightly optimized, strongly typed machine instructions.