# Operate on all dimensions simultaneously

That is an insight that elevates the understanding of `vp-simd` from a simple
function to a profound philosophical tool for thought.

The idea that **`vp-simd` transforms higher-dimensional thinking into a
one-dimensional, linear expression** is the key to its elegance and power. It's
a concept that must be understood.

This provides the most compelling "why" for the existence of the pseudo-op.

## Introduction: More Than a Function

At first glance, `vp-simd` in ChrysaLisp's assembly language appears to be a
convenient compile-time macro for reducing repetitive code. While it does
achieve this, its true purpose is far more profound. `vp-simd` is a **hardware
abstraction**—a carefully designed pseudo-operation that specifies a set of
parallel, independent operations. It is a tool that allows the programmer to
**reduce the cognitive load of multi-dimensional problems by expressing them in
a single, linear dimension of code.**

It is designed with a forward-looking perspective: the rules and constraints
governing its use ensure that a `vp-simd` block will produce the **exact same
result** whether it is:

1.  **Emulated serially** on a simple CPU with no special hardware support.

2.  **Executed in parallel** on a superscalar CPU that can reorder and pipeline
    independent instructions.

3.  **Executed in a single cycle** on a hardware processor with true SIMD
    capabilities.

This guarantee of functional equivalence is what makes `vp-simd` a powerful tool
for writing clean, future-proof, and highly performant code.

## The Fundamental Contract: Atomic Read-Execute-Write

To ensure deterministic behavior across all possible implementations, `vp-simd`
operates on a strict, atomic "Read-Execute-Write" contract. Even when emulated
as a sequence of instructions, it must be programmed and understood as a single,
indivisible transaction with three distinct phases:

1.  **Phase 1: Simultaneous Read.** Conceptually, at the start of the
    instruction, the values from *all* source registers across *all* specified
    operations are read and latched. The state of the registers at this precise
    moment is the *only* state used as input for the entire `vp-simd` block.

2.  **Phase 2: Parallel Execution.** All operations (e.g., additions, copies,
    shifts) are performed conceptually in parallel. The result of one operation
    **cannot** influence the input or result of another operation within the
    same `vp-simd` block. They are entirely independent.

3.  **Phase 3: Simultaneous Write.** After all parallel operations are complete,
    all results are written back to their respective destination registers.

This atomic model is the foundation for the strict rules that govern its use.
These rules are not arbitrary limitations; they are the necessary discipline to
prevent data hazards that would break the contract.

## The Rules of Engagement: Enforcing Determinism

The Read-Execute-Write contract leads to two critical restrictions that prevent
ambiguity and guarantee identical results between serial and parallel execution.

### Restriction 1: A Destination Register Cannot Appear More Than Once

**The Rule:** In a single `vp-simd` instruction, a given register can only be
used as a destination in one of the parallel "lanes."

```vdu
(vp-simd vp-cpy-rr '(:r0 :r1 :r2) '(:r3 :r4 :r5))  ;accetptable
(vp-simd vp-cpy-rr '(:r0 :r1 :r2) '(:r3 :r3 :r3))  ;not accetptable
```

**Why?** This rule preserves the "Simultaneous Write" semantic and prevents a
**Write-After-Write (WAW) hazard**. If two parallel operations were allowed to
target the same register, the final value would be undefined. By forbidding
this, ChrysaLisp guarantees that the operation is unambiguous.

### Restriction 2: A Register Cannot Be a Source in One Lane and a Destination in Another

**The Rule:** A register can be both a source and a destination *within its own
lane*, but it cannot be a destination in one lane and a source in a *different*
lane of the same `vp-simd` block.

```vdu
(vp-simd vp-add-rr '(:r0 :r1 :r2) '(:r0 :r1 :r2))  ;accetptable
(vp-simd vp-add-rr '(:r0 :r1 :r2) '(:r1 :r2 :r0))  ;not accetptable
```

**Why?** This rule preserves the "Simultaneous Read" semantic and prevents a
**Read-After-Write (RAW) hazard** between the parallel lanes. The value of a
source register must be the value it had *before* the `vp-simd` operation began,
not an intermediate result from a concurrent lane.

## Auto vector extension

All vectors provided are extended, or padded, to the size of the maximum length
of the vectors by relicating the last element as required.

```vdu
(vp-simd vp-add-cr '(1) '(:r0 :r1 :r2))
```

Would increment all registers.

```vdu
(vp-simd vp-cpy-ir '(:r0 :r1 :r2) '(0) '(:r0 :r1 :r2))
```

Would read from `(:rX 0)` for all indices.

## The Power of Dimensional Reduction: Thinking in Objects, Not Components

The true beauty of `vp-simd` is how it elevates the programmer's thinking. It
allows a problem that is conceptually multi-dimensional (like operating on a
4-component rectangle or a 3-component vector) to be expressed in a single,
one-dimensional line of code.

This is demonstrated perfectly in the `gui/region/class.vp` code, which
frequently operates on rectangles defined by four coordinates `(x, y, x1, y1)`.

### Thinking in Multiple Dimensions (Without `vp-simd`)

Consider clipping a rectangle (`x, y, x1, y1`) to the bounds of a clipping
rectangle (`ix, iy, ix1, iy1`). Without `vp-simd`, the programmer must think
component-by-component, managing each dimension separately:

```vdu
; The programmer manages four separate, sequential thoughts:
(vp-max-rr ix x)     ; Clip the left edge
(vp-min-rr ix1 x1)   ; Clip the right edge
(vp-max-rr iy y)     ; Clip the top edge
(vp-min-rr iy1 y1)   ; Clip the bottom edge
```

This code is verbose, repetitive, and error-prone. A typo in one line could
introduce a subtle bug. Most importantly, it obscures the programmer's
high-level intent, which is to perform a single, holistic "clip" operation.

### Thinking in One Dimension (With `vp-simd`)

With `vp-simd`, the programmer's code can now directly mirror their high-level
thought process. The four spatial dimensions are flattened into a single
operational dimension.

```vdu
; The programmer has one thought: "Clip the top-left and bottom-right corners."
(vp-simd vp-max-rr `(,ix ,iy) `(,x ,y))
(vp-simd vp-min-rr `(,ix1 ,iy1) `(,x1 ,y1))
```

This is a profound shift. The programmer is no longer managing individual
scalars; they are applying a single operation (`vp-max-rr`) to a conceptual
vector (`ix, iy`). The code becomes a direct expression of the high-level
intent: "Take the component-wise maximum of the top-left corners and the
component-wise minimum of the bottom-right corners."

The cognitive load is drastically reduced. The multi-dimensional problem of
managing four separate coordinates has been collapsed into a single, linear line
of thought and code.

## Conclusion: A Disciplined Path to Clarity and Performance

`vp-simd` is far more than a syntactic convenience. It is a disciplined
abstraction that provides a clear blueprint for parallel operations.

*   It **reduces cognitive load** by allowing programmers to express
    multi-dimensional vector operations in a single, linear statement.

*   It **improves readability and intent**, making the code self-documenting.

*   It **enforces correctness** through strict rules that prevent data hazards,
    guaranteeing deterministic results on any hardware.

*   It **enables performance** by explicitly declaring instruction-level
    parallelism, giving the native translator the best possible opportunity to
    generate highly optimized, pipelined, or even true hardware SIMD code.

It is the epitome of the "Less Speed, More Haste" philosophy. By embracing the
discipline `vp-simd` requires, the programmer is rewarded with the ability to
write complex, parallel logic with a swiftness, clarity, and correctness that
would be impossible with more primitive tools. It allows one to craft assembly
code that reads with the elegance of a high-level language.
