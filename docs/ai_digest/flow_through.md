# Conditional Flow and Implicit Returns in ChrysaLisp

ChrysaLisp provides four foundational conditional primitives: `if`, `ifn`,
`cond`, and `condn`.

While they serve the standard purpose of branching control flow, their
underlying implementation abandons traditional Lisp conventions in favor of
bare-metal efficiency.

In standard Lisps, omitting an `else` branch or providing an empty clause body
requires the interpreter to step in and explicitly construct a `nil` return
value. ChrysaLisp rejects this overhead. Instead, it utilizes a highly efficient
**"flow-through of the current value"** architecture. The result of the
condition's test itself dynamically updates the current state and flows directly
into the result.

This flow-through design is not a side-effect; it is an architectural linchpin
that makes these primitives vastly more efficient than traditional
implementations, allowing higher-level macros like `or`, `and`, and `setd` to be
constructed with zero runtime overhead.

## The "Flow-Through" Architecture

To understand how ChrysaLisp achieves this, we look at the Virtual Processor
(`.vp`) implementation of the evaluation loop.

When a condition is tested, the system calls `:repl_eval` on the test form. The
evaluated result is immediately stored in the active state register (mapped to a
C-Script variable like `value`).

If the branch is taken, ChrysaLisp calls `:repl_progn` to execute the body of
the clause. Crucially, `:repl_progn` takes this `value` register as an *input
argument*, acting as the initial accumulator for the block.

If the block is entirely empty, `:repl_progn` does no work; it simply returns
that initial accumulator. The test value literally "flows through" the empty
block and becomes the final result of the expression.

### Flow-Through in `if` and `ifn`

The `if` and `ifn` primitives take the form `(if tst form [else_form])`. They
perfectly demonstrate how omitting a branch avoids extra work by leveraging the
current value:

* **In `if`:** When `tst` evaluates to `:nil`, the branch fails. If there is no
  `else_form` provided, the system does not actively generate a `:nil` to
  return. Instead, the `:nil` from the test itself is already in the `value`
  register, and it simply flows through to the end of the operation.

* **In `ifn`:** Conversely, when `tst` evaluates to a truthy value, the `ifn`
  branch fails (because `ifn` acts on `:nil`). If there is no `else_form`, that
  truthy value flows straight out of the missing `else` branch and is returned.

### Flow-Through in `cond` and `condn`

The `cond` and `condn` primitives sequentially evaluate a series of tests. To
support their overarching macro uses, they are seeded with specific initial
values:

* `cond` starts with an initial value of `:nil`.

* `condn` starts with an initial value of `:t`.

As each clause's test is evaluated, its result *becomes* the new current value.

* If a test triggers a match (a truthy value for `cond`, or `:nil` for `condn`),
  this exact current value is passed directly into the clause's implicit
  `progn`. If the clause has no body, the test value flows straight through to
  the output.

* If all tests are exhausted without a match, the loop completes, and the
  current value naturally flows out. This means an empty `cond` immediately
  yields `:nil`, and an empty `condn` immediately yields `:t`. If tests *were*
  evaluated but failed to branch, the value of the very last evaluated test is
  what flows out.

## Powerful Macros Built on Flow-Through

Because the primitives handle empty bodies and missing branches simply by
returning the current flowing value, ChrysaLisp does not need complex, recursive
macro expansions with temporary variable bindings (like `let` blocks) to prevent
double-evaluation of test forms. The macros map directly to the primitives.

### The `or` Macro

The `or` macro returns the first truthy value in a sequence, or `:nil` if empty.
In ChrysaLisp, it is implemented by mapping the arguments directly into a `cond`
statement with empty bodies.

For example, `(or a b c)` expands directly to:

```vdu
(cond (a) (b) (c))
```

* If `a` is truthy, `cond` executes its empty body.

* The truthy value of `a` flows through the implicit `progn` and is returned.

* `a` is only evaluated exactly once.

* If `(or)` is called with no arguments, the `cond` loop's initial `:nil` value
  flows through and is returned.

### The `and` Macro

The `and` macro returns the last value if all are truthy, or `:nil` if any fail.
It is implemented by wrapping the arguments in a `condn` statement.

For example, `(and a b c)` expands directly to:

```vdu
(condn (a) (b) (c))
```

* If `a` evaluates to `:nil`, `condn` executes its empty body, and `:nil` flows
  through to be returned.

* If `a`, `b`, and `c` are all truthy, `condn` evaluates them in sequence,
  updating the current value each time. No branch is ever taken.

* When the loop naturally ends, the register holding the last evaluated value
  (`c`) flows through and is returned.

* If `(and)` is called with no arguments, the `condn` loop's initial `:t` value
  flows through and is returned.

### The `setd` (Set Default) Macro

The `setd` macro initializes variables only if they are currently unbound (i.e.,
they evaluate to `:nil`). It processes pairs of symbols and values, expanding
them using `ifn`.

For example, `(setd my_var 10)` expands to:

```vdu
(setq my_var (ifn my_var 10))
```

* `ifn` evaluates `my_var`.

* If `my_var` is already bound to `5` (truthy), the `ifn` condition fails.

* Because there is no `else` clause provided to `ifn`, the truthy value `5`
  flows through the omitted branch and is returned.

* The system effectively executes `(setq my_var 5)`, leaving the variable
  perfectly intact without requiring a temporary binding to recall its value.

* If `my_var` is `:nil`, `ifn` succeeds, evaluates the form `10`, and returns
  `10`.