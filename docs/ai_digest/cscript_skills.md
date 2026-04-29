# CScript and VP LLM Skills

To effectively write mixed CScript and lower-level VP register code, it is
essential to understand what the CScript compiler produces and how it affects
manual register definitions and assignment code.

## Register Mapping vs. Variable Definition

### Symbol Binding with `vp-rdef`

The `(vp-rdef)` function binds user-supplied symbols to VP register keyword
symbols. It does not perform stack allocation or type declarations; it simply
creates a direct symbol-to-symbol association.

You can use multiple `(vp-rdef)` statements in your code. They only define or
redefine `symbol --> symbol` mappings; they do not "allocate" registers, and
only the code following the statement can see the associations you just made.

### Symbol Tables with `def-vars`

The `(def-vars)` macro creates entries in a symbol table and records the
associated type for each symbol. These entries are mapped to stack offsets
within the current scope. While it calculates offsets from `:rsp`, it does not
allocate space on the stack itself.

## Scope and Stack Management

The `(push-scope)` and `(pop-scope)` functions handle the actual allocation and
deallocation of the current scope on the stack. The "scope" consists of the
variables declared via `(def-vars)`.

* `push-scope`: Allocates the required stack space, `(vp-alloc scope_size)`.

* `pop-scope`: Deallocates the scopes stack space, `(vp-free scope_size)`.

* `pop-scope-syms`: Removes the scope symbol entries without dropping a
  `(pop-scope)` function.

* `return`: The `(return)` function emits `(vp-free scope_size)` and `(vp-ret)`
  based on the size of the current scope. It is common to see `(return)`
  followed later, after more code, by `(pop-scope-syms)`. This occurs when you
  need to keep the scope active in terms of stack slots but need `(return)` to
  output the code that performs deallocation and return.

## Register Allocation Strategies

### `vp-rdef` Mechanics and Merging

When using `(vp-rdef)`, the first list provided contains the symbols you want to
use in the source code. The second list (if provided) contains the VP register
symbols to associate with them. The function automatically merges the default
register set `'(:r0 :r1 :r2 ... :r14)` onto whatever list you provide.

If you omit the second parameter, you receive the full VP register set starting
from `:r0`. A common trick is to use `_` to skip specific registers in the
default set:

```vdu
(vp-rdef (_ _ this that _ a b c))
; this --> :r2
; that --> :r3
; a --> :r5
; b --> :r6
; c --> :r7
```

Alternatively, you can force the first few registers to specific values and let
the rest default via the merge:

```vdu
(vp-rdef (this that a b c) '(:r13 :14))
; this --> :r13
; that --> :r14
; a --> :r0
; b --> :r1
; c --> :r2
```

### Class Interfaces

The `(method-input :class :method)` and `(method-output :class :method)`
functions retrieve the raw register lists for the inputs and outputs of any
VP-level class, as defined in the `class.inc` files. You can combine `(vp-rdef)`
and `(method-input)` to align register declarations so that your register stack
matches a function's requirements:

```vdu
(vp-rdef (this vtable a b c) (method-input :obj :inst_of))
; this --> :r0
; vtable --> :r1
; a --> :r2
; b --> :r3
; c --> :r4
```

## Mixing CScript and VP Assembly

### The `assign` Function

The `(assign)` function allows you to invoke the CScript compiler, the VP-level
auto-copy system, or both. Once mastered, you can use the CScript compiler to
handle significant "grunt work," such as managing the stack for spills and
keeping track of allocations and deallocations.

### Load/Drain Patterns

When you perform a call using CScript expressions for inputs and/or outputs, a
load/drain process occurs:

```vdu
(call :class :method {in_arg0, in_arg1} {out_arg0, out_arg1})
```

The `:class :method` inputs and outputs are used to construct two `(assign)`
statements:

```vdu
(assign {in_arg0, in_arg1} (method-input :class :method))
(v-call :class :method)
(assign (method-output :class :method) {out_arg0, out_arg1})
```

### CScript for Spills and Loads

In mixed VP/CScript code, the most common use for the CScript `{}` string
expressions is to handle spill and load code. Rather than manually using
"(vp-cpy-ri :r0 :rsp offset)", you can simply use "(assign `(,r_val) {val})" and let
CScript determine the correct copy instruction.

## Best Practices and Risks

### Register Clashes

CScript produces simple stack-to-register code for basic spill and load
operations. However, if you include complex math or field access, the CScript
compiler will generate code using temporary registers, which could clash with
your `(vp-rdef)` symbols. Keep expressions limited when mixing code.

If you know that you are not keeping registers "live" across the CScript code,
you can safely let the compiler handle more complex operations.

### Safe vs. Unsafe Expressions

**Safe (Simple `:rsp` <--> `:rXX` copies, and  `constant` --> `:rXX`):**

```vdu
(assign {val} `(,val))
(assign {a, b, c} `(,a ,b ,c))
(assign `(,val) {val})
(assign `(,a ,b ,c) {a, b, c})
(assign {85} `(,val))
(assign {v1, v2, v3} `(,a ,b ,c))
```

**Unsafe (Requires caution; compiler may clobber registers):**

```vdu
(assign {val * 34} `(,val))
(assign {ptr -> +str_length} `(,len))
(assign {((a * 54) << 6) / 8} `(,val))
```

### Shared Naming

It is perfectly acceptable to name your register symbols the same as your
CScript variables. This often simplifies development, as there is no conflict
between them inside the compiler or within `assign` statements.

```vdu
(def-vars
    (ptr this args))

(vp-rdef (this args))

(push-scope)
(entry `(,this ,args))
(assign `(,this ,args) {this, args})
...
(assign {this, args} `(,this ,args))
(exit `(,this ,args))
(pop-scope)
(return)
```

Or:

```vdu
(def-vars
    (ptr this args))

(vp-rdef (this args))

(push-scope)
(entry {this, args})
...
(assign {this} `(,this))
(vp-cpy-ir this 0 args)
(assign `(,args) {args})
...
(exit {this, args})
(pop-scope)
(return)
```

## Golden rules

If it's enclosed in a `{}` it is a CScript stack variable ! If it's in a
backticked list, or a free symbol, it is a `(vp-rdef)` symbol for a raw VP
register number !

**these are symbolic VP `(vp-rdef)` registers** !

```vdu
`(,this ,args)
(vp-cpy-ir this 64 args)
(assign `((,this 64)) `(,args))
```

**these are CScript `def-vars` stack slots** !

```vdu
{this, args}
(assign {this -> 64} {args})
```
