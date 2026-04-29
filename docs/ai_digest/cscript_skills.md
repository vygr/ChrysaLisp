# CScript and VP LLM skills

In order to be able to write mixed CScript and lower level VP register code you
REALLY need to understand what the CScript compiler will produce and how that
effects what your manual register definition and assign code is doing.

All that the `(vp-rdef)` function does is bind some user supply symbols to VP
register keyword symbols. There is no stack allocation, no type declarations,
nothing. Just a simple symbol to symbol association is created.

The `(def-vars)` macro on the other hand creates entries in a symbol table and
remembers the associated type declared for that symbol. And these entries are
mapped to stack offsets, this is the current scope. All that happens is the
building of an offsets from `:rsp`. Nothing has even allocated that space on the
stack yet !

The `(push-scope)` and `(pop-scope)` functions do the actual work of allocating
and deallocating the current scope, the `scope` is whatever you just declared
with `(def-vars)` !

 `(pop-scope-syms)` function removes the scope symbol entries, without dropping
 a `(return)` function, like `(pop-scope)` does. `(return)` will drop a
 `(vp-free offset)`, `(vp-ret)` based on the size of the current scope.

 This is why you can see `(return)` and then later on a `(pop-scope-syms)` !
 Whats happening is that your keeping the scope active as far as stack slots,
 but need to drop code as does the dealloc and ret !

 The `(assign)` function lets you invoke the CScript compiler, and/or the VP
 level auto copy system. You can mix and match, you just need to understand what
 is going to happen. The rules are not that difficult to understand and once
 mastered, you can get the CScript compiler to do a LOT of grunt work, looking
 after the stack for your spills, keeping track of the alloc and frees, etc.

 Other useful functions are the `(method-input :class :method)` and
 `(method-output :class :method)`. They let you retrieve the raw register lists
 for the ins/outs of any VP level class. This is the information in the
 `class.inc` files.

 Clever code can combine the `(vp-rdef)` and `(method-input)` functions to line
 up register declarations so that your stack of registers needed going into a
 function call can be used as the `drain` for the CScript --> VP transfer, and
 likewise on the output registers.

This load/drain from/to the calls inputs/outputs actually does happen when you
do a call using CScript expressions for the inputs and/or outputs.

```vdu
(call :class :method {in_arg0, in_arg1} {out_arg0, out_arg1})
```

The `:class :method` inputs and outputs are used here to construct 2 `(assign)` statements.

```vdu
(assign {in_arg0, in_arg1} (method-input :class :method))
(v-call :class :method)
(assign (method-output :class :method) {out_arg0, out_arg1})
```

When you use `(vp-rdef)` this first list you provide is the list of symbols you
want to use in the source code, the second, if provided, is the list of VP
register symbols you wish to associate with those. BUT you the function always
merges `'(:r0 :r1 :r2 :r3 ... :r13 :14)` onto the list you provide !

What this means is you can omit the second parameter and will get the full VP
register set from '(:r0 ... :r14). A common trick is to use `_` to use up a few
of the default set.

```vdu
(vp-rdef (this that a b c) '(_ _))
this --> :r2
that --> :r3
a --> :r4
b --> :r5
c --> :r6
```

Or you can force the first few registers to be what you want and let the rest
default via the merge.

```vdu
(vp-rdef (this that a b c) '(:r13 :14))
this --> :r13
that --> :r14
a --> :r0
b --> :r1
c --> :r2
```

Or use `(method-input)` to fix the first ones.

```vdu
(vp-rdef (this vtable a b c) (method-input :obj :inst_of))
this --> :r0
vtable --> :r1
a --> :r2
b --> :r3
c --> :r4
```

You can use multiple `(vp-rdef)` statements in your code, they only defined or
redefine `symbol --> symbol` mappings, They don't `allocate` registers and only
the following code can see the associations you just made.

In mixed VP/CScript code you the most common use for the CScript `{}` string
expressions is to handkle your spill and load code. Rather than manually using
`(vp-cpy-ri :r0 :rsp offset)`, it's nice to be able to just say `(assign r_val
{val})` and let the Cscript figure out the correct copy instruction.

The important thing to remember here is that CScript will produce this simple to/from stack code only for this simple spill and load form of use ! If you start including math or field access code etc, the CScript compiler will produce code using temp registers, and that could clash with your `(vp-rdef)` symbols ! so keep the expressions limited if you do mix code.

Having said that, if you DO know that your not keeping registers `live` over the CScript code, feel free to get it to do the donkey work !

```vdu
;all safe, only :rsp <--> :rXX code
(assign {val} `(,val))
(assign {a, b ,c} `(,a ,b ,c))
(assign `(,val) {val})
(assign `(,a ,b ,c) {a, b ,c})
(assign {85} `(,val))
(assign {1, 2, 3} `(,a ,b ,c))

;not safe, know what your doing !
;compiler could trash anything !
(assign {val * 34} `(,val))
(assign {ptr->+str_length} `(,len))
(assign {((a * 54) << 6) / 8} `(,val))
```

It is perfectly ok to name your register symbols the same as your CScript
variables, and this can make life easier. There is no clash between them inside
the compiler or assign statements.

```vdu
(def-vars
    (ptr this args))

(vp-rdef (this args))

(entry `(,this ,args))
(push-scope)
(assign `(,this ,args) {this, args})
...
(assign {this, args} `(,this ,args))
(exit `(,this ,args))
(pop-scope)
(return)
```

Or

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

All perfectly fine.

