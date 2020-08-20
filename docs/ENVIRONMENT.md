# Environments and Symbols

In this document we cover the ChrysaLisp environment system. The types of
symbols available, the functions to bind and search for them, and how this
relates to the Lisp environment, both the root environment and individual
function environments, plus custom properties.

## Symbols

A symbol is a ChrysaLisp object that is a sequence of characters, similar to
string objects, but the difference is that a symbol has been 'interned'. What
this means is that during the `(read)` part of the REPL, when symbols are read,
a test is made to see if this symbol already exists. If it does then the symbol
that has been read is replaced with the existing symbol object, if not a new
symbol object is created and stored.

In this way ALL symbol objects that have the same character sequence become the
exact same object in memory, and share the same memory location.

Why do this ? Why bother ? Well it allows some very useful things to happen if
you know that a symbol has only one object instance no matter how or where that
symbol was read in. It makes for very fast hash map key searches and find
operations if all you need to do is check that the address of the objects match
rather than compare the character sequences.

### Standard symbols

```lisp
bert
alf
a
z57u
```

These are examples of plain old symbols. Any sequence of characters that don't
start with a ':' or a numeral.

They can be bound to a value, any other object, using `(def)` or `(defq)` like
so.

```lisp
(defq bert 56)
(defq alf "ABCD" a '(1 2 3))
(def (env) 'z57u 78)
```

`(defq)` is just the same as `(def)` but it assumes that the environment to
bind the symbol within is the current environment `(env)` and that the symbols
don't need to be quoted, hence the name `(defq)`.

`(bind)` is a more sophisticated way of binding a group of symbols within the
current environment. It is used at function invocation to bind the initial
parameter symbols to the given argument list, but it can also be used by users
as well. You may like to do this as it implements a concept known as
destructuring.

```lisp
(bind '(a b c) (list 34 "Chris" '(3 Q)))
a
34
b
"Chris"
c
(3 Q)
```

Destructuring can be done as follows. Just use nested list syntax for the
parameters to descend into the structure of the given arguments. `&optional`
and `&rest` also work as expected within these destructuring bindings. If you
wish to ignore an argument the standard is to bind it to the _ symbol.

```lisp
(bind '((x y (z0 z1 z2) _)) (list (list 1 2 (list 3 4 5) 6)))
x
1
y
2
z0
3
z1
4
z2
5
```

To change the binding of an existing bound symbol, and this will raise an error
if the symbol is not bound, you use the `(set)` and `(setq)` functions like so.

```lisp
(setq bert 56)
(setq alf "ABCD" a '(1 2 3))
(set (env) 'z57u 78)
```

You can look up the binding for a symbol with the `(get)` function. If the
symbol is not bound this will return `nil`.

```lisp
(get 'bert)
56
(get 'alf)
"ABCD"
(get 'a)
'(1 2 3)
(get 'z57u)
78
(get 'xyz)
nil
```

It is possible to unbind a symbol by using `(undef)` like so.

```lisp
(undef (env) 'bert 'alf)
(get 'bert)
nil
(get 'alf)
nil
```

`(get)` can take an optional environment to work from. `(get sym e)` and if
given the search for the symbol binding will start from that environment.

A string can be 'interned' by user code by use of the `(sym)` function. This is
often very useful when creating and managing your own fast lookup functions or
caches. Or you may be generating a symbol programmatically for your own
reasons.

```lisp
(def (env) (sym "ABCDEF") 23)
(get 'ABCDEF)
23
ABCDEF
23
```

### Keyword Symbols

Keyword symbols are symbols that start with a ':' character, and they have
special consideration when evaluated, they always evaluate to themselves ! Now
why would that be useful ?

It certainly makes using `(def)`, `(set)`, `(get)` and `(undef)` easier, due to
not having to quote the symbol ! But that's not the only reason.

As these symbols always evaluate to themselves, they pass up/down through
layers of evaluation without changing and that is a VERY useful property.

They can still be bound to values like standard symbols but `(eval)` will never
see the value they are bound to, but your own properties system will be able to
use this to its advantage.

We will cover this later, first we need to talk about environments.

## Environments

An environment is a set of symbol bindings. Under the hood it's just a hash map
that associates symbols with values.

You can look at the current environment by typing `(env)` at the REPL.

```lisp
(env)
((stdio @class/stdio/vtable) (args (4446511344)) (stdin
@class/in/vtable) (stdout @class/out/vtable) (stderr @class/out/vtable))
```

This is showing you the symbols and bindings that exist at the current level,
in this case the 'lisp' application you happen to be inside.

If you want to look up the parent of an environment you can use `(penv)`, try
typing `(penv (env))` at the REPL prompt. I'm not going to print that here as
it's way too big, but that is the boot environment that all Lisp processes have
as their shared parent environment. It's populated via the
`class/lisp/boot.inc` file that's evaluated for every Lisp process launched.

### Function environments

Every function, ie. lambda, that is called is provided with it's own empty
environment, the parent of that environment is the current environment present
at invocation.

This function environment is initially populated with the formal parameter
symbols, bound to the arguments that are passed to said function on invocation.
So before your function body starts to run it will already be able to 'see' the
formal parameter symbol bindings. It is then free to add more and use the
current bindings as it wishes.

`(defq)` or `(bind)` functions will always bind symbols in the current
environment. `(setq)` will search the environment parentage to find a bound
symbol to operate on.

`(def)`, `(set)` and `(undef)` are given the environment explicitly, so can be
used to manipulate bindings that are not within the current functions
environment.

`(get)` is given the environment optionally, so can search for bindings that
are not within the current functions environment.

### Properties

Properties are just environments created and managed by user code. While they
are free to use standard symbols they most often use keyword symbols.

How do you create a user property environment ? By use of the optional
parameters to the `(env)` function.

```lisp
(defq e (env -1))
e
()
(def e :p1 78 :p2 89)
e
((:p1 78) (:p2 89))
(get :p1 e)
78
(get :p2 e)
89
(get :p3 e)
nil
```

The optional parameter, to `(env)`, if positive, is used to change the number
of hash buckets of the current environment, or create a new isolated
environment, ie. no parent, if negative.

You may wish to increase the number of buckets in the current environment,
beyond the default of 1, if it's going to contain an extremely large number of
symbol bindings !
