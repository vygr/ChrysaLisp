# Assignment

This document covers how the `(assign)` function is used to copy parameter
lists, provide easy access to resource binding operators and invoke the
C-Script expression compiler to extend basic VP level coding of functions.

The `(assign src dst)` function takes source and destination parameters. These
can be lists or strings, thus giving us four possible combinations. When a
string is used this invokes the C-Script expression compiler, on that string,
and this emits VP operations that evaluate that expression. The C-Script syntax
is a subset of C/C++ syntax that most C/C++ programmer will be familiar with.

Assignment can be used directly by use of the `(assign)` function in your
source, but it is always used under the hood, by functions like `(entry)`,
`(exit)`, `(call)` etc. The input and output parameters you declare in such
calls are processed by an `(assign)` function within those functions.

Remember that whenever you pass an input or take an output using a string
parameter you will invoke the C-Script parser and compiler to generate code for
you. When passing a list you will be using VP level or VP level helper
operators to emit `(vp-cpy-xxx)` instructions for you.

## VP Assignment

Let's start with a simple example.

```
	(assign '(r0 r1 42) '(r1 r2 r3))
```

We wish to assign register r0 to r1 and register r1 to r2 and the constant 42
to r3. Sounds simple enough. So we just emit:

```
	(vp-cpy-rr r0 r1)
	(vp-cpy-rr r1 r2)
	(vp-cpy-cr 42 r3)
```

Job done, yes ? Err, no. You've just corrupted what was in r1 prior to copying
it to r2 ! The `(assign)` function does a topological sort on the parameters
you provide to make sure this does not happen ! If it cannot guarantee a cycle
free set of copy instructions to emit then it will throw an error and expect
you to sort the problem out. This happens far less often than you might
imagine, but occasionally you will need to break the cycle with a temp
register.

The above example actually gets emitted as:

```
	(vp-cpy-rr r1 r2)
	(vp-cpy-rr r0 r1)
	(vp-cpy-cr 42 r3)
```

An example of a cycle that needs user intervention would be:

```
	(assign '(r0 r1 r2) '(r1 r2 r0))
```

Ouch, no way to sort that, so you're going to have to help out manually.
Assignment will never attempt to use registers not provided or spill to the
stack. It's there to make your life easier, not to be a full blown compiler !

You can use all of the VP addressing modes as well as registers.

```
	(assign '(r0 (r1 64 i) (r3 r2 us)) '((r2 8) r1 r5))
```

This will emit:

```
	(vp-cpy-dr-us r3 r2 r5)
	(vp-cpy-ri r0 r2 8)
	(vp-cpy-ir-i r1 64 r1)
```

Often you will be using the `(vp-def)` macro to use register equated names, or
be needing to calculate a value as part of the assignment parameters. You can
either construct the parameter lists with the `(list)` function or using the
quasi-quote and comma syntax, or whatever you can dream up in Lisp that will
output a parameter list or string that `(assign)` can consume. This is where
the real power of the Lisp based assembler kicks in ! You have access to one of
the worlds most powerful languages as your assembler macro system !

```
	(vp-def (x y z))
1)	(assign `(,x ,y) `(,z ,x))
2)	(assign (list x y) (list z x))
```

## VP Assignment operators

You can add operators to your parameters that assignment emits effective
address or resource binding instructions for.

```
	(assign '((& r0 6)) '(r1))
	(assign '((& r0 r1)) '(r2))
	(assign '(($ label)) '(r0))
	(assign '("Hello world" '(r0))
	(assign '((@ "sys/mem/alloc")) '(r0))
	(assign `((@ ,(f-path 'integer 'vtable))) '(r0))
```

Will emit:

```
	(vp-lea-i r0 6 r1)
	(vp-lea-d r0 r1 r2)
	(vp-lea-p label r0)
	(fn-string "Hello world" r0)
	(fn-bind "sys/mem/alloc" r0)
	(fn-bind "class/integer/vtable" r0)
```

## C-Script Assignment

Passing a string parameter will invoke the C-Script parser and compiler ! This
is where things start to get quite high level for an assembler ! This basic C
style expression compiler is just an assembler macro set, you can look at the
implementation in detail in the `sys/code.inc` file. It's quite a simple parse
-> reverse-polish -> compile -> optimise -> emit.

While it's possible to use the compilation to do basic things like math, it
really starts to get interesting when combined with some other functions that
allow management of local stack variables. Any symbols appearing in the parsed
output will be search for in the variable scopes. These variables are not the
same as Lisp level symbols, they are managed by a few scope creation and
variable declaration functions.

```
	(byte 'x 'y ...)
	(ubyte 'x 'y ...)
	(short 'x 'y ...)
	(ushort 'x 'y ...)
	(int 'x 'y ...)
	(uint 'x 'y ...)
	(long 'x 'y ...)
	(ulong 'x 'y ...)
	(ptr 'this 'that ...)
	(pptr 'p_this 'p_that ...)
	(pbyte 'p_x 'p_y ...)
	(pubyte 'p_x 'p_y ...)
	(pshort 'p_x 'p_y ...)
	(pushort 'p_x 'p_y ...)
	(pint 'p_x 'p_y ...)
	(puint 'p_x 'p_y ...)
	(plong 'p_x 'p_y ...)
	(pulong 'p_x 'p_y ...)
	(union '(...) '(...) ...)
```

After declaring the variables and their types, you follow this with a
`(push-scope)`, and when done with them a `(pop-scope)` or `(pop-scope-syms)`.
These scope functions just emit the required `(vp-alloc)` and `(vp-free)` calls
that allocate and free space on the stack for the current set of declared
variables. Scopes can be nested if desired.

A special helper function `(return)` is provided that will emit any required
`(vp-free)` along with a `(vp-ret)` instruction anywhere inside a nested set of
`(push-scope)`, `(pop-scope)`.

## C-Script function example

This is the system level mailbox declaration function. `sys_mail::declare`

Register inputs and outputs are declared in the `sys/mail/class.inc` file just
as with a VP function.

```
(def-class 'sys_mail)
(dec-method 'declare 'sys/mail/declare 'static '(r0 r1))
```

Implementation of the function is defined in the `sys/mail/class.vp` file.

```
(def-method 'sys_mail 'declare)
	;inputs
	;r0 = mailbox name c string (pubyte)
	;r1 = mailbox id (ulong)
	;trashes
	;all

	(ptr 'mail_statics 'name)
	(ulong 'id)

	(push-scope)
	(entry 'sys_mail 'declare {name, id})

	(assign (cat {@} (f-path 'sys_mail 'statics)) {mail_statics})
	(call 'symbol 'intern_cstr {name} {name})
	(call 'integer 'create {id} {id})
	(call 'hash_map 'insert {mail_statics->ml_statics_declare_map, name, id})
	(call 'symbol 'deref {name})
	(call 'integer 'deref {id})

	(pop-scope)
	(return)

(def-func-end)
```

Here we have to declare the function as trashing all registers as use of the
C-Script compiler could end up allocating and therefore invalidating any
register. We are also aliasing the name and id variables to do double duty to
save some stack space and avoid a union ! Naughty maybe, but heck this is how
we get the performance and why we write in assembler :)

The {} are just another way of declaring a string, like with "", just that {}
allows embedded "" and via-versa. It's just a string in the end that gets
passed to `(assign)`.

The C-Script compiler emits all the code required to read and write the
variables, with their correct data type. If you are curious to see the code
emitted by the compiler you can switch on printing of the emitted code by use
of the debug_emit and debug_inst flags. A `(setq debug_inst t)` will enable
printing of each expression compilation, `(setq debug_emit t)` will enable
printing of the entire functions final instructions. Be sure to `(setq
debug_inst nil)` and `(setq debug_emit nil)` after the section of code or
function to turn emit printing off.

This is the output from wrapping the 'hash_map 'insert line above:

```
	(setq debug_inst t)
	(call 'hash_map 'insert {mail_statics->ml_statics_declare_map, name, id})
	(setq debug_inst nil)
```

```
-> obj/Darwin/x86_64/sys/mail/declare
pre opt:
(vp-lea-i rsp 0 _v0)
(vp-cpy-cr ml_statics_declare_map _v1)
(vp-cpy-ir _v0 0 _v0)
(vp-add-rr _v1 _v0)
(vp-cpy-ir _v0 0 _v0)
(vp-lea-i rsp 8 _v1)
(vp-cpy-ir _v1 0 _v1)
(vp-lea-i rsp 16 _v2)
(vp-cpy-ir _v2 0 _v2)
post opt:
(vp-cpy-ir rsp (add 0 0) _v0)
(vp-cpy-ir _v0 (add ml_statics_declare_map 0) _v0)
(vp-cpy-ir rsp (add 8 0) _v1)
(vp-cpy-ir rsp (add 16 0) _v2)
```
