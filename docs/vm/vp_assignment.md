# VP Assignment

This document covers how the `(assign)` function is used to copy parameter
lists, provide easy access to resource binding operators and invoke the
C-Script expression compiler to extend basic VP level coding of functions.

The `(assign src dst)` function takes source and destination parameters. These
can be lists or strings, thus giving us four possible combinations. When a
string is used this invokes the C-Script expression compiler, on that string,
and this emits VP operations that evaluate that expression. The C-Script syntax
is a subset of C/C++ syntax that most C/C++ programmers will be familiar with.

Assignment can be used directly by use of the `(assign)` function in your
source, but it is always used, under the hood, by functions like `(entry)`,
`(exit)`, `(call)` etc. The input and output parameters you declare in such
calls are processed by an `(assign)` function within those functions.

Remember that whenever you pass an input or take an output using a string
parameter you will invoke the C-Script parser and compiler to generate code for
you. When passing a list you will be using VP level or VP level helper
operators to emit `(vp-cpy-xxx)` instructions for you.

## VP Assignment

Let's start with a simple example.

```vdu
	(assign '(:r0 :r1 42) '(:r1 :r2 :r3))
```

We wish to assign register `:r0` to `:r1` and register `:r1` to `:r2` and the
constant `42` to `:r3`. Sounds simple enough. So we just emit:

```vdu
	(vp-cpy-rr :r0 :r1)
	(vp-cpy-rr :r1 :r2)
	(vp-cpy-cr 42 :r3)
```

Job done, yes ? Err, no. You've just corrupted what was in `:r1` prior to
copying it to `:r2` ! The `(assign)` function does a topological sort on the
parameters you provide to make sure this does not happen ! If it can't
guarantee a cycle free set of copy instructions then it'll throw an error and
expect you to sort the problem out. This happens far less often than you might
imagine, but occasionally you will need to break the cycle with a temp
register.

The above example actually gets emitted as:

```vdu
	(vp-cpy-rr :r1 :r2)
	(vp-cpy-rr :r0 :r1)
	(vp-cpy-cr 42 :r3)
```

An example of a cycle that needs user intervention would be:

```vdu
	(assign '(:r0 :r1 :r2) '(:r1 :r2 :r0))
```

Ouch, no way to sort that, so you're going to have to help out manually.
Assignment will never attempt to use registers not provided or spill to the
stack. It's there to make your life easier, not to be a full blown compiler !

You can use all of the VP addressing modes as well as registers.

```vdu
	(assign '(:r0 (:r1 64 i) (:r3 :r2 us)) '((:r2 8) :r1 :r5))
```

This will emit:

```vdu
	(vp-cpy-dr-us :r3 :r2 :r5)
	(vp-cpy-ri :r0 :r2 8)
	(vp-cpy-ir-i :r1 64 :r1)
```

Often you will be using the `(vp-def)` macro to use register equated names, or
be needing to calculate a value as part of the assignment parameters. You can
either construct the parameter lists with the `(list)` function or using the
quasi-quote and comma syntax, or whatever you can dream up in Lisp that will
output a parameter list or string that `(assign)` can consume. This is where
the real power of the Lisp based assembler kicks in ! You have access to one of
the worlds most powerful languages as your assembler macro system !

```vdu
	(vp-def (x y z))
	(assign `(,x ,y) `(,z ,x))
	(assign (list x y) (list z x))
```

## VP Resource operators

You can add operators to your parameters which assignment emits effective
address or resource binding instructions for.

```vdu
	(assign '((& :r0 6)) '(:r1))
	(assign '((& :r0 :r1)) '(:r2))
	(assign '(($ label)) '(:r0))
	(assign '("Hello world") '(:r0))
	(assign '((@ "sys/mem/alloc")) '(:r0))
	(assign `((@ ,(f-path 'num :vtable))) '(:r0))
```

Will emit:

```vdu
	(vp-lea-i :r0 6 :r1)
	(vp-lea-d :r0 :r1 :r2)
	(vp-lea-p label :r0)
	(fn-string "Hello world" :r0)
	(fn-bind "sys/mem/alloc" :r0)
	(fn-bind "class/num/vtable" :r0)
```

* $ label address reference

* & effective address reference

* "" embedded string address reference

* @ function address reference

## C-Script Assignment

Passing a string parameter will invoke the C-Script parser and compiler ! This
is where things start to get quite high level for an assembler ! This basic C
style expression compiler is just an assembler macro set, you can look at the
implementation in detail in the `lib/asm/cscript.inc` file. It's quite a
simple, tokenize -> reverse-polish -> compile -> optimise -> emit.

While it's possible to use the compilation to do basic things like math, it
really starts to get interesting when combined with some other functions that
allow management of local stack variables. Any symbols appearing in the parsed
output will be searched for in these variable scopes. These variables are not
the same as Lisp level symbols, they are managed by a few scope creation and
variable declaration functions.

```vdu
	(def-vars
		(byte x y ...)
		(ubyte x y ...)
		(short x y ...)
		(ushort x y ...)
		(int x y ...)
		(uint x y ...)
		(long x y ...)
		(ulong x y ...)
		(ptr this that ...)
		(pptr p_this p_that ...)
		(pbyte p_x p_y ...)
		(pubyte p_x p_y ...)
		(pshort p_x p_y ...)
		(pushort p_x p_y ...)
		(pint p_x p_y ...)
		(puint p_x p_y ...)
		(plong p_x p_y ...)
		(pulong p_x p_y ...)
		(union (...) (...) ...)
		...)
```

After declaring the variables and their types, you follow this with a
`(push-scope)`, and when done with them a `(pop-scope)` or `(pop-scope-syms)`.
These scope functions just emit the required `(vp-alloc)` and `(vp-free)` calls
that allocate and free space on the stack for the current set of declared
variables. Scopes can be nested if desired.

A special helper function `(return)` is provided that will emit any required
`(vp-free)` along with a `(vp-ret)` instruction anywhere inside a nested set of
`(push-scope)`, `(pop-scope)`.

## C-Script Resource operators

You can add resource operator prefixes to your C-Script string parameters too.
These are in addition to the C/C++ style operators.

```vdu
	(assign {&var1, &var2} {p_var1, p_var2})
	(assign {$label} {p_label})
	(assign {"Hello", "World"} {p_str1, p_str2})
	(assign {"Hello", "World"} '(:r2 :r3))
	(assign {@sys/mem/alloc} {p_alloc})
	(assign (cat {@} (f-path 'num :vtable)) {p_vtable})
```

## C-Script function example

This is the system level mailbox declaration function. `'sys_mail :declare`

Register inputs and outputs are declared in the `sys/mail/class.inc` file just
as with a VP function.

```vdu
(def-class sys_mail :nil
	(dec-method :declare sys/mail/declare :static (:r0 :r1)))
```

Implementation of the function is defined in the `sys/mail/class.vp` file.

```vdu
(def-method 'sys_mail :declare)
	;inputs
	;:r0 = mailbox name c string (pubyte)
	;:r1 = mailbox id (ulong)
	;trashes
	;:r0-:r14

	(def-vars
		(ptr statics name)
		(ulong id))

	(push-scope)
	(entry 'sys_mail :declare {name, id})

	(assign {@sys/statics/statics} {statics})
	(call 'sym :intern_cstr {name} {name})
	(call 'num :create {id} {id})
	(call 'hmap :insert {statics->statics_sys_mail_service_map, name, id})
	(call 'sym :deref {name})
	(call 'num :deref {id})

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
variables, with their correct data type.

## Errorcases and debug modes

If you are curious to see the code emitted by the compiler you can switch on
printing of the emitted code by use of the `*debug_emit*` and `*debug_inst*`
flags. A `(setq *debug_inst* :t)` will enable printing of each expression
compilation, `(setq *debug_emit* :t)` will enable printing of the entire
functions final instructions. Be sure to `(setq *debug_inst* :nil)` and `(setq
*debug_emit* :nil)` after the section of code or function to turn emit printing
off.

This is the output from wrapping the `'hmap :insert` line in the example above:

```vdu
(let ((*debug_inst* :t))
	(call 'hmap :insert {statics->statics_sys_mail_service_map, name, id})
)
```

```vdu
-> obj/Darwin/x86_64/sys/mail/declare
pre opt:
	(vp-lea-i :rsp 0 _v0)
	(vp-cpy-cr statics_sys_mail_service_map _v1)
	(vp-cpy-ir _v0 0 _v0)
	(vp-add-rr _v1 _v0)
	(vp-cpy-ir _v0 0 _v0)
	(vp-lea-i :rsp 8 _v1)
	(vp-cpy-ir _v1 0 _v1)
	(vp-lea-i :rsp 16 _v2)
	(vp-cpy-ir _v2 0 _v2)
post opt:
	(vp-cpy-ir :rsp (+ 0 0) _v0)
	(vp-cpy-ir _v0 (+ statics_sys_mail_service_map 0) _v0)
	(vp-cpy-ir :rsp (+ 8 0) _v1)
	(vp-cpy-ir :rsp (+ 16 0) _v2)
```

You can wrap the `(def-func-end)` of a VP function or method with the following
to see the final VP code output.

```vdu
(let ((*debug_emit* :t))
	(def-func-end)
)
```

For example the `'array :append` method.

```vdu
(emit-label '_0)
(emit-long -1)
(emit-short _7 _2 _5 _6 4096)
(emit-label '_1)
(emit-string "class/array/append")
(emit-byte 0)
(emit-byte (- _2 _1))
(emit-align 8 (- _2 _1))
(emit-label '_2)
(emit-beq-rr :r3 :r2 _4)
(emit-cpy-rr :r1 :r6)
(emit-cpy-rr :r2 :r7)
(emit-cpy-rr :r3 :r8)
(emit-cpy-ir-ui :r0 24 :r9)
(emit-sub-rr :r2 :r3)
(emit-lea-d :r3 :r9 :r1)
(emit-cpy-ri-i :r1 :r0 24)
(emit-call-p _5)
(emit-cpy-ir :r6 16 :r1)
(emit-shl-cr 3 :r7)
(emit-shl-cr 3 :r8)
(emit-add-rr :r1 :r7)
(emit-add-rr :r1 :r8)
(emit-cpy-ir :r0 16 :r1)
(emit-shl-cr 3 :r9)
(emit-add-rr :r1 :r9)
(emit-label '_3)
(emit-cpy-ir :r7 0 :r1)
(emit-add-cr 8 :r9)
(emit-add-cr 8 :r7)
(emit-cpy-ri :r1 :r9 -8)
(emit-bne-rr :r8 :r7 _3)
(emit-label '_4)
(emit-ret)
(emit-align 8 :nil)
(emit-label '_5)
(emit-long (- _6 *pc*))
(emit-label '_6)
(emit-string "class/array/set_cap")
(emit-byte 0)
(emit-align 8 :nil)
(emit-label '_7)
```

`*debug_mode*` setting in the `class/lisp/root.inc` file lets you set the
compile option for the system. The various mode are:

* 0 release, strip all error checking

* 1 normal, with error checking

The `(errorcase)`, `(errorif)` and `(errorifnot)` macros alow you to
conditionally include source code if the `*debug_mode*` is greater than 0.

For example adding conditional type checking of input parameters that will be
removed in release mode. The `(signature)` macro drops the label `sig` at the
front of the function signature table !

```vdu
	...
	(errorif-lisp-args-sig 'error :r1 3)
	...
(errorcase
(vp-label 'error)
	(jump 'lisp :repl_error '(:r0 "(piece-scans brd index vectors)" +error_msg_wrong_types :r1))
	(signature '(str num list)))
	...
```

## Limitations

The `(assign)` function has to be very fast, useful in the general cases it has
to deal with, but can't be a full blown compiler. So it has limitations on what
it attempts to do and what it can guarantee.

Most of the time calling functions requires `(assign)` to martial parameters
from memory or registers into the functions input registers or martial the
functions output registers into the user registers or memory locations. In
these cases it never has to deal with memory to memory operations. It doesn't
even attempt to sort an assignment item if the destination is a memory
location, it can't know if that might alias with an input etc, it's not doing a
full use/def analysis.

The main thing it concentrates on is making sure to sort the assignment list so
that any register destinations do not get written to before they are used by
any future source or destination items.

## Auto typing

If you don't provide a qualifier for the copy type, `i ui s us b ub`,
`(assign)` will attempt to lookup the type as declared in any `(def-struct)`
for that field. If not found or not a symbol it'll default to a long. If the
type is found it'll use the correct VP copy instruction that matches the
field type.

This saves a huge amount of finger trouble with field accesses and is the
standard way you should access fields unless you have a good reason not to.

So for example:

```vdu
	(assign '((:r0 str_length)) '(:r0))
```

This will know from the type of 'str_length', a uint, that it should output the
following.

```vdu
	(vp-cpy-ir-ui :r0 str_length :r0)
```

## Output register assignment

Often you may wish to know what the output register/s are from a method call,
maybe you want to test a return value for an error condition or such. While
it's possible to use the `(method-output 'class :method)` call to access them,
there is a shortcut available that uses the `(assign)` function to do the job.

If you provide a none key symbol as an output, the corresponding output
register name will be bound to that symbol within the `*func_env*` environment.

For example, checking the error return value from type checking code could be
done as follows:

```file
class/seq/lisp.vp ":env_args_type" ""
```

Here `tmp` is bound to the register coming out of the call to `:env_args_type`
and later the call to `:env_args_match` ! You can then use that register,
whatever it is, in subsequent code.
