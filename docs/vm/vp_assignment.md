# VP Assignment

This document covers how the `(assign)` function is used to copy parameter
lists, provide easy access to resource binding operators, and reach the
fields of memory, a function's frame among them.

The `(assign src dst)` function takes source and destination lists: of
registers, memory, constants and resource operators to registers, or of
registers to memory. Memory to memory, and a constant to memory, throw: go
by a register.

Assignment can be used directly by use of the `(assign)` function in your
source, but it is always used, under the hood, by functions like `(entry)`,
`(exit)`, `(call)` etc. The input and output parameters you declare in such
calls are processed by an `(assign)` function within those functions.

Until 2026-11-08 a parameter could also be a `(vpx ...)` form, compiled by
vpx, the expression compiler, to named locals on the stack. vpx is gone, and
`def-vars` and the scopes with it: a frame is a structure now, see Frames
below.

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

```image
docs/diagrams/assign_order.cwb
```

You can use all of the VP addressing modes as well as registers.

```vdu
	(assign '(:r0 (:r1 64 i) (:r3 :r2 us)) '((:r2 8) :r1 :r5))
```

This will emit:

```vdu
	(vp-load-rrr-us :r3 :r2 :r5)
	(vp-store-rrc :r0 :r2 8)
	(vp-load-rcr-i :r1 64 :r1)
```

Often you will be using the `(vp-rdef)` macro to use register equated names, or
be needing to calculate a value as part of the assignment parameters. You can
either construct the parameter lists with the `(list)` function or using the
quasi-quote and comma syntax, or whatever you can dream up in Lisp that will
output a parameter list or string that `(assign)` can consume. This is where
the real power of the Lisp based assembler kicks in ! You have access to one of
the worlds most powerful languages as your assembler macro system !

```vdu
	(vp-rdef (x y z))
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
	(assign `((@ ,(f-path :num :vtable))) '(:r0))
```

Will emit:

```vdu
	(vp-lea-rcr :r0 6 :r1)
	(vp-lea-rrr :r0 :r1 :r2)
	(vp-lea-pr label :r0)
	(fn-string "Hello world" :r0)
	(fn-bind "sys/mem/alloc" :r0)
	(fn-bind "class/num/vtable" :r0)
```

* $ label address reference

* & effective address reference

* "" embedded string address reference

* @ function address reference

## Frames

A function that keeps values over its calls gives them a frame on the
stack, a structure whose fields are its slots. The structure is named
`slot`, so its fields are `slot_x`, its size `slot_size`, and `(vp-alloc
slot_size)` notes it, so its slots can be kept in registers where nothing
it calls is in the way, `lib/asm/slots.inc`. `(vp-alloc)` and `(vp-free)`
round a size up to whole longs.

```vdu
(def-method :thing :pair)
	;inputs
	;:r0 = thing object (ptr)
	;:r1 = name str object (ptr)
	;outputs
	;:r0 = thing object (ptr)
	;:r1 = pair list object (ptr)
	;trashes
	;:r1-:r14

	(structure slot 0
		(ptr this name list))

	(vp-alloc slot_size)
	(entry :thing :pair '((:rsp slot_this) (:rsp slot_name)))

	(call :list :create :nil '((:rsp slot_list)))
	(call :list :push_back2 '((:rsp slot_list) (:rsp slot_this) (:rsp slot_name)))

	(exit :thing :pair '((:rsp slot_this) (:rsp slot_list)))
	(vp-free slot_size)
	(vp-ret)

(def-func-end)
```

A slot is a field like any other, so its width and sign come from its type,
an address of it is `(& :rsp slot_x)`, and several are moved at once by
`(load-fields :rsp '(slot_a slot_b) '(:r0 :r1))` and `(save-fields)`, sorted
so that arm64 pairs them. A jump out of a function, `(jump)`, `(f-jmp)` and
the like, does not free the frame: load what it takes, `(vp-free
slot_size)`, then jump.

## Build modes and emitted code

If you are curious to see the code emitted you can switch on printing of
it by use of the `*build_emit*` flag. A `(setq *build_emit* :t)` will enable
printing of the entire function's final instructions. Be sure to `(setq
*build_emit* :nil)` after the section of code or function to turn emit
printing off.

You can wrap the `(def-func-end)` of a VP function or method with the following
to see the final VP code output.

```vdu
(let ((*build_emit* :t))
	(def-func-end)
)
```

For example the `:array :append` method.

```vdu
(label '_0)
(long -1)
(short _7 _2 _5 _6 4096)
(label '_1)
(string "class/array/append")
(byte 0)
(byte (- _2 _1))
(align 8 (- _2 _1))
(label '_2)
(beq-rr :r3 :r2 _4)
(cpy-rr :r1 :r6)
(cpy-rr :r2 :r7)
(cpy-rr :r3 :r8)
(load-rcr-ui :r0 24 :r9)
(sub-rr :r2 :r3)
(lea-rrr :r3 :r9 :r1)
(store-rrc-i :r1 :r0 24)
(call-p _5)
(load-rcr :r6 16 :r1)
(shl-cr 3 :r7)
(shl-cr 3 :r8)
(add-rr :r1 :r7)
(add-rr :r1 :r8)
(load-rcr :r0 16 :r1)
(shl-cr 3 :r9)
(add-rr :r1 :r9)
(label '_3)
(load-rcr :r7 0 :r1)
(add-cr 8 :r9)
(add-cr 8 :r7)
(store-rrc :r1 :r9 -8)
(bne-rr :r8 :r7 _3)
(label '_4)
(ret)
(align 8 :nil)
(label '_5)
(long (- _6 *pc*))
(label '_6)
(string "class/array/set_cap")
(byte 0)
(align 8 :nil)
(label '_7)
```

`*build_mode*` setting in the `class/lisp/root.inc` file lets you set the
compile option for the system. The various mode are:

* 1 normal, with error checking, the default

* 2 validate, normal plus heap patterns, guard words and dead object checks

The `(validatecase)` macro allows you to conditionally include source code
only if the `*build_mode*` is 2 or more. Error checks themselves are plain
code, a `(gotoif)` or `(gotoifnot)` to an error label, and are in every build.

For example type checking of input parameters. The `(signature)` macro drops
the label `sig` at the front of the function signature table !

```vdu
	...
	(errorif-lisp-args-sig 'error :r1 3)
	...
(vp-label 'error)
	(jump :lisp :repl_error '(:r0 :r1 "(piece-scans brd index vectors)" +error_msg_wrong_types))
	(signature '(:str :num :list))
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
`(assign)` will attempt to lookup the type as declared in any `(structure)`
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
	(vp-load-rcr-ui :r0 str_length :r0)
```

## Output register assignment

Often you may wish to know what the output register/s are from a method call,
maybe you want to test a return value for an error condition or such. While
it's possible to use the `(method-output :class :method)` call to access them,
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
