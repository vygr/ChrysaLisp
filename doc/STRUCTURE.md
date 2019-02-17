# Structuring

This document covers the various structuring elements of VP/C-Script source
code. Conditionals, loops, switches, structures, enums, bits, classes, objects
etc. The definition of these functions are in the `sys/code.inc` and
`sys/class.inc` files.

## Data Structures

These are the functions that allow you to define symbols for constants, bit
masks, field offsets and types. They just declare Lisp symbols within the
`*compile-env*` environment.

### Enum

Allow you to define a set of incrementing constants. You can set the first
value and each subsequent symbol gets the current value plus one.

```
(def-enum 'bert 24)
	(enum 'a 'b 'c)
	(enum 'd)
	(enum 'e 'f)
(def-enum-end)
```

This example will start the enum count at 24, this is an optional value, and
will default to 0 if not present. The end result is a set of symbols and
assigned values of:

```
bert_a -> 24
bert_b -> 25
bert_c -> 26
bert_d -> 27
bert_e -> 28
bert_f -> 29
```

### Bits

Allow you to define a set of bit masks. Again you can set the initial bit
offset and each subsequent bit the mask is shifted left by 1.

```
(def-bit 'alf 2)
	(bit 'a 'b 'c)
	(bit 'd)
	(bit 'e 'f)
(def-bit-end)
```

This example will start the bit offset at 2, a mask value of 4, this is an
optional value, and will default to 0 if not present. The end result is a set
of symbols and assigned values of:

```
alf_a -> 4
alf_b -> 8
alf_c -> 16
alf_d -> 32
alf_e -> 64
alf_f -> 128
```

### Structures

Allow you to define field offsets and types for a set of symbols. An optional
starting offset can be provided allowing inheritance from a previous structure.

Field offsets are aligned to the natural alignment for that type ! The size of
the entire structure is not aligned.

```
(def-struct 'sue)
	(byte 'a 'b 'c)
(def-struct-end)

(def-struct 'carl)
	(short 'a 'b 'c)
(def-struct-end)

(def-struct 'bob)
	(union
		'(struct 'a 'sue)
		'(struct 'b 'carl))
	(short 'c)
	(int 'd)
	(offset 'o)
	(long 'e)
	(ptr 'f)
(def-struct-end)

(def-struct 'mary 'bob)
	(ushort 'a)
	(uint 'b)
	(pulong 'c)
	(pptr 'd)
(def-struct-end)
```

The end result is a set of symbols and assigned values of:

```
sue_a -> 0
_t_sue_a -> "b"
sue_b -> 1
_t_sue_b -> "b"
sue_c -> 2
_t_sue_c -> "b"
sue_size -> 3

carl_a -> 0
_t_carl_a -> "s"
carl_b -> 2
_t_carl_b -> "s"
carl_c -> 4
_t_carl_c -> "s"
carl_size -> 6

bob_a -> 0
_t_bob_a -> nil
bob_b -> 0
_t_bob_b -> nil
bob_c -> 6
_t_bob_c -> "s"
bob_d -> 8
_t_bob_d -> "i"
bob_o -> 12
bob_e -> 16
_t_bob_e -> "l"
bob_f -> 24
_t_bob_f -> "p"
bob_size -> 32

mary_a -> 32
_t_mary_a -> "S"
mary_b -> 36
_t_mary_b -> "I"
mary_c -> 40
_t_mary_c -> "pL"
mary_d -> 48
_t_mary_d -> "pp"
mary_size -> 56
```

## Code Structures

ChrysaLisp has a rich set of structured coding constructs. The main job of
these functions is to automate the creation of labels within your source and
track which labels auto constructed jumps should be jumping to. You could do
this all manually just using the `(vp-label)` and `(vp-bxx)` instructions, but
even within VP code this can be tedious and error prone. Better to let the
computer do this stuff for you !

### Switch

`(switch)`, `(case)`, `(default)`, `(endswitch)` and `(break)` are the
primatives that all the other structured coding functions are based upon.

```
	(switch ['name])
	(case exp1)
		...
		(break [name])
	(case exp2)
		...
		(break [name])
	(case exp3)
		...
		(break [name])
	(default)
		...
	(endswitch)
```

The switch name is optional and the case expression can be a VP list
expression, eg `'(r0 >= r3)`, `'(r4 == 56)`, which is parsed to a VP branch
instruction, or a C-Script string that is evaluated, eg `{length + 23 >
buf_len}`, and the result tested against 0 with a VP branch instruction.

Each `(case exp)` statement emits a comparison test and jumps to the next case
if the comparison fails to be true. You do not need to place a `(break)` after
each case, this will fall through to the next `(case)` statement or the
`(default)`. Be aware that falling through will execute the following `(case)`
test and act accordingly.

There are various flavours of `(case)` and `(break)`.

```
	(case exp)
	(casenot exp)
	(break [name])
	(breakif exp [name])
	(breakifnot exp [name])
```

### If

`(vpif)`, `(elseif)`, `(else)` and `(endif)` allow basic control flow.
`(break)` statements covered in the section on switch will jump to the
corresponding `(endif)` although you may use named break statements to jump to
a named `(endif)`.

```
	(vpif exp ['name])
		...
		[(break [name])]
		...
	(elseif exp)
		...
		[(breakif exp [name])]
		...
	(else)
		...
		[(breakifnot exp [name])]
		...
	(endif)

	(vpifnot exp ['name])
		...
	(elseifnot exp)
		...
	(else)
		...
	(endif)
```

Unlike switch cases, break statements will be inserted automatically, there
will be no fall through to the next condition test.

### Loops

`(loop-start)`, `(loop-end)`, `(loop-while)` and `(loop-until)` let you repeat
a section of code forever or until a condition is true at the start or end of
the loop.

You may combine the various loop start and end statements as you wish, having
conditions at the start and end of the loop as you desire.

```
	(loop-start ['name])
		...
		[(break [name])]
		[(breakif exp [name])]
		[(breakifnot exp [name])]
		[(continue)]
		[(continueif exp)]
		[(continueifnot exp)]
		...
	(loop-end)

	(loop-while exp ['name])
		...
	(loop-end)

	(loop-whilenot exp ['name])
		...
	(loop-end)

	(loop-start ['name])
		...
	(loop-until exp)

	(loop-start ['name])
		...
	(loop-untilnot exp)

	(loop-while exp ['name])
		...
	(loop-until exp)

	(loop-whilenot exp ['name])
		...
	(loop-untilnot exp)
```

## Object Structures

Classes and objects extend basic structures with methods that act on those
structures. They also define a lifecycle for the data structure and a reference
counting scheme to track when the object is no longer in use and can be
reclaimed.

### Lifecycle

```
	(call 'xxx 'create)
		(call 'xxx 'new)
			(call 'xxx 'init)
				...
				[(call 'xxx 'ref)]
				[(call 'xxx 'ref_if)]
				...
				[(call 'xxx 'deref)]
				[(call 'xxx 'deref_if)]
				...
			(call 'xxx 'deinit)
		(call 'xxx 'delete)
	(call 'xxx 'destroy)
```

For any given class there are `create` methods for that class that will take
any construction parameters and return a fully initialised instance. If the
`new` or `init` methods fail then they will tidy up and return 0 to indicate a
problem.

The `init` method is responsible for taking an allocated chunk of memory and
setting the fields of that object to the initial state, allocating any
resources etc, and failing with an error if not able to do so.

The `deinit` is the counterpart to `init`, it takes an instance to the final
state, releasing any resources, and preparing for the object to be freed.

The `new` method is responsible for allocating a chunk of memory for the object
instance.

The `delete` method frees the object instance, reversing the action of the
`new` method.

The `ref` or `ref_if` methods just increment the object reference counter.

The `deref` or `deref_if` methods decrement the object reference counter and if
it becomes 0 automatically call the `destroy` method !

### Class and Object declaration

This is an example from the `class/pair/class.inc` file.

```
(include 'class/obj/class.inc)

(def-class 'pair 'obj)
(dec-method 'vtable 'class/pair/vtable)
(dec-method 'create 'class/pair/create 'static '(r0 r1) '(r0))
(dec-method 'new 'class/pair/new 'static nil '(r0))
(dec-method 'init 'class/pair/init 'static '(r0 r1 r2 r3))
(dec-method 'ref_first 'class/pair/ref_first 'static '(r0) '(r0 r1))
(dec-method 'ref_second 'class/pair/ref_second 'static '(r0) '(r0 r1))
(dec-method 'get_first 'class/pair/get_first 'static '(r0) '(r0 r1))
(dec-method 'get_second 'class/pair/get_second 'static '(r0) '(r0 r1))
(dec-method 'set_first 'class/pair/set_first 'static '(r0 r1) '(r0))
(dec-method 'set_second 'class/pair/set_second 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/pair/deinit 'final)

(def-struct 'pair 'obj)
	(ptr 'first)
	(ptr 'second)
(def-struct-end)

;;;;;;;;;;;;;;;;
;;inline methods
;;;;;;;;;;;;;;;;

(defcfun class/pair/init ()
	;inputs
	;r0 = pair object (ptr)
	;r1 = vtable (pptr)
	;r2 = first object (ptr)
	;r3 = second object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = 0 if error, else ok
	(vp-cpy-ri r2 r0 pair_first)
	(vp-cpy-ri r3 r0 pair_second)
	(s-call 'pair 'init '(r0 r1) '(r0 r1)))

(defcfun class/pair/get_first ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	(vp-cpy-ir r0 pair_first r1))

(defcfun class/pair/get_second ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	(vp-cpy-ir r0 pair_second r1))
```

The `(def-class)` declares the class name and which class it inherits its
methods from. The `(def-struct)` declares the structure of the object instance,
inheriting from the parent class instance.

Here we can see an example of the use of inline methods. If a Lisp function is
declared matching the declared method path this is taken to mean, 'run this
function' to emit the method call.

Then the none inline method are defined in the `class/pair/class.vp` file. Note
the use of the helper method generators `(gen-new 'pair)`, `(gen-create 'pair)`
and `(gen-class 'pair)`. These helpers use the corresponding method
declarations to generate the method code for you. Take a look in
`sys/class.inc` for the implementation of these helpers.

```
(include 'sys/func.inc)
(include 'class/pair/class.inc)

(gen-new 'pair)
(gen-create 'pair)
(gen-class 'pair)

(def-method 'pair 'deinit)
	;inputs
	;r0 = pair object (ptr)
	;trashes
	;all but r0

	(vp-push r0)
	(call 'obj 'deref '((r0 pair_first)))
	(vp-cpy-ir rsp 0 r0)
	(call 'obj 'deref '((r0 pair_second)))
	(vp-pop r0)
	(s-jump 'pair 'deinit '(r0))

(def-func-end)

(def-method 'pair 'ref_first)
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;trashes
	;r2

	(vp-cpy-rr r0 r2)
	(call 'obj 'ref '((r0 pair_first)) '(r1))
	(vp-cpy-rr r2 r0)
	(vp-ret)

(def-func-end)

(def-method 'pair 'ref_second)
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;trashes
	;r2

	(vp-cpy-rr r0 r2)
	(call 'obj 'ref '((r0 pair_second)) '(r1))
	(vp-cpy-rr r2 r0)
	(vp-ret)

(def-func-end)

(def-method 'pair 'set_first)
	;inputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;trashes
	;all but r0

	(vp-push r0)
	(vp-cpy-ir r0 pair_first r2)
	(vp-cpy-ri r1 r0 pair_first)
	(call 'obj 'deref '(r2))
	(vp-pop r0)
	(vp-ret)

(def-func-end)

(def-method 'pair 'set_second)
	;inputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;trashes
	;all but r0

	(vp-push r0)
	(vp-cpy-ir r0 pair_second r2)
	(vp-cpy-ri r1 r0 pair_second)
	(call 'obj 'deref '(r2))
	(vp-pop r0)
	(vp-ret)

(def-func-end)
```

Note that the `init` and `deinit` methods make an `(s-call)` to the superclass
`init` and `deinit` methods !

Although this is a very simple class, that just hold references to two other
objects (for use by the `hash_set` and `hash_map` classes), it covers all the
basic requirements of any class.
