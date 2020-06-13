# Diary of a new feature

This is an example of adding a new feature to ChrysaLisp. From the VP assembler
support at the lowest level to the user visible Lisp function available for
Lisp apps to make use of.

Recently a request came up about being able to create fast hash set and hash
map structures at the Lisp level. Now ChrysaLisp already has a VP level class
for hash maps and hash sets, but they are specialized for handling the
environment facilities of the Lisp interpreter.

There is a fast `(find)` function that can be used to create simple and fast
flat maps, but in order to build custom structures with multiple buckets it
would be nice to have access to a hash function for VP class objects !

At least this would then allow a simple way for a user to create a set of
buckets of flat maps, for example, if the key object could return a hash value
num to the user.

OK, so there is already a hash method on the base `obj` class. It is used
internally, especially with `sym` and `str` objects for the internal
environment handling. What we want to do here is expose it to the general Lisp
level code.

## Low level VP method

In the `obj` base class there exists this virtual method definition:

```lisp
(dec-method :hash 'class/obj/hash :virtual '(r0) '(r0 r1))
```

The implementation for the `obj` class is:

```lisp
(def-method 'obj :hash)
	;inputs
	;r0 = object (ptr)
	;outputs
	;r0 = object (ptr)
	;r1 = hash code (ulong)
	;trashes
	;r1-r14

	(entry 'obj :hash '(r0))
	(exit 'obj :hash '(r0 r0))
	(vp-ret)

(def-func-end)
```

It's defined to trash all registers because this is a `virtual` method, who
knows what code actually gets run when you call the `:hash` method ? It's very
likely that all the registers are going to be trashed by any hash function of
any interest.

All this is doing at the lowest level is returning the object address as the
hash value ! Cheap and cheerful but not exactly a good distribution of hash
values, but it gives us the ability to call the `:hash` method of any object
and get back a value we can use.

Subclasses `override` this method to provide better support for the type of
object of the subclass. For example a very important one is for `str` and `sym`
objects, the core of the ChrysaLisp environment system.

The method override. Which happens to be `final` in this case is defined in the
`class/str/class.inc` file.

```lisp
(dec-method :hash 'class/str/hash :final)
```

And the implementation of this is in the `class/str/class/vp` file:

```lisp
(def-method 'str :hash)
	;inputs
	;r0 = str object (ptr)
	;outputs
	;r0 = str object (ptr)
	;r1 = hash code (ulong)
	;trashes
	;r1-r4

	(entry 'str :hash '(r0))

	(assign '((r0 str_hashcode)) '(r1))
	(vpif '(r1 = 0))
		(assign '((r0 str_length)) '(r4))
		(vp-lea-i r0 str_data r3)
		(vp-add-rr r3 r4)
		(vp-xor-rr r1 r1)
		(vpif '(r3 /= r4))
			(loop-start)
				(vp-cpy-ir-ub r3 0 r2)
				(vp-add-cr byte_size r3)
				(vp-add-rr r2 r1)
				(vp-cpy-rr r1 r2)
				(vp-shl-cr 10 r1)
				(vp-add-rr r1 r2)
				(vp-cpy-rr r2 r1)
				(vp-shr-cr 6 r2)
				(vp-xor-rr r2 r1)
			(loop-until '(r3 = r4))
		(endif)
		(vp-cpy-rr r1 r2)
		(vp-shl-cr 3 r1)
		(vp-add-rr r1 r2)
		(vp-cpy-rr r2 r1)
		(vp-shr-cr 11 r2)
		(vp-xor-rr r1 r2)
		(vp-cpy-rr r2 r1)
		(vp-shl-cr 15 r2)
		(vp-add-rr r2 r1)
		(vp-cpy-cr 0xffffffff r2)
		(vp-and-rr r2 r1)
		(assign '(r1) '((r0 str_hashcode)))
	(endif)

	(exit 'str :hash '(r0 r1))
	(vp-ret)

(def-func-end)
```

Yes, that's a big mouth full of VP assembler code, but this method is a
critical performance issue for the entire system, so it has to be. All it's
doing is scanning through the chars of the string and mixing them together into
an int that it will store as the hash code, and returns it as well.

Note the use of the instance field `str_hashcode` ! This starts life as 0 when
a new `str` object is created. A `str` object is immutable (let's not debase
ourselves with talk of what a `str_stream` is doing ;) ), so we could calculate
the hash code at create time yes ? Well yes we could but why bother ? We are
wasting time calculating a hash code that might never get used ! So only if the
`:hash` method is called are we going to calculate the value and cache it in
the `str_hashcode` field in case somebody calls it again in the future.

And yes, we may get a hash code of 0 generated, but that's very unlikely so go
report me in your "hash code I have written" blog :)

Here we see the field defined in the `str` instance.

```lisp
(def-struct 'str 'seq)
	(uint 'length 'hashcode)
	(local-align)
	(offset 'data)
(def-struct-end)
```

## Lisp level binding

What we eventually want is a Lisp level function `(hash obj) -> num` that let's
call this VP method from Lisp code. Currently there isn't one, so let's create
a binding for it.

Lisp bindings go in the `lisp.vp` files, in this case we will be adding a
function into the `class/obj/lisp.vp` file. There are already bindings present
in that file for other functions so we will just add one to the end.

First we need to add a declaration to `class/obj/class.inc` for the new method:

```lisp
(dec-method :lisp_hash 'class/obj/lisp_hash :static '(r0 r1) '(r0 r1))
```

And here is the new binding method we add to `class/obj/lisp.vp`:

```lisp
(def-method 'obj :lisp_hash)
	;inputs
	;r0 = lisp object (ptr)
	;r1 = args list object (ptr)
	;outputs
	;r0 = lisp object (ptr)
	;r1 = return value object (ptr)
	;trashes
	;r1-r14

	(entry 'obj :lisp_hash '(r0 r1))

(errorcases
	(assign '((r1 array_length)) '(r2))
	(gotoif '(r2 /= 1) 'error))

	(vp-push r0)
	(defq in (method-input `obj :hash))
	(class/array/bind_args r1 in)
	(call `obj :hash in '(_ r0))
	(call `num :create '(r0) '(r1))
	(vp-pop r0)

	(exit 'obj :lisp_hash '(r0 r1))
	(vp-ret)

(errorcases
(vp-label 'error)
	(jump 'lisp :repl_error '(r0 "(hash obj)" wrong_num_of_args r1)))

(def-func-end)
```

## Compiling

At this point we can `make` the system and we see the following:

```lisp
>make
-> obj/x86_64/AMD64/class/obj/lisp_get_field
-> obj/x86_64/AMD64/class/obj/lisp_set_field
-> obj/x86_64/AMD64/class/obj/lisp_hash
Done
```

And we can incorporate this new binding into the boot_image with:

```lisp
>make boot
Done
image -> obj/x86_64/AMD64/sys/boot_image (171828)
```

So we are done ? Not quite, we still need to define the Lisp level command that
lets us call this new binding ! And that's done with an `(ffi)` call in one of
the `lisp.inc` files where the `(ffi)` calls are kept. The master file is
`class/lisp/boot.inc` for built in language bindings and this new binding
certainly comes under that category, so we will add an entry to that file.

I'm going to add an entry at the top of this block of `(ffi)` calls as it feels
like the right kind of group to put this new `(hash)` function in.

```lisp
(ffi hash "class/obj/lisp_hash" 0)
(ffi type-of "class/lisp/lisp_type" 0)
(ffi eql "class/lisp/lisp_eql" 0)
(ffi copy "class/lisp/lisp_copy" 0)
```

## Give it a spin

Reboot the system and lets try this out in the repl:

```lisp
./run_tui.sh 
ChrysaLisp Terminal 1.5
>lisp
ChrysaLisp 1.3
Press ESC/Enter to exit.
(hash)
Error: (hash obj) wrong_num_of_args ! < () > File: stdin(1)
(hash 4 5)
Error: (hash obj) wrong_num_of_args ! < (4 5) > File: stdin(2)
```

Well we seam to have got our error cases right !

```lisp
(hash 2)
4562365688
(hash "Chris was here")
2292613118
(hash "Gary was too")
1309476830
(hash "Frank coming too")
976379024
```

My work here is done :) Enjoy building wonderful search acceleration structures folks.
