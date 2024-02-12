# Diary of a new feature

```image
apps/images/data/molecule.cpm
```

This is an example of adding a new feature to ChrysaLisp. From the VP assembler
support at the lowest level to the user visible Lisp function available for
Lisp apps to make use of.

## Requirement

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

In the `obj` base class there exists this virtual method declaration:

```file
class/obj/class.inc ":hash" "dec-method"
```

The implementation for the `obj` class is:

```file
class/obj/class.vp ":hash" "def-method"
```

It's defined to trash all registers because this is a `:virtual` method, who
knows what code actually gets run when you call the `:hash` method ? It's very
likely that all the registers are going to be trashed by any hash function of
any interest.

All this is doing at the lowest level is returning the object address as the
hash value ! Cheap and cheerful but not exactly a good distribution of hash
values, but it gives us the ability to call the `:hash` method of any object
and get back a value we can use.

Subclasses `:override` this method to provide better support for the type of
object of the subclass. For example a very important one is for `str` and `sym`
objects, the core of the ChrysaLisp environment system.

The method override, which happens to be `:final` in this case, is declared in
the `class/str/class.inc` file.

```file
class/str/class.inc ":hash" "dec-method"
```

And the implementation of this is in the `class/str/class.vp` file:

```file
class/str/class.vp ":hash" "def-method"
```

Yes, that's a big mouthful of VP assembler code, but this method is a critical
performance issue for the entire system, so it has to be fast. What it's doing
is scanning through the chars of the string and mixing them together into an
`int` that it'll store as the hash code, and returns it as well.

Note the use of the instance field `str_hashcode` ! This starts life as 0 when
a new `str` object is created. A `str` object is immutable (let's not debase
ourselves with talk of what a `sstream` is doing ;) ), so we could calculate
the hash code at create time yes ? Well yes we could but why bother ? We are
wasting time calculating a hash code that might never get used ! So only if the
`:hash` method is called are we going to calculate the value and cache it in
the `str_hashcode` field in case somebody calls it again in the future.

And yes, we may get a hash code of 0 generated, but that's very unlikely, so go
report me in your "hash code I have written" blog :)

Here we see the field defined in the `str` instance.

```file
class/str/class.inc "seq_size" ""
```

## Lisp level binding

What we eventually want is a Lisp level function `(hash obj) -> num` that lets
us call this VP method from Lisp code. Currently there isn't one, so let's
create a binding for it.

Lisp bindings go in the `lisp.vp` files, in this case we will be adding a
function into the `class/obj/lisp.vp` file. There are already bindings present
in that file for other functions so we will just add one to the end.

First we need to add a declaration to `class/obj/class.inc` for the new method:

```file
class/obj/class.inc ":lisp_hash" "dec-method"
```

And here is the new binding method we add to `class/obj/lisp.vp`:

```file
class/obj/lisp.vp ":lisp_hash" "def-method"
```

## Compiling

At this point we can `make` the system and we see the following:

```vdu
>make
-> obj/x86_64/AMD64/class/obj/lisp_get_field
-> obj/x86_64/AMD64/class/obj/lisp_set_field
-> obj/x86_64/AMD64/class/obj/lisp_hash
Done
```

And we can incorporate this new binding into the boot_image with:

```vdu
>make boot
Done
image -> obj/x86_64/AMD64/sys/boot_image (161268)
```

So we are done ? Not quite, we still need to define the Lisp level command that
lets us call this new binding ! And that's done with an `(ffi)` call in one of
the `lisp.inc` files where the `(ffi)` calls are kept. The master file is
`class/lisp/root.inc` for built in language bindings and this new binding
certainly comes under that category, so we will add an entry to that file.

I'm going to add an entry at the top of this block of `(ffi)` calls as it feels
like the right kind of group to put this new `(hash)` function in.

```vdu
(ffi hash "class/obj/lisp_hash" 0)
(ffi type-of "class/lisp/lisp_type" 0)
(ffi eql "class/lisp/lisp_eql" 0)
(ffi copy "class/lisp/lisp_copy" 0)
```

## Give it a spin

Reboot the system and lets try this out in the repl:

```vdu
./run_tui.sh
ChrysaLisp Terminal
>lisp
ChrysaLisp
Press ESC/Enter to exit.
(hash)
Error: (hash obj) wrong_num_of_args ! < () > File: stdin(1)
(hash 4 5)
Error: (hash obj) wrong_num_of_args ! < (4 5) > File: stdin(2)
```

Well we seam to have got our error cases right !

```vdu
(hash 2)
4562365688
(hash "Chris was here")
2292613118
(hash "Gary was too")
1309476830
(hash "Frank coming too")
976379024
```

My work here is done :) Enjoy building wonderful search acceleration structures
folks.

## Results

It's worth pointing out that this has now been used in the creation of standard
set and map classes `Emap, Xmap, Fmap, Lmap` and `Xset, Fset`. I refer everyone
to look at `lib/collections/`. These collection classes are present across the
entire system via `root.inc` so feel free to use them in apps without needing
to `(import)` anything.
