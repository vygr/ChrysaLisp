# Lisp

It's probably worth a few words specifically about the included Lisp and how it
works, and how many rules it breaks ! The reason for doing the Lisp was to
allow me to create an assembler to replace NASM, I was not concerned with
sticking to 'the lisp way', or whatever your local Lisp guru says. No doubt I
will have many demons in hell awaiting me...

First of all there is no garbage collector by choice. All objects used by the
Lisp are reference counted objects from the class library. Early on the Lisp
was never going to have a problem with cycles because it had no way to create
them, but as I developed the assembler I decided to introduce two functions,
`push` and `elem-set`, that could create cycles. However the efficiency
advantage in coding the assembler made me look the other way. There are now
other ways that a cycle can be created, by naming an environment within its own
scope, but again this was too good an efficiency feature to miss out on. So you
do have to be careful not to create cycles, so think about how your code works.

No tail recursion optimization ! There is a single looping function provided in
native code, `while`, every other looping construct builds on this primitive.
There are also two native primitives `some!` and `each!` that provide generic
access to iterating over a slice of a list/s, calling a function on the grouped
elements and then calling a function on the result of that. Standard `some` and
`each` are built on these but they also allow other constructs to be built and
gain the advantage of machine coded iteration. I try to stick to a functional
approach in my Lisp code, and manipulate collections of things in a functional
way with operations like `map`, `filter`, `reduce`, `each` etc. I've not found
the lack of tail recursion a problem.

All symbols live in the same environment, functions, macros, everything. The
environment is a chain of hash maps. Each lambda gets a new hash map pushed
onto the environment chain on invocation, and dereferenced on exit. The `env`
function can be used to return the current hash map and optionally resize the
number of buckets from the default of 1. This proves very effective for storing
large numbers of symbols and objects for the assembler as well as creating
caches. Make sure to `setq` the symbol you bind to the result of `env` to `nil`
before returning from the function if you do this, else you will create a cycle
that can't be freed.

`defq` and `bind` always create entries in the top environment hash map. `setq`
searches the environment chain to find an existing entry and sets that entry or
fails with an error. This means `setq` can be used to write to symbols outside
the scope of the current function. Some people don't like this, but used wisely
it can be very powerful. Coming from an assembler background I prefer to have
all the guns and knives available, so try not to shoot your foot off.

There is no cons, cdr or car stuff. Lists are just vector objects and you use
`push`, `cat`, `slice` etc to manipulate elements. Also an empty list does not
evaluate to `nil`, it's just an error.

Function and macro definitions are scoped and visible only within the scope of
the declaring function. There is no global macro list. During macro expansion
the environment chain is searched to see if a macro exists.

## Within any Lisp instance

### Built in symbols

```lisp
&rest
&optional
nil
t
```

### Built in functions

```lisp
add
age
apply
array
asr
bind
cat
catch
char
clear
cmp
code
cond
copy
def
def?
defmacro
defq
div
each!
elem
elem-set
env
eq
eql
eval
fcos
fdiv
ffi
file-stream
find
floor
fmod
fmul
frac
fsin
fsqrt
ge
gensym
gt
lambda
le
length
list
load
logand
logior
logxor
lt
macro
macroexpand
macroexpand-1
match?
max
merge
min
mod
mul
ne
not
pipe
pipe-read
pipe-write
points
pop
prin
print
progn
push
quasi-quote
quote
read
read-char
read-line
repl
save
set
setq
shl
shr
slice
some!
split
str
string-stream
sub
sym
throw
time
type-of
undef
unquote
unquote-splicing
while
write
write-char
```

### boot.inc symbols

```lisp
min_long
max_long
min_int
max_int
fp_shift
fp_2pi
fp_pi
fp_hpi
fp_qpi
fp_rpi
fp_int_mask
fp_frac_mask
```

### boot.inc macros

```lisp
and
ascii
case
compose
curry
dec
if
inc
let
minus
opt
or
rcurry
run
setd
times
unless
until
when
```

### boot.inc functions

```lisp
abs
align
lognot
count-leading-ones
count-trailing-ones
count-trailing-zeros
cpu
cubed
defun
divmod
each
each-line
each-mergeable
each-mergeable-rev
each-rev
equalp
every
each-pipe-line
filter
from-base-char
insert
log2
lst?
map
map-rev
merge
neg
notany
notevery
num?
partition
platform
prin-base
range
reduce
reduce-rev
get-byte
get-short
get-int
get-long
get-cstr
shuffle
shuffled
sign
some
sort
sorted
squared
str?
swap
sym?
to-base-char
to-num
trim
trim-end
trim-start
write-line
within-compile-env
```

## Within any cmd/lisp.lisp instance

### asm.inc symbols

```lisp
debug_mode
debug_emit
debug_inst
```

### asm.inc functions

```lisp
all-vp-files
compile
compile-pipe
compile-test
make
make-all
make-all-platforms
make-boot
make-boot-all
make-info
make-platforms
make-test
remake
remake-platforms
```

## Within a `*compile-env*` enviroment

### `*compile-env*` symbols

```lisp
*compile-env*
*compile-includes*
```

### `*compile-env*` macros

```lisp
defcvar
defcfun
defcmacro
undefc
```

### `*compile-env*` functions

```lisp
include
```
