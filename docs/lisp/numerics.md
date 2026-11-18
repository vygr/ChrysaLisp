# Numbers and Vectors

ChrysaLisp has four kinds of number: integers, fixed point, doubles and
floats, and a vector of each, a typed array of their raw values. The number
classes are a ladder of what each kind can do, each doing all that the one it
comes of does and more, and a vector class sits beside each.

```image
docs/diagrams/numeric_types.cwb
```

## The ladder

```code
:num -> :fixed -> :double -> :float
:nums -> :fixeds -> :doubles -> :floats
```

How a kind keeps its value is its own affair, in the methods it overrides. What
the ladder says is what it can do:

* `:num`, the integers: arithmetic, compares, the bitwise operations.

* `:fixed` adds the fractions, what drawing works in: `(frac)` `(floor)`
`(ceil)` `(recip)` `(sin)` `(cos)`, as the Canvas, fonts and paths need.

* `:double` adds the real numbers beyond that: `(exp)` `(log)` `(tanh)`, and
`(quant)`. It is Lisp's general real number.

* `:float` does what a double does, at 32 bits, the width of a GPU. Every
float is exactly a double, so a float can go wherever a double is asked for.

The vectors are the same: `:fixeds` adds `(fixeds-frac)` `(fixeds-floor)`
`(fixeds-ceil)` to what `:nums` does, `:doubles` the real numbers, and `:floats`
is a `:doubles` at 32 bits. Others to come, a vector of 16 bit floats, go
beside `:floats`, under `:doubles`.

So `(fixed? x)` asks whether x can do what a fixed does, and is true of a
double and a float; `(double? x)` is true of a float. Each has a `??` that asks
what x is exactly: `(fixed?? x)` is true only of a fixed, `(double?? x)` only
of a double. Code that reads a value as it is stored asks the `??`, so does a
VP function that takes its args by the signature of a class in capitals,
`(signature '(:DOUBLES))`, exactly that class; in lower case any that comes of
it.

## Numbers

### Num

These are 64 bit integer numbers, the size of a VP register. The VP class,
`class/num/`, holds the integer in the `num_value` field, a `boxed` integer as
other languages say.

During `(read)` any symbol beginning with a numeral and having no `.` will be
parsed to a num object. You can specify the base with a prefix of `0b`, `0x` or
`0o` for binary, hex or octal. Nums parsed during `(read)` are interned in a
similar manner to symbols.

Useful constants defined in `root.inc`.

`+min_long +max_long +min_int +max_int`

### Fixed

These are `48.16` fixed point numbers. The VP class, `class/fixed/`, holds a
fixed value in the `num_value` field. Most of the Canvas class drawing
operations take Fixed numbers for coordinates, and work in them inside.

During `(read)` any symbol beginning with a numeral and having a `.` will be
parsed to a fixed object, the base given as for a num. Fixeds parsed during
`(read)` are interned in a similar manner to symbols.

In VP code a multiply of two is a `vp-mul-rr` followed by a `vp-asr-cr` of
`+fp_shift` bits, and a divide a `vp-shl-cr` of `+fp_shift` bits followed by
a `vp-div-rr`.

There is an extensive VP level macro set for fixed point vectors held in
registers, the `vec-` macros of `sys/math/class.inc`: the path generators and
strokers of `gui/path/` are written in them.

Useful constants defined in `root.inc`.

`+fp_shift +fp_int_mask +fp_frac_mask +fp_2pi +fp_pi +fp_hpi +fp_rpi`

### Double

These are 64 bit IEEE doubles. The VP class, `class/double/`, holds a double
value in the `num_value` field.

Useful constants defined in `lib/math/vector.inc`.

`+double_0 to +double_10 +double_-1 to +double_-10 +double_1/2 to +double_1/20 +double_-1/2 to
+double_-1/20 +double_pi +double_hpi +double_2pi`

### Float

These are 32 bit IEEE singles. The VP class, `class/float/`, holds a float in
the low half of the `num_value` field, the upper half clear, so the same float
is always the same value. Its arithmetic and compares are in 32 bits, as a
GPU's are, so a float gives the same answer on every CPU and on a GPU.

## Conversions

There are four conversion functions built in. `(n2i)`, `(n2q)`, `(n2d)` and
`(n2f)`. These pass through, without change, a number that is exactly of the
kind asked for, otherwise they convert the input into the requested output.

Conversion to a fixed or an integer will lose fractional bits, they will be
discarded and not rounded. A double made a float is the float nearest it.

## Numeric functions

### Basic arithmetic

These operate on any number type and return the same number type.

`(+) (-) (*) (/) (%)`

### Extended arithmetic

These operate on any number type and return the same number type.

`(abs) (max) (min) (neg) (sqrt) (sign) (pow)`

These operate on a fixed, a double or a float, and return the same type.

`(recip) (sin) (cos) (frac) (floor) (ceil)`

These operate on a double or a float, and return the same type. Of a fixed
they are an error, fixed point is what drawing works in.

`(exp) (log) (tanh)`

`(quant double tolerance)` rounds a double to the nearest whole number of the
tolerance, for comparisons of doubles.

### Bitwise logical

These operate on any number type and return a num integer.

`(logand) (logior) (logxor)`

### Bitwise shifts

These operate on any number type and return a num integer.

`(>>) (>>>) (<<)`

### Comparison

These operate on any number type and return `:t | :nil`.

`(/=) (<) (<=) (=) (>) (>=)`

### `root.inc` extras

`(neg?) (pos?) (odd?) (even?) (lognot) (log2) (ntz) (nto) (nlz) (nlo)`

## Vectors

Vectors are created with the `(nums ...)`, `(fixeds ...)`, `(doubles ...)` or
`(floats ...)` functions. The first three do not care what kind of number they
are given ! They just pack the `num_value` fields of the input parameters into
a typed array, a word an element. `(floats ...)` keeps each as a float, an element of 32
bits, 4 bytes, as a GPU buffer has them, and gives each back as a
`:float`. `(nums-convert)` makes a vector of one kind from one of another,
converting each element.

The operations on them are native methods of their classes, each over the
whole vector in one call, with an optional output vector:

`(nums-add) (nums-sub) (nums-mul) (nums-div) (nums-mod) (nums-min) (nums-max)
(nums-abs) (nums-scale) (nums-sum) (nums-dot) (nums-add-scaled) (nums-convert)`

`(fixeds-frac) (fixeds-floor) (fixeds-ceil)`

`(doubles-quant)` and the 4 by 4 matrix multiplies of `lib/math/matrix.inc`
are of exactly a `:doubles`.

The `lib/math/vector.inc` library provides a host of functions on vectors, from
simple operations such as `(vector-add ...)` to complex ones like
`(vector-dist-to-line ...)`, and constructor macros for common types,
`(Vec3-q ...)`, `(Vec4-q ...)`, `(Vec3-d ...)`, `(Vec4-d ...)` etc.

All base vector operations take an optional output vector. This avoids a memory
allocation and can provide faster operation. But be aware that the given
optional output vector will be the returned value from the call, you may mutate
an input vector !

Temp vector constants are defined in the `lib/math/vector.inc` file, such as
`+fixeds_tmp3`, `+fixeds_tmp4`, `+doubles_tmp3`, `+doubles_tmp4`, `+fixeds_zero3`,
`+fixeds_zero4`, `+doubles_zero3` and `+doubles_zero4`.

## The maths beneath

The methods of the classes do their sums by the functions of `sys/math`, each
of a value in a register, given back in one: `q_` of fixed point, `d_` of a
double and `f_` of a single. The shader compiler calls the same ones from the
code it makes. They all keep to one set of what they may trash, `:r0` to `:r2`
and `:d0` to `:d3`, `sys/math/trashes.inc`, so what calls them keeps what it
wants in any other register, and `lint` says of one that goes outside it.
