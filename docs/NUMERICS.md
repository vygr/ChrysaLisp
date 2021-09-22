# Numbers and Vectors

ChrysaLisp supports a variety of number formats. Integers, fixed point and
reals. Functions are provided to convert between these formats.

Vectors are arrays of these primitives. There are functions that operate on
every member of the array in one call.

## Numbers

### Num

These are 64 bit integer numbers. They are the same bit size as a VP register.
The VP class, `class/num/`, holds an integer value in the `num_value` field.
These are referred to as `boxed` integers in other languages.

### Fixed

These are `16.16` fixed point numbers. The VP class, `class/fixed/`, holds a
fixed value in the `num_value` field. In certain operations due to the VP
registers being 64 bits in size they are actually `48.16` format. Most of the
Canvas class drawing operations take Fixed numbers for coordinates, plus their
internal operations are conducted in Fixed format.

The CScript compiler can operate on this format directly using the `*>` and
`</` operators. These operators represent a multiply folowed by an arithmetic
shift right by `+fp_shift` bits and a shift left by `+fp_shift` bits followed
by a divide.

### Real

These are a floating point number format. The VP class, `class/real/`, holds an
real value in the `num_value` field. Not IEEE, but a compromise format for fast
operations on integer only machines. They are a `32.32` mantisa.exp format.

Zero is represented as integer 0, and negative and positive numbers pass the
same tests as integers do.

The mantissa is a signed 32 bit twos compliment value. The exponent is also a
signed 32 bit twos compliment value.

## Conversions

There are three conversion functions built in. `(n2i)`, `(n2f)` and `(n2r)`.
These pass through, without change, the same type number, otherwise they
convert the input into the requested output.

Conversion to a format with lower precision will loose fractional bits, they
will be discarded and not rounded.

## Numeric functions

### Basic arithmetic

These operate on any number type and return the same number type.

`(+) (-) (*) (/) (%)`

### Extended arithmetic

These operate on any number type and return the same number type.

`(abs) (max) (min) (neg)`

These operate on any fractional type and return the same number type.

`(recip) (sin) (cos) (frac) (floor)`

### Bitwise Logical

These operate on any number type and return a num integer.

`(logand) (logior) (logxor)`

### Bitwise Shifts

These operate on any number type and return a num integer.

`(>>) (>>>) (<<)`

### Comparison

These operate on any number type and return `t | nil`.

`(/=) (<) (<=) (=) (>) (>=)`

## Vectors

Vectors are created with the `(nums ...)`, `(fixeds ...)` or `(reals ...)`
functions. These functions do not care what numeric data format is given ! They
just pack the `num_value` fields of the input parameters into a typed array.

The `lib/math/vector.inc` library provides a host of functions to operate on
vectors. From simple operations such as `(vec-add ...)` to complex operations
like `(vec-distance-to-line ...)`.

Standard constructor macros are provided for common types. `(Vec3-f ...)`,
`(Vec4-f ...)`, `(Vec3-r ...)`, `(Vec4-r ...)` etc.

All base vector operations take an optional output vector. This avoids a memory
allocation and can provide faster operation. But be aware that the given
optional output vector will be the returned value from the call !

Temp vector constants are defined in the `lib/math/vector.inc` file, such as
`+fixeds_tmp3`, `+fixeds_tmp4`, `+reals_tmp3`, `+reals_tmp4`, `+fixeds_zero3`,
`+fixeds_zero4`, `+reals_zero3` and `+reals_zero4`.
