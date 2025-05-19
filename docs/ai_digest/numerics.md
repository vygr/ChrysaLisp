# Numerics and Vectors in ChrysaLisp: A Detailed Overview

ChrysaLisp provides a set of numeric object types and corresponding vector
formats designed for efficiency and control, particularly with an eye towards
systems programming and performance on integer-based hardware. This document
details these numeric types—`Num` (integer), `Fixed` (fixed-point), and `Real`
(a custom floating-point representation)—along with their vector counterparts
and the broader vision for numeric computation in the ChrysaLisp ecosystem.

## Numeric Object Types

All numeric objects in ChrysaLisp are instances of VP (Virtual Processor)
classes, inheriting from the base `obj` class and more specifically from the
`num` class (for scalar numbers).

**`Num` (Integer)**

* **Description:** Represents 64-bit signed integer numbers. This is the
fundamental integer type and aligns with the VP register size.

* **VP Class:** `class/num/class.inc` and `class/num/class.vp`.

* **Internal Storage:** The integer value is stored in the `num_value` field of
the `num` VP class instance. These are sometimes referred to as "boxed"
integers.

* **Lisp Representation:**

    * During `(read)`, any sequence of digits not containing a decimal point
    (`.`) is parsed into a `num` object.
    
    * Radix can be specified with prefixes: `0b` for binary, `0x` for
    hexadecimal, and `0o` for octal (though octal support isn't explicitly
    mentioned in the provided docs, it's a common Lisp convention that might be
    present).
    
    * `num` objects parsed during `(read)` are interned using a global hash
    table, similar to symbols, for efficiency (as implied by the `:intern`
    method in `num.md`).

* **Constants:** Useful constants like `+min_long`, `+max_long`, `+min_int`,
`+max_int` are defined in `class/lisp/root.inc` (mentioned in `lisp.md`) and
relate to the range of values representable.

**`Fixed` (Fixed-Point)**

* **Description:** Represents fixed-point numbers in a `48.16` format. This
means 48 bits are used for the integer part and 16 bits for the fractional
part, providing a balance between range and precision for integer-based
arithmetic.

* **VP Class:** `class/fixed/class.inc` and `class/fixed/class.vp`. It inherits
from `num`.

* **Internal Storage:** The fixed-point value is stored in the `num_value`
field (inherited from the `num` class), scaled appropriately.

* **Lisp Representation:**

    * During `(read)`, any sequence of digits containing a decimal point (`.`)
    is parsed into a `fixed` object. Radix prefixes (`0b`, `0x`) can also be
    used.
    
    * `fixed` objects parsed during `(read)` are also interned.

* **Usage:** Extensively used by the `Canvas` class drawing operations for
coordinates, and its internal operations are conducted in this format
(`numerics.md`).

* **CScript Integration:** The CScript compiler supports direct operations on
`fixed` values using the `*>` (fixed-point multiply) and `</` (fixed-point
divide) operators.

    * `*>`: Represents a multiplication followed by an arithmetic shift right
    by `+fp_shift` (16) bits.
    
    * `</`: Represents a shift left by `+fp_shift` (16) bits followed by a
    division.
    
    (As described in `numerics.md` and `cscript.md`).

* **VP DSL:** An extensive set of VP-level DSL macros and functions for
fixed-format vector math is available in `sys/math/class.inc`, heavily used by
`gui/canvas/` drawing functions (`numerics.md`).

* **Constants:**

    * `+fp_shift` (16), `+fp_int_mask`, `+fp_frac_mask` (for separating integer
    and fractional parts).
    
    * Pre-calculated fixed-point values for Pi: `+fp_2pi`, `+fp_pi`, `+fp_hpi`
    (half Pi), `+fp_rpi` (reciprocal of Pi) are defined in
    `class/lisp/root.inc` (mentioned in `numerics.md`).

**`Real` (Custom Floating-Point)**

* **Description:** Implements a custom floating-point number format designed to
be efficient on integer-only machines. **It is explicitly *not* an IEEE 754
standard floating-point type.** This choice prioritizes performance and control
within the ChrysaLisp VP environment, which currently focuses on integer
operations.

* **VP Class:** `class/real/class.inc` and `class/real/class.vp`. It inherits
from `fixed`.

* **Internal Storage:** The `real` value is stored in the `num_value` field
(inherited from `num`). It uses a `32.32` mantissa.exponent format:

    * **Mantissa:** A signed 32-bit two's complement value.
    
    * **Exponent:** A signed 32-bit two's complement value.

* **Representation Details (`numerics.md`):**

    * Zero is represented as the integer `0`.
    
    * Negative and positive numbers pass the same comparison tests as integers.

* **VP Implementation (`numerics.md`, referencing `sys/math/class.vp`):** Basic
operations on `real` numbers involve a sequence of steps:

    1. **Unpack:** Separate the mantissa and exponent.
    
    2. **Align Mantissas:** Adjust mantissas based on exponent differences
    before an operation.
    
    3. **Operate:** Perform the arithmetic operation (add, mul, etc.) on the
    mantissas.
    
    4. **Normalize:** Adjust the resulting mantissa and exponent to fit the
    defined format.
    
    5. **Repack:** Combine the new mantissa and exponent back into the single
    `num_value`.

* **Constants:** Pre-calculated `real` constants are defined in
`lib/math/vector.inc` (mentioned in `numerics.md`), such as `+real_0` to
`+real_10`, `+real_pi`, etc. These are instances of the `real` VP class.

## Numeric Conversions

ChrysaLisp provides built-in functions for converting between these numeric
types (`numerics.md`):

* `(n2i num)`: Converts a number (`Num`, `Fixed`, or `Real`) to an integer (`Num`).

* `(n2f num)`: Converts a number to fixed-point (`Fixed`).

* `(n2r num)`: Converts a number to real (`Real`).

**Behavior:**

* If the input number is already of the target type, it is passed through
without change.

* Conversion to a format with lower precision (e.g., `Real` or `Fixed` to
`Num`, or `Real` to `Fixed`) will result in loss of fractional information;
bits are discarded, not rounded.

## Numeric Functions

ChrysaLisp offers a comprehensive set of functions that operate on these
numeric types (`numerics.md`, `num.md`, `fixed.md`, `real.md`). Many are
implemented as efficient VP methods.

* **Basic Arithmetic:** `(+)`, `(-)`, `(*)`, `(/)`, `(%)`. These generally
operate on any numeric type and return a result of the same type as the first
argument, or a "promoted" type if operands differ (e.g., `Fixed + Num ->
Fixed`). Division by zero will typically result in an error or a specific
representation like infinity if supported by the type (though the docs don't
detail this for `Real`).

* **Extended Arithmetic:** `(abs)`, `(max)`, `(min)`, `(neg)`, `(sqrt)`,
`(sign)`. These also operate on any numeric type and return results of the same
type.

* **Fractional Arithmetic (primarily for `Fixed` and `Real`):** `(recip)`,
`(sin)`, `(cos)`, `(frac)` (fractional part), `(floor)`.

* **Bitwise Logical (operate on the underlying integer representation):**
`(logand)`, `(logior)`, `(logxor)`. Return a `Num`.

* **Bitwise Shifts (operate on the underlying integer representation):** `(>>)`
(arithmetic right), `(>>>)` (logical right), `(<<)` (left). Return a `Num`.

* **Comparison:** `(/=)`, `(<)`, `(<=)`, `(=)`, `(>)`, `(>=)`. Operate on any
numeric type and return `:t` or `:nil`.

* **`root.inc` Extras:** `(neg?)`, `(pos?)`, `(odd?)`, `(even?)`, `(lognot)`,
`(log2)` (integer log base 2), `(pow)` (integer power), `(ntz)` (number of
trailing zeros), `(nto)` (number of trailing ones), `(nlz)` (number of leading
zeros), `(nlo)` (number of leading ones).

## Vector Types

ChrysaLisp provides typed arrays for `Num`, `Fixed`, and `Real` values,
offering efficient storage and operations on collections of these primitives.
These are `nums`, `fixeds`, and `reals` respectively.

**Introduction to Vector Types**

* **Purpose:** To store sequences of raw numeric values contiguously (or in
large contiguous chunks), rather than as lists of boxed numeric objects.

* **Performance:** This design aims for:

    * **Data Locality:** Improved cache performance compared to lists of
    pointers.
    
    * **SIMD Amenability:** Contiguous data is well-suited for Single
    Instruction, Multiple Data (SIMD) operations, a key feature of modern CPUs
    and GPUs. ChrysaLisp's design anticipates leveraging such hardware
    capabilities.
    
    * **Reduced Overhead:** Less memory overhead per element for dense
    collections compared to lists of `obj` pointers.

**Vector Creation**

* `(nums [integer ...])`: Creates a vector of `Num` values.

* `(fixeds [fixed-point-literal ...])`: Creates a vector of `Fixed` values.

* `(reals [real-literal ...])`: Creates a vector of `Real` values.

* **Type-Agnostic Packing:** These constructor functions are designed to be
flexible. They typically take Lisp numbers (which might be `num`, `fixed`, or
`real` objects) and pack their underlying `num_value` fields into the typed
array, performing necessary conversions if the input literal's type doesn't
match the vector's native type (`numerics.md`). For instance, `(nums 1.5 2)`
would likely store the integer parts.

**`lib/math/vector.inc` Library**

This library provides a host of Lisp-level functions and macros for working
with these numeric vectors (`numerics.md`):

* **Constructor Macros:** Convenience macros like `(Vec3-f x y z)` (for a
3-element `fixeds` vector), `(Vec4-r x y z w)`, etc., simplify vector creation.

* **Optional Output Vector:** Many vector operations (e.g., `vec-add`) can take
an optional output vector as an argument. If provided, the result is stored in
this vector, avoiding a new memory allocation and potentially improving
performance by reusing existing memory. If not provided, a new vector is
created.

* **Temporary Vector Constants:** The library defines constants like
`+fixeds_tmp3`, `+reals_tmp4`, `+fixeds_zero3` which are pre-allocated vectors
useful for intermediate calculations without repeated allocations.

**Vectorized Operations (VP Level)**

The real power of these typed arrays comes from their associated VP class
methods, which are implemented in highly optimized VP assembly or CScript.
These methods operate directly on the raw numeric data within the vectors.

* **VP Classes:** `class/nums/class.inc`, `class/fixeds/class.inc`,
`class/reals/class.inc`. These inherit from `array` and ultimately `seq`.

* **Methods:** These classes override methods like `:add`, `:sub`, `:mul`,
`:div`, `:dot` (dot product), `:scale` to perform element-wise or specialized
vector arithmetic.

    * For example, `nums :add` (from `nums.md`) takes two source `nums` objects
    and a destination `nums` object, performing element-wise addition.
    
    * `fixeds :dot` calculates the dot product of two `fixeds` vectors.
    
    * `reals :scale` multiplies each element of a `reals` vector by a scalar `real` value.

* **Efficiency:** These VP methods are designed for performance, directly
manipulating the contiguous data blocks without the overhead of Lisp object
dispatch for each element.

## Future Directions and Hardware Integration

ChrysaLisp's numeric and vector architecture is designed with future evolution
in mind, particularly concerning hardware acceleration:

1. **VP Extensibility for FPU:** The VP model itself is not static. It can be
extended in the future to include dedicated floating-point registers (for
single and double precision IEEE 754 numbers) and associated FPU instructions.
This would allow `Real` (or new IEEE float types) to be mapped to hardware FPU
operations where available.

2. **Integer Efficiency Focus:** The current custom `Real` format and the
emphasis on `Fixed` point arithmetic reflect a pragmatic approach to achieve
good performance on systems primarily or solely equipped with integer ALUs.

3. **Hardware Vector Primitives:** The use of contiguous typed arrays (`nums`,
`fixeds`, `reals`) makes the system inherently amenable to hardware-accelerated
vector operations. Future translators or VP extensions could map high-level
vector operations or specific VP vector instructions to:

    * CPU SIMD instructions (e.g., SSE, AVX on x86; NEON on ARM).
    
    * GPU computation (offloading large-scale vector/matrix operations to a 
      GPU).

4. **Host Interface Table for Native Vector Arithmetic:** A potential future
development could involve a "host interface table" that allows ChrysaLisp's VP
runtime to call out to highly optimized native libraries (e.g., BLAS, LAPACK,
or custom SIMD/GPU kernels) for specific vector and matrix arithmetic tasks.
This would provide a bridge to leverage existing high-performance native code.

## Conclusion

ChrysaLisp's numeric system offers a tiered approach: standard 64-bit integers,
a `48.16` fixed-point type well-suited for graphics and embedded control, and a
custom `32.32` real format for floating-point-like calculations on
integer-focused hardware. The corresponding vector types (`nums`, `fixeds`,
`reals`) are crucial for performance, providing efficient, contiguous storage
and enabling optimized low-level VP operations. This architecture balances
current needs for efficiency on diverse hardware with a clear path towards
future enhancements, including more extensive hardware FPU and vector
processing support.