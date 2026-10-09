# ChrysaLisp Native Acceleration Guide

This is a detailed breakdown of the native code acceleration used in the
Mandelbrot application. This guide explains how to write
`lisp.vp` files, handle floating-point mathematics in the Virtual Processor
(VP), and integrate them into ChrysaLisp applications.

## Analyzing Mandelbrot

In ChrysaLisp, performance-critical code can be offloaded from the
interpreter to the Virtual Processor (VP) assembly. These files are typically
named `lisp.vp`. They define native functions that can be called directly
from Lisp code via the Foreign Function Interface (FFI).

This guide analyzes `apps/science/mandelbrot/lisp.vp` to demonstrate
register management, floating-point math, constant loading, and SIMD
operations.

### 1. The Anatomy of a Native Function

A native function is defined using `def-func` and ended with `def-func-end`.
The entry point expects arguments in specific registers (Standard ABI: `:r0`
= `this`, `:r1` = `args`).

#### Basic Structure

```vdu
(def-func (path-to-absolute "./shade"))
    ; 1. Register Definition
    ; Integer/Pointer registers
    (vp-rdef (this args cnt top pal consts t0 t1 t2 col))
    ; Floating-point registers
    (vp-fdef (x0 y0 xc yc x2 y2 dx dy one two bail f0 f1 f2 f3))

    ; 2. Entry and Setup
    (entry `(,this ,args))

    ; 3. Argument Validation
    (errorif-lisp-args-sig 'error :r1 4)    ; Ensure 4 arguments type tested

    ; 4. Argument Unpacking
    (vp-push `(,this))  ; Save 'this' if needed
    ; Extract arguments from Lisp list into registers
    (list-bind-args :r1 `(,t0 ,t1 ,top ,pal) '(:real :real :num :str)
        `(,x0 ,y0 ,top ,pal))

    ; ... Logic ...

    ; 5. Return Construction
    (call :num :create `(,col) '(:r1))      ; Wrap result in Lisp Num object
    (vp-pop :r0)                            ; Restore 'this'

    (exit `(,this ,args))
    (vp-ret)

    ; Error handling boilerplate...
(def-func-end)
```

### 2. Register Management (`vp-rdef` & `vp-fdef`)

ChrysaLisp provides macros to map symbolic names to physical registers
automatically.

* **`vp-rdef` (General Purpose):** Maps symbols to integer/pointer registers
  (`:r0` - `:r14`).

* **`vp-fdef` (Floating Point):** Maps symbols to floating-point registers
  (`:f0` - `:f15`).

**Example from Mandelbrot:**

```vdu
; Define integer registers (pointers, counters)
(vp-rdef (this args cnt top pal consts t0 t1 t2 col))

; Define float registers (math operands)
(vp-fdef (x0 y0 xc yc x2 y2 dx dy one two bail f0 f1 f2 f3))
```

*Note: The compiler allocates registers from the pool. You do not need to
manually manage `:r3` vs `:r4` unless interfacing with specific ABI calls.*

### 3. Handling Constants in Native Code

Unlike immediate integers, floating-point constants cannot be embedded
directly into arithmetic instructions. They must be stored in a data section
and loaded into registers.

#### Step 1: Define the Constants Data Block

At the end of the function (before `def-func-end`), `fn-const` and
`fn-consts`, define a label and the raw representation of the
floating-point numbers for you.

```vdu
    (vp-align +long_size)
(vp-label 'fn_consts)
    ; n2r converts a number to the platform's Real format (double)
    ; fn-consts generates the hex mapping table
    (vp-long (n2r 2.0) (n2r 65536) ...) 
```

#### Step 2: Load the Address

Use `vp-lea-p` (Load Effective Address - Program) to get the pointer to the
constants block.

```vdu
; 'consts' is a register defined in vp-rdef used here as a base pointer
(vp-lea-p 'fn_consts consts) 
```

#### Step 3: Load Fields into Registers

Use the `load-fields` macro to offset from the base pointer and load specific
values into `vp-fdef` registers.

```vdu
; Load 2.0 into register 'two' and 65536.0 into register 'bail'
(load-fields consts
    (fn-consts +real_2 (n2r 65536)) ; The definition to calculate offsets
    `(,two ,bail))                  ; The destination registers
```

There are only 16 float registers, and `shade` has more constants than
that. A constant is loaded where it is needed, into whichever register is
free there, and the base pointer is kept in a register of its own for the
whole of the function.

### 4. Floating Point Arithmetic

ChrysaLisp VP assembly uses a specific suffix for floating-point operations,
usually `_ff`.

**Common Instructions (Mandelbrot Example):**

* **Move:** `(vp-cpy-ff x2 xc)` (Copy float `x2` to `xc`)

* **Add:** `(vp-add-ff y2 f0)` (`f0 += y2`)

* **Subtract:** `(vp-sub-ff y2 xc)` (`xc -= y2`)

* **Multiply:** `(vp-mul-ff two yc)` (`yc *= two`)

*   **Comparison:**

    ```vdu
    (vp-cpy-ff x2 f0)
    (vp-add-ff y2 f0)
    (gotoif `(,f0 >= ,bail) 'outside) ; Leave the loop if f0 >= 65536.0
    ```

### 5. Advanced: SIMD in Mandelbrot

The Mandelbrot function uses `vp-simd`, which applies one operation across
lists of registers. While VP64 is scalar, it generates the sequence of
scalar instructions for you, which keeps the code short. A list shorter
than the longest is made up to that length with its last register.

**Examples from `apps/science/mandelbrot/lisp.vp`:**

```vdu
; Zero cnt, then convert it into each of xc, yc, x2, y2, dx, dy (all 0.0)
(vp-xor-rr cnt cnt)
(vp-simd vp-cvt-rf `(,cnt) `(,xc ,yc ,x2 ,y2 ,dx ,dy))

; Add x0 to xc and y0 to yc
(vp-simd vp-add-ff `(,x0 ,y0) `(,xc ,yc))

; Copy xc, yc to x2, y2, then square them
(vp-simd vp-cpy-ff `(,xc ,yc) `(,x2 ,y2))
(vp-simd vp-mul-ff `(,x2 ,y2) `(,x2 ,y2))
```

### 6. Integration: Linking to Lisp

To make these functions available to the high-level Lisp interpreter:

1. **JIT Compile:** In the app script (e.g.,
   `apps/science/mandelbrot/child.lisp`), compile the VP file.

    ```vdu
    (jit *app_root* "lisp.vp" '("shade"))
    ```

2. **Define FFI:** Bind the native function name to a Lisp symbol.

    ```vdu
    ; Format: (ffi "path/to/func_name" lisp-symbol-name)
    (ffi (cat *app_root* "shade") shade)
    ```

3. **Call:** Use it like a standard Lisp function.

    ```vdu
    ; (shade x0 y0 top palette) -> -1 | argb
    (defq d (shade x0 y0 top +mandel_pal))
    ```

### 7. Full Workflow Example: Mandelbrot Shade

Here is the breakdown of the Mandelbrot set calculator
(`apps/science/mandelbrot/lisp.vp`). One call works out the whole colour of
one pixel, so the Lisp that calls it only has to store the answer.

1. **Signature**: Expects `x0` and `y0` (Reals), `top`, how many turns of the
   loop a point is given (Integer), and the palette (String, an int for each
   colour). Returns the colour of the point, or -1 if it is inside the set.

2. **Early out**: A point of the main heart, or of the disc to its left, is
   inside the set, a few multiplies say so, and it would else run the loop
   to the top.

3. **Setup**: Zero out `cnt`, `xc`, `yc`, `x2`, `y2`, and the derivative
   `dx`, `dy`.

4. **Loop**:

    * Check escape condition (`x^2 + y^2 >= 65536`). The far escape is what
      makes the depth come out smooth.

    * Step the derivative: `d = 2*z*d + 1`.

    * Calculate imaginary part: `y = 2*x*y + y0`.

    * Calculate real part: `x = x^2 - y^2 + x0`.

    * Update squares: `x2 = x*x`, `y2 = y*y`.

    * Increment `cnt`. Every 64 turns, if the derivative is very big, scale
      it and the `one` that is added to it down, before a Real overflows.

    * Loop until `cnt == top`, and that is a point inside the set.

5. **Smooth depth**: How far through its last turn the point got out is a
   log2 of a log2. There is no log instruction, so the first is a count of
   halvings and a curve for what is left, and the second is the same curve
   again. The depth is `cnt` and that fraction, with no steps in it.

6. **Colour**: The root of the depth, times a rate, is an index into the
   palette, masked to its size, as integer instructions.

7. **Light**: `z` over its derivative points straight out from the set, the
   way the ground slopes at that pixel. Its dot product with a light from
   the top left makes a multiplier, a little over or under 256.

8. **Result**: Red, green and blue are each multiplied by the light, shifted
   down and kept to 255, put back together with a full alpha, and returned
   as a Lisp `Num` object.

```vdu
(loop-start)
    ; Check Exit Condition (x2 + y2 >= 65536)
    (vp-cpy-ff x2 f0)
    (vp-add-ff y2 f0)
    (gotoif `(,f0 >= ,bail) 'outside)

    ; d = 2 * z * d + 1
    (vp-simd vp-cpy-ff `(,xc ,xc) `(,f1 ,f2))
    (vp-simd vp-mul-ff `(,dx ,dy) `(,f1 ,f2))
    (vp-simd vp-mul-ff `(,yc ,yc) `(,dy ,dx))
    (vp-sub-ff dy f1)
    (vp-add-ff dx f2)
    (vp-simd vp-cpy-ff `(,f1 ,f2) `(,dx ,dy))
    (vp-simd vp-add-ff `(,f1 ,f2) `(,dx ,dy))
    (vp-add-ff one dx)

    ; y = 2 * x * y + y0
    (vp-mul-ff two yc)
    (vp-mul-ff xc yc)

    ; x = x2 - y2 + x0
    (vp-cpy-ff x2 xc)
    (vp-sub-ff y2 xc)

    ; the two adds, x0 to xc and y0 to yc
    (vp-simd vp-add-ff `(,x0 ,y0) `(,xc ,yc))

    ; Update squares
    (vp-simd vp-cpy-ff `(,xc ,yc) `(,x2 ,y2))
    (vp-simd vp-mul-ff `(,x2 ,y2) `(,x2 ,y2))

    (vp-add-cr 1 cnt)
    ; ... every 64 turns, bring the derivative down if it is very big ...
(loop-until `(,cnt = ,top))
```

`vp-simd` takes constants as well as registers, the three channels of the
colour are done as one line each:

```vdu
(vp-simd vp-and-cr (list 0xff) `(,col ,t1 ,t2))
(vp-simd vp-mul-rr `(,t0) `(,col ,t1 ,t2))
(vp-simd vp-shr-cr (list 8) `(,col ,t1 ,t2))
(vp-simd vp-min-cr (list 0xff) `(,col ,t1 ,t2))
```

### Summary of Best Practices

1. **Use `vp-rdef`/`vp-fdef`**: Never hardcode register names (e.g., `:r5`,
   `:f2`) inside the logic. Use named variables.

2. **Argument Binding**: Use `list-bind-args` or `array-bind-args` to
   efficiently unpack Lisp data structures into registers.

3. **Constant Tables**: Group all floating-point constants at the end of the
   function, take their address once at the start using `vp-lea-p`, and load
   them with `load-fields`, at the start, or where each is needed if there
   are more of them than registers.

4. **Error Safety**: Always use `errorif-lisp-args-sig` to verify input types
   before processing.
