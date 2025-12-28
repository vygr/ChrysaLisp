# ChrysaLisp Native Acceleration Guide

This is a detailed breakdown of the native code acceleration used in the
Mandelbrot and Raymarch applications. This guide explains how to write `lisp.vp`
files, handle floating-point mathematics in the Virtual Processor (VP), and
integrate them into ChrysaLisp applications.

## Analyzing Mandelbrot and Raymarch

In ChrysaLisp, performance-critical code can be offloaded from the interpreter
to the Virtual Processor (VP) assembly. These files are typically named
`lisp.vp`. They define native functions that can be called directly from Lisp
code via the Foreign Function Interface (FFI).

This guide analyzes `apps/mandelbrot/lisp.vp` and `apps/raymarch/lisp.vp` to
demonstrate register management, floating-point math, constant loading, and SIMD
operations.

### 1. The Anatomy of a Native Function

A native function is defined using `def-func` and ended with `def-func-end`. The
entry point expects arguments in specific registers (Standard ABI: `:r0` =
`this`, `:r1` = `args`).

#### Basic Structure

```vdu
(def-func 'apps/mandelbrot/depth)
    ; 1. Register Definition
    (vp-rdef (this args cnt ox0 oy0))       ; Integer/Pointer registers
    (vp-fdef (x0 y0 xc yc x2 y2 four two))  ; Floating-point registers

    ; 2. Entry and Setup
    (entry `(,this ,args))

    ; 3. Argument Validation
    (errorif-lisp-args-sig 'error :r1 2)    ; Ensure 2 arguments type tested

    ; 4. Argument Unpacking
    (vp-push `(,this))  ; Save 'this' if needed
    ; Extract arguments from Lisp list into registers
    (list-bind-args :r1 `(,ox0 ,oy0) '(:real :real) `(,x0 ,y0)) 

    ; ... Logic ...

    ; 5. Return Construction
    (call :num :create `(,cnt) '(:r1))      ; Wrap result in Lisp Num object
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

**Example from Raymarch:**

```vdu
; Define integer registers (pointers, counters)
(vp-rdef (this args ray_org ray_dir consts cnt trunc len max_len ...))

; Define float registers (math operands)
(vp-fdef (p0 p1 p2 flen fmax_len fmin_dist fmarch_factor ...))
```

*Note: The compiler allocates registers from the pool. You do not need to
manually manage `:r3` vs `:r4` unless interfacing with specific ABI calls.*

### 3. Handling Constants in Native Code

Unlike immediate integers, floating-point constants cannot be embedded directly
into arithmetic instructions. They must be stored in a data section and loaded
into registers.

#### Step 1: Define the Constants Data Block

At the end of the function (before `def-func-end`), `fn-const` and `fn-consts`,
define a label and the raw representation of the floating-point numbers for you.

```vdu
    (vp-align +long_size)
(vp-label 'fn_consts)
    ; n2r converts a number to the platform's Real format (double)
    ; fn-consts generates the hex mapping table
    (vp-long (n2r 4.0) (n2r 2.0) ...) 
```

#### Step 2: Load the Address

Use `vp-lea-p` (Load Effective Address - Program) to get the pointer to the
constants block.

```vdu
; 'cnt' is a register defined in vp-rdef used here as a base pointer
(vp-lea-p 'fn_consts cnt) 
```

#### Step 3: Load Fields into Registers

Use the `load-fields` macro to offset from the base pointer and load specific
values into `vp-fdef` registers.

```vdu
; Load 4.0 into register 'four' and 2.0 into register 'two'
(load-fields cnt
    (fn-consts (n2r 4.0) (n2r 2.0)) ; The definition to calculate offsets
    `(,four ,two))                  ; The destination registers
```

### 4. Floating Point Arithmetic

ChrysaLisp VP assembly uses a specific suffix for floating-point operations,
usually `_ff`.

**Common Instructions (Mandelbrot Example):**

* **Move:** `(vp-cpy-ff x2 xc)` (Copy float `x2` to `xc`)

* **Add:** `(vp-add-ff y2 temp)` (`temp += y2`)

* **Subtract:** `(vp-sub-ff y2 xc)` (`xc -= y2`)

* **Multiply:** `(vp-mul-ff two yc)` (`yc *= two`)

*   **Comparison:**

    ```vdu
    (vp-cpy-ff x2 temp)
    (vp-add-ff y2 temp)
    (breakif `(,temp >= ,four)) ; Break loop if temp >= 4.0
    ```

### 5. Advanced: SIMD in Raymarch

The Raymarch app (`apps/raymarch/lisp.vp`) demonstrates `vp-simd`, a powerful
macro that unrolls operations to process vectors (X, Y, Z) in parallel. While
VP64 is scalar, this macro generates the sequence of scalar instructions
automatically, making code cleaner.

**Example from `apps/raymarch/lisp.vp`:**

```vdu
; Multiply p0, p1, p2 by p0, p1, p2 (Square them)
(vp-simd vp-mul-ff `(,p0 ,p1 ,p2) `(,p0 ,p1 ,p2))

; Add p1 to p0
(vp-add-ff p1 p0)

; Add p2 to p0 (Now p0 = x^2 + y^2 + z^2)
(vp-add-ff p2 p0)

; Square Root
(vp-sqrt-ff p0 p0)
```

### 6. Integration: Linking to Lisp

To make these functions available to the high-level Lisp interpreter:

1. **JIT Compile:** In the app script (e.g., `apps/mandelbrot/child.lisp`),
   compile the VP file.

    ```vdu
    (jit "apps/mandelbrot/" "lisp.vp" '("depth"))
    ```

2. **Define FFI:** Bind the native function name to a Lisp symbol.

    ```vdu
    ; Format: (ffi "path/to/func_name" lisp-symbol-name)
    (ffi "apps/mandelbrot/depth" depth)
    ```

3. **Call:** Use it like a standard Lisp function.

    ```vdu
    ; (depth x0 y0)
    (write-char reply (depth cx cy))
    ```

### 7. Full Workflow Example: Mandelbrot Depth

Here is the breakdown of the inner loop of the Mandelbrot set calculator
(`apps/mandelbrot/lisp.vp`).

1. **Signature**: Expects `x0` and `y0` (Reals). Returns `cnt` (Integer).

2. **Setup**: Zero out `cnt`, `xc`, `yc`, `x2`, `y2`.

3. **Loop**:

    * Check escape condition (`x^2 + y^2 >= 4`).

    * Calculate imaginary part: `y = 2*x*y + y0`.

    * Calculate real part: `x = x^2 - y^2 + x0`.

    * Update squares: `x2 = x*x`, `y2 = y*y`.

    * Increment `cnt`.

    * Loop until `cnt == 255`.

4. **Result**: Convert integer `cnt` to a Lisp `Num` object and return.

```vdu
(loop-start)
    ; Check Exit Condition (x2 + y2 >= 4)
    (vp-cpy-ff x2 temp)
    (vp-add-ff y2 temp)
    (breakif `(,temp >= ,four))

    ; y = 2 * x * y + y0
    (vp-mul-ff two yc)
    (vp-mul-ff xc yc)
    (vp-add-ff y0 yc) ; Note: In source, this add happens implicitly in SIMD logic or via accumulation

    ; x = x2 - y2 + x0
    (vp-cpy-ff x2 xc)
    (vp-sub-ff y2 xc)
    (vp-add-ff x0 xc)

    ; Update squares
    (vp-simd vp-cpy-ff `(,xc ,yc) `(,x2 ,y2))
    (vp-simd vp-mul-ff `(,x2 ,y2) `(,x2 ,y2))

    (vp-add-cr 1 cnt)
(loop-until `(,cnt = 255))
```

### Summary of Best Practices

1. **Use `vp-rdef`/`vp-fdef`**: Never hardcode register names (e.g., `:r5`,
   `:f2`) inside the logic logic. Use named variables.

2. **Argument Binding**: Use `list-bind-args` or `array-bind-args` to
   efficiently unpack Lisp data structures into registers.

3. **Constant Tables**: Group all floating-point constants at the end of the
   function and load them once at the start using `vp-lea-p` and `load-fields`.

4. **Error Safety**: Always use `errorif-lisp-args-sig` to verify input types
   before processing.