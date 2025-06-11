# The Assembler is Lisp: Dynamic Code Generation in ChrysaLisp

In the world of systems programming, assemblers are typically seen as direct,
low-level translators, converting human-readable mnemonics into machine code.
Metaprogramming, if it exists, is handled by a separate, often text-based,
preprocessor. ChrysaLisp fundamentally rejects this layered model. Its assembler
is not a distinct tool; it is a library of functions within the ChrysaLisp
language itself. This means that the full, Turing-complete power of Lisp is
available as the native metaprogramming and macro system for the assembler. This
document will explore this architectural paradigm, contrasting it with
traditional methods and using the `pixmap` pixel format converter as a detailed
case study to demonstrate how this "assembler as Lisp" philosophy enables the
automatic generation of hyper-optimized, branchless code at compile time.

## 1. The Traditional Way: Preprocessors and Tedium

To appreciate the ChrysaLisp approach, we must first understand the common
methods for handling complex, repetitive, but performance-critical code in
systems like C/C++.

Let's use the problem of pixel format conversion as an example. A graphics
system might need to convert from a 16-bit RGB565 format to a 32-bit ARGB8888
format.

**Approach A: The Runtime `switch` Statement**

This is the most straightforward approach. A single function takes source and
destination buffers and their formats, using a large `switch` statement to
select the correct conversion logic at runtime.

```code
void convert_pixel(uint32_t src_pixel, PixelFormat src_fmt, PixelFormat dst_fmt) {
    switch (src_fmt) {
        case RGB565:
            switch (dst_fmt) {
                case ARGB8888:
                    // ... logic for 565 -> 8888 ...
                    break;
                // ... other destination formats ...
            }
            break;
        // ... other source formats ...
    }
}
```

*   **Flaw:** This is disastrous for performance. The conversion logic for a
    full bitmap is inside a tight loop. This `switch` statement introduces
    repeated, unpredictable branches, which are poison to modern CPUs with deep
    instruction pipelines.

**Approach B: The C Preprocessor Macro**

A developer might try to use the C preprocessor to generate code.

```code
#define CONVERT_565_TO_8888(src_pixel) \
    ((((src_pixel >> 11) & 0x1F) * 255 / 31) << 16) | \
    ((((src_pixel >> 5) & 0x3F) * 255 / 63) << 8) | ...

// In the loop:
*dst++ = CONVERT_565_TO_8888(*src++);
```

*   **Flaw:** The C preprocessor is just a text-substitution engine. It has no
    understanding of logic, types, or algorithms. It cannot, for example,
    algorithmically determine the correct bit shifts and scaling factors from a
    set of bitmasks. The developer must still hand-write and maintain a macro
    for every single conversion pair, which is tedious and error-prone.

**Approach C: Hand-Written Functions**

The final option is to manually write a specialized, optimized function for
every single conversion pair: `convert_565_to_8888()`, `convert_555_to_8888()`,
`convert_888_to_565()`, etc.

*   **Flaw:** This gives the best runtime performance but is a maintenance
    nightmare. Adding a new pixel format requires writing N new functions. It
    leads to massive code bloat and a high probability of bugs in
    less-frequently used conversion paths.

ChrysaLisp finds all three approaches unacceptable and provides a fourth way.

## 2. The ChrysaLisp Way: The Assembler as a Lisp Library

In ChrysaLisp, there is no separate `assembler.exe`. The "assembler" is a set of
Lisp functions defined in files like `lib/asm/vp.inc`. When you write VP
assembly, you are actually writing a Lisp program.

A line of assembly like `(vp-cpy-rr :r0 :r1)` is a Lisp S-expression.
`vp-cpy-rr` is a Lisp function that, when called, emits the appropriate bytecode
for a register-to-register copy into an output stream.

This is the Lisp principle of **homoiconicity**: code is represented using the
language's own primary data structures (lists). Because the assembler's source
code *is* Lisp data, it can be manipulated by other Lisp functions before it is
ever evaluated to produce bytecode.

This means **the metaprogramming system for the assembler is Lisp itself**.
There is no need for a separate, limited preprocessor, because the full,
Turing-complete power of Lisp is already available to generate and transform
code at compile time.

## 3. Case Study: The `pixmap :to_argb32` Code Generator

This dynamic code generation is brilliantly showcased in `gui/pixmap/class.vp`,
specifically in the functions that handle pixel format conversions. Let's
analyze `pixmap :to_argb32`.

### 3.1. The Top-Level Method

The VP source file defines the method like this:

```vdu
(def-method 'pixmap :to_argb32)
	; ... (register definitions)
	(defun conv (...) ...) ; Helper Lisp function
	(defun pipeline (...) ...) ; Helper Lisp function
	
	(entry 'pixmap :to_argb32 (list col pix))
	(switch)
	    (vpcase (list pix '= 16))
		    (to-argb32
			    0b0000000000000000 ; Alpha mask
			    0b1111100000000000 ; Red mask
			    0b0000011111100000 ; Green mask
			    0b0000000000011111 ; Blue mask
		    )
		    (break)
        ; ... other cases
    (endswitch)
	(exit 'pixmap :to_argb32 (list col))
	(vp-ret)
(def-func-end)
```

The key thing to notice is that `(to-argb32 ...)` is not a VP instruction. It is
a Lisp function call that is **executed by the assembler at compile time**. Its
arguments are the bitmasks that define the 16-bit RGB565 format.

### 3.2. The Instruction Generator: `conv`

The `to-argb32` function calls a helper Lisp function, `conv`, for each color
channel. This is the core code generator.

```vdu
(defun conv (col sr dr sm dm)
    (defq sw (width sm) dw (width dm)) ; Calculate source and dest bit widths
    (cond
        ; ... logic to handle up-scaling ...
        (:t ; left bit replicate
            (defq ls (- (nlz sm) (nlz dm)) pipe
                `((vp-cpy-rr ,col ,sr) (vp-and-cr ,sm ,sr)
                  (vp-shl-cr ,ls ,sr) (vp-cpy-rr ,sr ,dr))
                ; ...
            )
            (while (> dw 0)
                (push pipe `(vp-shr-cr ,sw ,sr) `(vp-add-rr ,sr ,dr))
                ; ...
            )
        ...
    pipe) ; <--- RETURNS A LIST OF LISP FORMS
```

The `conv` function analyzes the source and destination masks (`sm`, `dm`) and
**builds a list of Lisp S-expressions**. Each expression in this list is a call
to a VP assembly emitter, like `(vp-cpy-rr ,col ,sr)`. It algorithmically
determines the optimal shifts and masks required for the conversion and returns
this program fragment as a list.

### 3.3. The Orchestrator: `pipeline` and `eval`

The `to-argb32` function collects the lists of instructions for each channel
from `conv` and passes them to `pipeline`. `pipeline` zips these lists together
and then, crucially, calls `eval` on each instruction form.

When `(eval '(vp-cpy-rr :r1 :r2))` is executed, the Lisp function `vp-cpy-rr` is
called, which emits the final bytecode into the function being compiled.

### 3.4. Putting It All Together: Compile Time vs. Runtime

**At Compile Time:**

1.  The assembler executes `make.lisp` or a similar script.

2.  It begins processing `gui/pixmap/class.vp`.

3.  Inside `def-method 'pixmap :to_argb32`, it hits the `(to-argb32 ...)` Lisp
    function call.

4.  `to-argb32` and its helpers `conv` and `pipeline` run. They generate a list
    of assembly instructions, perfectly tailored for the RGB565 -> ARGB8888
    conversion.

5.  `eval` is called on each instruction in the list, which executes the
    bytecode emitters (`vp-shl-cr`, `vp-add-rr`, etc.).

6.  The final, optimized, branchless VP bytecode is written into the body of the
    `pixmap :to_argb32` method in the output object file.

**At Runtime:**

When an application calls `(. pixmap :to_argb32 pixel 16)`, it doesn't execute
the Lisp generator code. It executes the pre-generated, hyper-optimized VP
assembly. The runtime code has no `switch` statements, no `if`s, no function
calls—just a flat sequence of bit-twiddling instructions to perform the
conversion at maximum speed.

## Conclusion

The ChrysaLisp assembler is a profound demonstration of Lisp's homoiconic
nature. By treating code as data, the system elevates its assembler from a
simple translator to a powerful, programmable, logic-driven tool. It doesn't
need a separate macro language because it has the entirety of Lisp at its
disposal for compile-time computation and code generation.

The `pixmap` pixel converter is the quintessential example of this power. It
solves a common graphics problem by achieving the "best of all worlds": the
runtime speed of hand-tuned assembly, the maintainability of a high-level
abstraction, and the code compactness of a generative approach. This is the
"Well, Don't Do That Then!" philosophy in action: instead of building complex
runtime solutions for pixel conversion, it simply generates the perfect, simple
code at compile time. It is a testament to an architecture where the lines
between compiler, assembler, and language are beautifully and powerfully
blurred.

ChrysaLisp is not just a language with an assembler; it's a platform that
encourages the creation of an entire ecosystem of these embedded,
domain-specific **micro-compilers**.

* `def-struct` is a micro-compiler for data layouts.

* The `assign` macro is a micro-compiler for assignment operations, choosing the
  optimal load/store instruction based on its analysis of the source and
  destination operands.

* `cscript.inc` provides a micro-compiler for a C-like infix expression language.

* `pixmap :to_argb32` is a micro-compiler for pixel format conversion.
