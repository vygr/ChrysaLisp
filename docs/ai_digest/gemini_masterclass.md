# ChrysaLisp Masterclass: Advanced Data-Driven Dispatch

Gemini 3.1 Pro quite likes the `some` construct used in the Canvas loader
helpers !

One of the most powerful aspects of ChrysaLisp is its ability to combine the
malleability and expressiveness of high-level Lisp with the raw, mechanical
performance of C.

This masterclass explores a specific, highly optimized pattern for **data-driven
function dispatch**. We will take a standard, bulky `cond` statement and
transform it into a hyper-fast, compile-time-resolved iteration loop.

## The Problem: The Bulky `cond`

Consider a typical file loader that checks file extensions to determine which
decoding function to call. A standard approach uses a `cond` block:

```vdu
(defun canvas-info (file)
	(or (if (defq stream (file-stream file))
			(cond
				((ends-with ".cpm" file) (pixmap-cpm-info stream))
				((ends-with ".flm" file) (pixmap-cpm-info stream))
				((ends-with ".tga" file) (TGA-info stream))
				((ends-with ".svg" file) (SVG-info stream))
				((ends-with ".cwb" file) (CWB-info stream))))
		(list -1 -1 -1)))
```

While readable, this is repetitive and requires evaluating a series of
conditional branches at runtime. If we add 10 more image formats, the code
becomes unwieldy.

## The Solution: Concurrent `some` Iteration

Here is the fully optimized ChrysaLisp idiom to solve this:

```vdu
(defun canvas-info (file)
	(or (if (defq stream (file-stream file))
			(some (# (if (ends-with %0 file) (%1 stream)))
				  '(".cpm" ".flm" ".tga" ".svg" ".cwb")
				  (const `'(,pixmap-cpm-info ,pixmap-cpm-info ,TGA-info ,SVG-info ,CWB-info))))
		(list -1 -1 -1)))
```

In just four lines, we have packed an immense amount of ChrysaLisp's core
philosophy. Let's break down exactly why this works and why it is so fast.

### 1. The `some` Loop and Concurrent Iteration

In ChrysaLisp, `some` (and its imperative cousin `some!`) iterates over
sequences and returns the *first* truthy value evaluated by its lambda.

Crucially, ChrysaLisp's iterators seamlessly support **concurrent list
iteration**. By passing two lists to `some`, the lambda receives elements from
both lists simultaneously. As soon as `(ends-with %0 file)` matches, it executes
the loader `(%1 stream)`, and `some` immediately halts and returns that result.

### 2. The `#` Anaphoric Lambda

Notice the use of `#` instead of `lambda`.

```vdu
(# (if (ends-with %0 file) (%1 stream)))
```

This is the anaphoric lambda shortcut. It automatically binds incoming arguments
to the globally interned symbols `%0`, `%1`, `%2`, etc.

* `%0` maps to the current element in the first list (the string `".cpm"`).

* `%1` maps to the current element in the second list (the function pointer).

**Performance Note:** Because `%0` and `%1` are static, globally interned
symbols, they maintain a stable `str_hashslot`. This guarantees an **O(1) cache
hit** during environment lookup at runtime, making the `#` macro significantly
faster than a standard `lambda` with named parameters.

### 3. The `const` and Quasi-Quote Magic

This is where the true compile-time magic happens:

```vdu
(const `'(,pixmap-cpm-info ,pixmap-cpm-info ,TGA-info ,SVG-info ,CWB-info))
```

Why do we use `const` and quasi-quoting instead of a simple `(list ...)` or a
quoted literal `'(... )`?

1. **The `not_a_function` Trap:**

	If we used `(static-q (pixmap-cpm-info TGA-info ...))`, ChrysaLisp's `prebind`
	pass (which optimizes ASTs) would see a list and assume it is a function call.
	It would attempt to bind `pixmap-cpm-info` as a function, but leave `TGA-info`
	and the rest as raw, unresolved symbols. At runtime, attempting to execute a
	symbol via `(%1 stream)` throws a `not_a_function !` error.

2. **Compile-Time Resolution:**

   To fix this, we need the compiler to evaluate the symbols into actual
   function pointers *before* the program runs.

   * `const` tells the compiler: "Evaluate the following expression right now,
     during compilation, and bake the result directly into the binary."

   * `` `'(,A ,B) `` uses quasi-quoting `` ` `` to create a template, and
     unquoting `,` to force the compiler to look up the function pointers for
     `A` and `B` in the current REPL environment.

   * The outer `'` (quote) ensures that the resulting list of raw memory
     addresses/function objects is treated as literal data, not executable code.

## The Result

When the ChrysaLisp compiler is finished with this block, there are no runtime
symbol lookups, no `eval` overhead, and no deep conditional trees.

The binary simply contains a highly-optimized loop stepping through an array of
strings and an array of raw function pointers, executing the correct one in O(1)
time.

It is the readability and dynamism of Lisp, executing with the mechanical
efficiency of a C jump table.

Using `(const ...)` to force compile-time evaluation, combined with the
quasi-quote `` `'(,A ,B) `` to evaluate the symbols into function pointers and
return a literal list, is a masterclass in ChrysaLisp macros. It completely
bypasses the prebind AST logic by handing the compiler a fully resolved list of
function objects, while keeping the scope perfectly tight (no need for a
module-level defq). Zero runtime overhead, zero namespace pollution.
