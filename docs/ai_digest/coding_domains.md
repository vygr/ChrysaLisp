# The ChrysaLisp Coding Domains: A Programmer's Guide

While the entire ChrysaLisp operating system is written using S-expressions
(parenthesized syntax), it is a mistake to view it as a single monolithic
language. There are three distinct coding domains (or layers): **Lisp**,
**CScript**, and **VP Assembler**.

Understanding which domain you are in is critical, as valid syntax in one
domain is often undefined or functionally broken in another.

## 1. The Lisp Layer (Application Domain)

**File Extensions:** `.lisp`, `.inc`
**Execution:** Interpreted (High-Level)

This is the user-space layer where Applications (`apps/`), Commands (`cmd/`),
and high-level logic reside. It is a vector-based, functional Lisp dialect.

### Characteristics

* **No Register Access:** You cannot access CPU registers (`:r0`–`:r14`), the
  stack pointer (`:rsp`), or raw memory addresses.

* **Memory Management:** Automatic. You deal with Objects (`:str`, `:num`,
  `:list`, `:hmap`), not pointers.

* **Forbidden Includes:** You cannot import files from `lib/asm/` or
  `lib/trans/`. Macros like `vp-rdef`, `assign`, or `def-vars` do not exist
  here.

* **Interface:** You interact with the system via **FFI** (Foreign Function
  Interface) calls defined in `class/lisp/root.inc`.

### Coding Style

* **Variables:** Defined via `defq` (global/constant) or `bind` (local).

* **Control Flow:** `if`, `when`, `while`, `each!`, `map!`.

* **Example:**

	```vdu
	(defun my-app-function (list_input)
		(defq result (list))
		(each! (lambda (item)
			(if (> item 10)
				(push result (* item 2))))
			list_input)
		result)
	```

## 2. The CScript Layer (System Class Domain)

**File Extensions:** `.vp`
**Execution:** Compiled to Native Code (Intermediate-Level)

CScript is a high-level macro assembler used primarily to write the Class
Library (`class/`, `gui/`, `sys/`). It abstracts away register allocation and
stack management, looking somewhat like C logic wrapped in S-expressions.

### Characteristics

* **Variable Management:** Uses **`def-vars`** to define stack variables. You
  do not manually push/pop the stack for these variables.

* **Method Calls:** Uses `(call :class :method ...)` syntax.

	* *Crucial Distinction:* You must use the **virtual method** call (e.g.,
	  `call :list :push_back`). Do not call internal assembler macros (e.g.,
	  `class/list/push_back`) directly unless you are manually managing the
	  specific registers those macros trash.

* **Assignment:** Relies heavily on the **`assign`** macro. This is a
  pattern-matching assembler that generates moves, loads, and stores based on
  operand types.

	* Syntax: `(assign {src1, src2} {dest1, dest2})`

* **Control Flow:** Structured assembly macros: `vpif`, `else`, `loop-while`,
  `loop-start`, `break`.

### Coding Style

* **No Registers:** Avoid using `:r0` explicitly. Use named variables defined
  in `def-vars`.

* **Example:**

	```vdu
	(def-method :my_class :my_method)

		(def-vars
			(ptr this list)
			(int count))

		; Alloc stack space
		(push-scope)
		; Binds inputs to vars
		(entry :my_class :my_method {this, list})

		; Virtual call
		(call :list :get_length {list} {_, count})

		(vpif {count > 0})
			; 'assign' handles the math/move
			(assign {count + 1} {count})
		(endif)
		
		; Cleanup and return
		(exit :my_class :my_method {this})
		; Dealloc stack space
		(pop-scope)
		(return)

	(def-func-end)
	```

## 3. The VP Assembler Layer (The "Metal")

**File Extensions:** `.vp` (specifically `lib/asm/*.inc` and optimized
sections of classes)
**Execution:** Compiled to Native Code (Low-Level)

This is the raw Virtual Processor architecture. When writing at this level,
you are manually managing the register file (`:r0` through `:r14` + `:rsp`)
and the stack.

### Characteristics

* **Manual Register Allocation:** You use **`vp-rdef`** to alias symbol names
  to specific physical registers.

* **Direct Instructions:** You use opcodes like `vp-add-rr` (Register-
  Register), `vp-cpy-ir` (Indirect-Register), `vp-lea-i` (Load Effective
  Address).

* **Stack Management:** You must manually `vp-push` and `vp-pop` to preserve
  registers.

* **Templated Assignment:** You can use `assign` here, but usually with
  explicit register lists: `(assign '(:r0 :r1) '(:r2 :r3))` or via templating
  style if using `(vp-rdef)` register names.

### Mixed Domain (Advanced)

It is possible to mix CScript and VP Assembler, but it requires deep
knowledge of the compiler's register usage. See `gui/canvas/fpoly.vp`.

* CScript generally assumes it owns the register set between statements.

* If you inject raw VP instructions inside a CScript block, you must ensure
  you do not clobber registers that CScript is relying on for variable
  storage at that moment.

### Coding Style

* **Example:**

	```vdu
	; Map registers
	(vp-rdef (accum iterator end) '(:r0 :r1 :r2))
	; Zero out accum
	(vp-xor-rr accum accum)
	(loop-start)
		; Increment ptr
		(vp-add-cr 8 iterator)
		; Raw compare
		(breakif `(,iterator = ,end))
		; Raw math
		(vp-add-rr iterator accum)
	(loop-end)
	```

## 4. The Command Domain (`cmd/`)

The `cmd/` folder contains command-line utilities. While written in **Lisp
Layer** code, they operate under a specific environment constraint different
from GUI applications or System Services.

### Characteristics

* **Lifecycle:** Ephemeral. They start, process data, and terminate
  immediately.

* **I/O:** They rely on the **Pipe** object and `:stdio`.

	* Input: `(io-stream 'stdin)`

	* Output: `(io-stream 'stdout)`

* **Arguments:** Arguments are passed in via a specific `options` structure
  processing the `:stdio` args.

**Example:**

	```vdu
	(defun main ()
		(when (defq stdio (create-stdio))
			; Only allowed because we are in a cmd/ context
			(while (defq line (read-line (io-stream 'stdin)))
				(print line))))
	```

## The Golden Rules

1. **Lisp Level:** Never attempt to import `lib/asm/` files. If you need
   system functionality, check `class/lisp/root.inc` for the FFI binding.

2. **CScript Level:** Do not use `class/array/get_elem`. Use `(call :array
   :get_elem ...)` to ensure vtable lookups occur correctly, unless you are
   implementing the array class itself.

3. **VP Level:** Know your registers. `:r0` is usually the return value.
   `:r14` is often the dispatch register for method calls. Clobbering them
   breaks the world.
