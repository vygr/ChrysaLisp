# ChrysaLisp Exception and Error Handling System

## The Core Philosophy: Prevention Over Handling

The ChrysaLisp exception system is a direct manifestation of its overarching
engineering philosophy: "Well, Don't Do That Then!"

In traditional operating systems and language runtimes, error checking is
ubiquitous. Functions at all levels of the stack redundantly validate
parameters, check bounds, and assert state. ChrysaLisp rejects this approach for
production environments, viewing it as an unacceptable drag on raw performance.

The system is built on a strict division of responsibility:

* **The Inner Core:** Virtual Processor (VP) assembler routines and low-level
  data structures (the inner core) perform absolutely *zero* error checking.
  They assume the data they are given is perfectly valid. This allows critical
  paths, like `:hmap` lookups or vector math, to run at maximum possible speed
  without branching overhead.

* **The FFI Boundary:** Error checking is pushed as far up the call stack as
  possible, specifically to the Lisp binding FFI functions (the `lisp_`
  methods).

* **The Application Layer:** Application developers are entirely responsible for
  validating user-facing inputs.

The exception system (`catch` and `throw`) and all FFI-level type checking are
designed strictly as **development and testing tools**. They exist to help the
programmer prove their logic is flawless before deployment.

Once a ChrysaLisp system is compiled for release, the entire error-checking
infrastructure ceases to exist in the generated binary.

## Build Modes and VP Assembler Macros

The presence of the exception and validation system is dictated by the global
`*build_mode*` variable during OS and library compilation.

There are three primary build modes:

* **`*build_mode* 0` (Release):** All error checking, type validation, and
  signature verification are completely stripped from the final compiled code.
  The system runs at absolute maximum performance. Supplying invalid arguments
  to a function in this mode will likely crash the node or corrupt memory.

* **`*build_mode* 1` (Debug):** The standard development mode. FFI functions
  validate argument counts, types, and sequence bounds before passing data to
  the inner core. If a violation occurs, an error object is thrown.

* **`*build_mode* 2` (Validate):** Enables aggressive, deep-level runtime
  validation. This includes expensive checks like verifying stack pointer
  boundaries via `(call :sys_task :stack)` upon entry to functions, ensuring no
  stack overflows occur during heavy recursion or deep call chains.

To accomplish this seamlessly at the VP assembler level, ChrysaLisp uses
specific C-Script/Lisp macros defined in `lib/asm/code.inc`:

* `(errorcase ...)`: Emits the enclosed code block only if `*build_mode* >= 1`.

* `(validatecase ...)`: Emits the enclosed code block only if `*build_mode* >= 2`.

* `(noterrorcase ...)`: Emits the enclosed code block only if `*build_mode* <= 0`.

For example, a typical Lisp FFI binding in the source code looks like this:

```vdu
(def-method :seq :lisp_elem)
    (entry :seq :lisp_elem '(:r0 :r1))
    
    (errorif-lisp-args-sig 'error1 :r1 2)
    
    ; ... payload extraction ...
    
(errorcase
    (gotoif '(:r11 < 0) 'error2)
    (gotoif '(:r11 >= :r1) 'error2))
    
    (call :seq :ref_elem '(:r0 :r11) '(_ :r1))
    ; ...
```

In Release mode, the `errorif-lisp-args-sig` (which wraps an `errorcase`) and
the bounds checking logic silently vanish during the build process, leaving only
the raw parameter extraction and the high-speed `:seq :ref_elem` call.

## Lisp Level Exceptions: Catch and Throw

At the Lisp scripting level, exceptions are managed using the `catch` and
`throw` primitives.

### Throwing an Error

Errors are triggered manually via the `throw` function:

```vdu
(throw msg obj)
```

* **`msg`:** A string containing a human-readable description of the error
  (e.g., "Invalid argument type!").

* **`obj`:** The payload object. This can be any ChrysaLisp object the developer
  wishes to attach for debugging context. Frequently, FFI functions will pass
  the entire `args` list so the developer can inspect exactly what caused a
  signature mismatch.

Under the hood, `throw` directly calls the VP method `:lisp :repl_error`, which
constructs the internal error object and immediately halts current execution
flow.

### Catching an Error

The `catch` function is used to trap thrown errors and conditionally recover
from them:

```vdu
(catch form eform)
```

* The system first evaluates `form`.

* If `form` returns a normal object, `catch` completes and returns that object.

* If `form` results in an `:error` object being thrown, `catch` intercepts it.

* It temporarily binds the captured `:error` object to the special symbol `_`
  within the local lexical scope.

* It then evaluates the `eform` (the error handler form).

* **Resolution:** If `eform` evaluates to a "truthy" value (anything other than
  `:nil` or another `:error`), the exception is considered successfully handled,
  and `catch` returns that truthy value.

* **Rethrowing:** If `eform` evaluates to `:nil` or generates a new error, the
  `:lisp :lisp_catch` method gracefully allows the *original* error to continue
  bubbling up the call stack to the next handler, or ultimately to the REPL.

## The Anatomy of the Error Object

When an exception is thrown, the `:lisp :repl_error` method instantiates a
highly detailed `+error` object (inheriting from `+obj`).

This object is designed to capture the exact state of the runtime at the moment
of failure. It is defined in `class/error/struct.inc` and contains the following
fields:

* **`description`:** A string pointer to the human-readable `msg` provided by
  the throw.

* **`object`:** A pointer to the `obj` payload provided by the throw.

* **`script`:** The name of the currently executing top-level script (e.g.,
  `"cmd/edit.lisp"`), pulled from `+lisp_script_name`.

* **`file`:** The specific file currently being evaluated by the stream (could
  be an imported `.inc` file), pulled from `+lisp_stream_name`.

* **`line`:** The exact line number in the `file` where the stream parser was
  executing, pulled from `+lisp_stream_line`.

* **`msg`:** An internal unsigned integer ID used to map standard FFI errors
  (like `+error_msg_wrong_types` or `+error_msg_not_valid_index`) to pre-defined
  strings in `class/error/class.vp`.

* **`frame`:** A pointer to the captured stack frame list, detailing the
  hierarchy of function calls leading to the crash.

Because ChrysaLisp uses iterative processing and heap-allocated lists rather
than deep machine recursion, capturing this context is both safe and highly
accurate.

## Stack Frame Capture

ChrysaLisp provides a dynamic, opt-in mechanism for tracking the call stack,
which is invaluable for debugging complex applications before compiling them for
release.

To enable stack tracing, an application simply imports the frames debugging
library:

```vdu
(import "lib/debug/frames.inc")
```

When this file is imported, it immediately redefines the core `defun` and
`defmethod` macros within the current compilation scope.

* The new macros silently wrap the body of every newly defined function with a
  `(stack-instrument ...)` call.

* Upon entering the function, this instrumentation pushes a string formatted as
  `"function_name(line_number)"` onto a global list bound to the symbol
  `*stack_frame*`.

* Upon exiting the function, it pops that string off the list.

If an error is thrown anywhere inside an instrumented application, the `:lisp
:repl_error` VP method executes the following logic:

* It searches the current lexical environment (`:hmap`) for the symbol
  `*stack_frame*`.

* If found, it makes a safe reference copy of that list.

* It embeds this list directly into the `frame` property of the resulting
  `:error` type object.

When the error is eventually printed to the REPL or log via `(. err :print)`, it
outputs the exact file, line number, the failing payload object, and a
beautifully formatted traceback of the Lisp call stack, making root-cause
analysis trivial.