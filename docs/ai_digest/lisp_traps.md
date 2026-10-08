# Lisp Traps

Things that catch out someone new to writing ChrysaLisp Lisp, of either
kind. Each has a good reason behind it, the speed and the size of the system
are bought with them, and each is small once known. Most give an error that
points somewhere else, or no error at all, which is why they are here.

All but the two that say otherwise are checked by
`tests/core/test_traps.lisp`, so this file can not say what is no longer so.
If a trap is fixed its test fails, and it comes out of here.

## Names

### A `+name` is a constant, and its value is put in the code

A symbol that starts with `+` is prebound, the value goes into the code in
place of the name when the function is made. If the value is a list, that
list is then run as a form.

```lisp
(defq +sizes (list 1 2 3))
(defun f () (first +sizes))
(f) ; -> (lambda ([arg ...]) body) not_a_function
```

Quote it twice, or give a list that is not a constant a `*name*`. The same
goes for `(const (list ...))`, use `(static-q (...))` for a list that is
made once.

```lisp
(defq +sizes ''(1 2 3))
(defun f () (first (static-q (1 2 3))))
```

### A key of `case` that starts with `+` is the value, not the symbol

```lisp
(case op (+ :plus) (- :minus) (:t :other))
```

With `op` the symbol `+` that is `:other`. With `-` it is `:minus`. For
symbols of operators use `cond` with `eql`, or `find` on a quoted list.

### A name in a parameter list must not be a macro

A parameter list is macro expanded with the rest. A parameter called `bits`
or `when`, both macros, is seen as a call of the macro. The error is
`symbol_not_bound`, and says nothing of the parameter.

```lisp
(defun f (bits) bits) ; no
(defun f (nbits) nbits)
```

If a short, common name gives an error that makes no sense, try another
name.

### Scope is dynamic

A name that is not a local of a function is looked for in the function
that called it, and the one that called that. So a function can read, and
`setq`, a local of its caller.

```lisp
(defun helper () (setq total 99))
(defun f () (defq total 1) (helper) total)
(f) ; -> 99
```

That is used on purpose all over the system, it is how a callback sees the
variables of the code it was given by. The trap is the helper that means a
variable of its own and forgot the `defq`, with a name like `out`, `idx` or
`total`, which every caller has. Always `defq` what is yours. And when a
variable of yours changes under you, look at what you called.

### In a module, call only what is above you

Inside `(env-push)` ... `(env-pop)` a function is bound to the functions
defined before it, as it is made. A call to one defined later is looked for
by name when it runs, and by then the module's names have gone, all but the
exported ones.

```lisp
(env-push)
(defun first-one (n) (second-one n)) ; symbol_not_bound when called
(defun second-one (n) n)
(export-symbols '(first-one))
(env-pop)
```

Put a helper above what uses it. A function that calls itself is fine if it
is exported, and is not if it is not. `((const f) ...)` inside `f` does not
help, `f` is not bound yet.

### Your names are live while the assembler runs

Code that makes VP, inside `within-compile-env`, shares names with the
assembler. A function of yours called `vp-something`, or a constant like
`+vp_rregs`, can be the assembler's own. Give anything that is live then a
prefix the assembler does not use. There is no test of this one.

## Values

### Only `:nil` is false

`0`, `""` and an empty list are all true.

```lisp
(if (list) :yes :no) ; -> :yes
```

Ask `(empty? seq)` or `(nempty? seq)`, and `(= n 0)`.

### `:nil` is a symbol, and a symbol is a sequence

```lisp
(first (list))  ; -> :nil
(first :nil)    ; -> ":", which is true
```

So `(first (first found))`, with nothing found, is `":"` and not `:nil`,
and a loop that waits for it to be true has it at once. Ask `(empty? found)`
before taking it apart.

### A quoted list is the one list, every time

```lisp
(defun f () (defq out '()) (push out 1))
(f) ; -> (1)
(f) ; -> (1 1)
```

The quoted list is part of the code, and `push` changes it. Use `(list)`
for a list that will be changed.

### A copy of a list is not a copy of what is in it

`(cat lst)` is a new list of the same elements. A list inside it is the
same list in both.

### A str mailed on the one node is the str itself

A message to a task on the same node is not copied, what arrives is the
object that was sent. If the one who gets it writes into it, the sender's
is written into, and so is any list or set the sender had put it in.

```lisp
(defq mbox (mail-mbox) sent (str-alloc 8))
(mail-send mbox sent)
(set-long (mail-read mbox) 0 5)
(get-long sent 0) ; -> 5
```

The Net service kept the address of each peer in a set, and mailed the
same str to the link, which cuts it at the `:` where it lies. The key was
never found again, and every beacon made another link. Send `(cat msg)` if
you keep what you send, or if you do not know what the other end does.

### Numbers do not mix

A number with a point, `1.5`, is a fixed, 16 bits each side of the point.
A number without is an integer. A real is a third kind, made with `(n2r)`.
Arithmetic on two kinds is an error, `wrong_types`, which is easy to find.
A compare is not.

```lisp
(+ 1 1.5)  ; wrong_types
(= 1 1.0)  ; -> :nil, and no error
```

Change one, `(n2f 1)`, `(n2i 1.5)`, `(n2r 1.5)`.

A fixed is cut to its 16 bits as it is read, `0.001` is `0.00099`. For a
real that is exact use `(str-to-real "0.001")`.

### `num?`, `fixed?` and `real?` say how deep, and a real is all three

Each gives `:nil` or a number, and the number can be `0`, which is true. A
real is a fixed, and a fixed is a num. To tell them apart ask `real?`
first, then `fixed?`, then `num?`.

### `find` with a str in a str looks for its first character

```lisp
(find "elx" "hello") ; -> 1
```

`find` takes an element, and the element of a str is a character. For a str
inside a str use `(substr text pattern)`, or `(split)` the text and `find`
a whole line in the list.

### `sort` needs to be told how, for anything but strs

`(sort (list 3 1 2))` is an error. `(sort lst (const -))` for numbers. It
sorts the list it is given, it does not make another.

## Errors

### The usual build checks, a release build does not

`make all boot` is the checked build. A wrong number of args, a wrong type,
an index past the end, are each an error with the name of the function. A
release build, `make it`, the snapshot, and the emulator's image, has none
of those checks, and the same mistake there stops the node or worse. Nor
does a `throw` from a function given to `(pipe-run)` get out of it there.
Work in the checked build. `docs/ai_digest/exceptions.md`.

### A handler that gives `:nil` passes the error on

```lisp
(catch (work) (progn (tidy-up) :nil)) ; the error carries on up
(catch (work) (progn (tidy-up) :t))   ; the error ends here
```

Both are wanted, the trap is a handler whose last form happens to be `:nil`,
a `print` is not, a `setq` to `:nil` is.

### Let go of nothing that is still open

An error that goes up through a function takes its locals with it. If one
was a `Pipe` to a command that will not end by itself, the task waits on it
for ever, and the error is never seen. `(pipe-run)` closes its pipe as an
error passes. If you hold a `Pipe` yourself, `catch`, `(. pipe :close)` or
`(. pipe :abort)`, and give `:nil` to pass the error on.

A pipe to commands that end when their stdin does is safe to let go of,
its stdin is ended first.

### A test must not be made from the clock

A number that is different every run, the time say, finds a fault one run
in many, on one machine in three. Reading `1791408183000000.0` once stopped
an x86_64 node, and nothing else. If a task of a test just vanishes on one
kind of CPU, think of a divide, and of the size of the numbers. That one is
fixed, and has tests of its own, the habit is what is left.
