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

### A local with the name of a function, alone in an `and` or an `or`

`(and a b)` is made into `(condn (a) (b))`, and `(or a b)` into `(cond (a)
(b))`. Each term is then a list of one thing, and a list is a call. When
the function is made, a name at the head of a list that is a function is
bound to it. So a local called `num`, `str`, `list`, `first`, any function,
is called, with nothing, in place of being looked at.

```lisp
(defun f (&optional num) (and num (> num 0)))
(f) ; -> (> num num ...) wrong_types, num was called, gave 0, and went on
```

At the top level, not in a function, it works, nothing is bound there. Give
the local another name, `cnt`, or write the term as a test, `(and (num? cnt)
...)`.

### `setd` can not tell `:nil` from not given

```lisp
(defun f (&optional listen) (setd listen :t) listen)
(f :nil) ; -> :t
```

`setd` gives a default to a parameter that is `:nil`, and one that was
passed as `:nil` is `:nil`. So an optional that defaults to `:t` can never
be turned off. Name it the other way round, so that not given, and `:nil`,
both mean the usual thing.

### A `#` with no `%0` in it takes no arguments

```lisp
(filter (# (/= 0 (% (!) 3))) seq) ; wrong_num_of_args
(pipe-run "echo x" (# :nil))      ; the same
```

`(# ...)` makes a lambda of as many parameters as the highest `%n` in its
body. One that uses only `(!)`, the index, or nothing of what it is given,
has none, and is then called with one. Write `(lambda (&) ...)` for a
function that ignores the one thing it is given, and `(lambda (&ignore)
...)` for one that ignores all it is given, however many.

Not `(lambda (_) ...)`. The `_` is a name like any other, it is bound each
time the function is called, to a thing that is never looked at, and it
has a slot in the hash of the environment to be found in. An `&` in a list
of parameters, and in a `bind`, is a place that takes a thing and binds
nothing.

```lisp
(map (lambda (&) 0) seq)            ; a 0 for each
(each (lambda ((name & size)) ...)) ; the first and the third of each
(bind '(a &ignore) seq)             ; the first, and no more is looked at
```

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

### `(cat lst)` is a new list of the same things, `(copy lst)` is new all the way down

`(cat lst)` is a new list of the same elements. A list inside it is the
same list in both. If the whole of it is to be copied, `(copy lst)` does,
it copies the list and every list in it, however deep.

```lisp
(defq a (list 1 (list 2 3)) b (cat a) c (copy a))
(push (second b) 9)
(second a) ; -> (2 3 9), b has the same inner list
(second c) ; -> (2 3), c has one of its own
```

Which object a thing is, is what `(weak-ref)` gives, a number, so the
two can be told apart for sure: of a form of 8 lists, not one list of its
`copy` has the number of a list of the original, and all but the top one
of its `cat` have.

It is lists that `copy` copies. What is in one that is not a list, a str,
a `nums`, an `array`, is the same one in both, and `copy` of one of those
by itself gives it back. And it is for plain lists: a map or an object is
a list underneath, and `copy` of one is a plain list of what it is made
of, no longer a map. An environment has `(env-copy)`.

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

### A type is a chain of classes, and `x?` asks if a class is in it

`(type-of)` gives the classes a thing is, the one it inherits from first
and its own last. A real is a fixed, and a fixed is a num.

```lisp
(type-of 3)           ; -> (:num)
(type-of 1.5)         ; -> (:num :fixed)
(type-of (n2r 1.5))   ; -> (:num :fixed :real)
(type-of (list))      ; -> (:seq :array :list)
(type-of (Fmap))      ; -> (:seq :array :list :hmap)
```

A predicate with one `?` asks if a thing is of a class or of any class
built on it, is its class in that chain. The answer is true or it is
`:nil`. What comes back when it is true is not `:t`, it is a number as it
happens, `0` for one, and `0` is true, only `:nil` is false. It is an
answer to test, not a value to read.

```lisp
(num? (n2r 1.5))      ; -> true, a real is a num
(list? (Fmap))        ; -> true, a map is a list
(if (num? 3) "yes")   ; -> "yes", though what (num? 3) gave was 0
```

A predicate with two, `list??`, asks if it is that class itself, the last
of the chain, and no other. There is one for each class that others are
built on, `array??`, `list??`, `num??`, `fixed??`, `nums??`, `fixeds??`
and `str??`.

```lisp
(list?? (list))       ; -> :t
(list?? (Fmap))       ; -> :nil
(str? 'name)          ; -> true, a symbol is a str
(str?? 'name)         ; -> :nil
(num?? 1.5)           ; -> :nil, it is a fixed
```

So to tell a real from a fixed from a num, ask `real?` first, then
`fixed?`, then `num?`. And where a list is to be told from the things that
are built on one, a map, a class of your own, it is `list??` that is
wanted. `(. obj :type_of)` is the chain with the Lisp classes on the end
as well, `(:seq :array :list :hmap :View :Label :Button)`.
`docs/ai_digest/type_system.md` has the whole of it.

### `trim` wants its characters in order

```lisp
(trim "  ab \n" " \t\r\n")              ; nothing is trimmed
(trim "  ab \n" (char-class " \t\r\n")) ; -> "ab"
```

The characters to trim are a class, searched by halves, so they have to be
in order, and a str typed as it comes to mind is not. `(char-class)` makes
one, sorted, and with ranges, `"a-z0-9"`. Given one that is not in order,
`(trim)`, `(bskip)` and the like miss some of it, and say nothing.

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
