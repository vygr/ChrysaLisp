# Macros

This document covers ChrysaLisp macros, what they are and what you can do with
them. What can be said about Lisp macros that has not already been said ?

When I first started ChrysaLisp I had never used Lisp in anger, just observed
from the sidelines and thought "that's neat..." . One of the biggest features
of Lisp like languages was always macros. Not the text substitution idea of
lesser languages, but the real deal.

So what is "the real deal..." ? Sounds like some Lisp guys just smack talking
other languages to me yeah ?

Well no actually ! It really is a big deal once you get into it. It's not just
that it can save you a lot of typing, which it can, but it allows you to create
your own syntax for your applications and things that I don't think I'm
qualified to talk about yet.

So let's start slow and work our way up to my level, basic Amoeba... the Lisp
Gurus are going to die laughing at my attempts to cover this subject.

## Macros are just functions

There is no difference between a macro and a function. Both are functions but a
macro is called automatically during the act of reading in your source code.

After your source code is read in by the `(read)` function, the resulting tree
is scanned by the `(macroexpand)` phase of the REPL.

Any symbol that's the first item in a list, that's bound within the current
environment, to a `macro` defined function rather than a `lambda`, has that
list substituted for the result of calling that macro function with the
parameters of the rest of the list.

That's a very cold explanation of what happens and we can probably sum it up by
saying, any source that looks like a macro function call is replaced by the
result of that macro function call before the code is actuality 'executed'.

Let's look at a very simple macro.

```vdu
(defmacro ascii-code (_)
	; (ascii-code char) -> num
	(code _))
```

This macro will substitute any occurrence of `(ascii-code "X")` with the code
value of that character. When the source code is evaluated it'll only be the
code value of "X", it won't be the char string "X". You can refer to the code
of a character in your source without having to know the code value, the macro
does the work for you.

For example these Editor key bindings:

```vdu
*key_map_control* (scatter (Fmap)
	(ascii-code "M") action-macro-record
	(ascii-code "m") action-macro-playback
	(ascii-code "/") action-comment-block
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	...
	(ascii-code "O") action-unique)
```

The `*key_map_control*` Fmap is a map of `numbers->lambda` not a map of
`string->lambda` ! This source becomes:

```vdu
*key_map_control* (scatter (Fmap)
	77 action-macro-record
	109 action-macro-playback
	47 action-comment-block
	97 action-select-all
	98 action-select-block
	...
	79 action-unique)
```

## Macros can substitute a new list

A macro can return a new list, not just an atom as in the previous example.

```vdu
(defmacro inc (_)
	; (inc num) -> num
	(list '+ _ 1))
```

This macro would turn `(inc x)` into `(+ x 1)`.

## Macros are nothing to do with `quasi-quote`

Quasi quotation is useful within macros but has nothing to do with how macros
work. An ordinary function is free to use `quasi-quote` but a macro can make
use of this as a templating system to make the macro easier to write.

Take the above example and recast it in a `quasi-quote` form.

```vdu
(defmacro inc (_)
	; (inc num) -> num
	`(+ ,_ 1))
```

This example is so simple that the benefit of adopting `quasi-quote` style is
not obvious. But stick with me as we proceed.

## Macros can extend your language

It might surprise you to learn that a lot of what you take for granted as
language constructs within ChrysaLisp are provided as macros ! They are not
actually built in primitives but macros, supplied in the `root.inc` file.

Take the `(when)` construct:

```file
class/lisp/root.inc "macro when" ""
```

This replaces your use of `(when ...)` with either an `(if ...)` or `(cond
...)` primitive. Thus providing you with a nicer syntax to express your intent.
Plus if later you decide to add more statements to the when body you don't need
to manually change the `(if ...)` yourself !

## Macros can do complex substitution

Only the result returned by a macro is substituted for the source form. Your
macro can do complex work and return the result of that work, for example the
`(or)` construct substitutes a single `(cond)` but builds the clauses before
returning it.

```file
class/lisp/root.inc "macro or" ""
```

Here the `(or ...)` is replaced with the `out` list, which starts life as
`(cond)` and for each parameter a new clause is pushed.

And how about the `(case)` construct !!! This replaces your simple use of
`(case)` with a flat map search and evaluate of a `key->result` including
checking if it can optimise the result based on the type of clauses you
provided and if you included a default.

```file
class/lisp/root.inc "macro case" ""
```

## Macros can be nested

The `(macroexpand)` function, called by the REPL, expands macros from left to
right, top to bottom.

It's perfectly OK to have nested macro forms, just remember that the lower
macros will get to 'eat' the results of the higher macros !

The resulting tree will only consist of built in primitives, atoms and none
macro function calls.

Most of the GUI widget trees, for application UIs, are constructed with nested
ui macros, for example the Pcb app UI:

```file
apps/pcb/app.lisp "*window*" ""
```

This expands into a program to build the UI tree !

## Macros can turn run time work into read time work

Macros are expanded at source read time via the REPL. This happens once before
your source starts to 'run'. Thus you can use macros to move calculations to
'read time' rather than doing them at 'run time'.

A simple example is the `(const)` macro:

```file
class/lisp/root.inc "macro const" ""
```

This macro replaces the source form with the evaluation of that form at 'read
time'.

A simple use:

```vdu
	(split data (const (cat " " (ascii-char +char_lf))))
```

Here we know the splitting chars string is going to be constant at run time,
but it's convenient to express it as the concatenation of a space and lf char.

## Macros can decorate existing functions

A great example here is the profiling library. The library is imported with:

```vdu
(import "lib/debug/profile.inc")
```

This redefines the `(defun)` and `(defmethod)` macros to collect timing
information. Your function and method existing functionality is not affected
but supplemented with wrapper code that builds and maintains the profiling
information.

```file
lib/debug/profile.inc
```

Another example of wrapping code in a decorator macro is the Editor application
`(undoable)` macro. This macro can be used to wrap any code that mutates the
text to ensure its effects can be undone.

```file
apps/edit/utils.inc "macro undoable" ""
```

And an example of the macro in use:

```file
gui/edit/lisp.inc ":reflow" ""
```

Here the paragraph reflow action mutations can be undone in a single step.

## Macros can provide type abstraction

You can write source code that allows easy switching of types by using macros
to abstract them. The Bubbles application uses this idea to allow selection
between fixed point and real number types.

Here is a section of the `apps/bubbles/app.inc` file:

These macros define an interface for creating and converting to/from a 'number'
and the actual numeric type selected.

```file
apps/bubbles/app.inc "cond" ""
```

These macros are then used instead of the raw types, for example in the
lighting function:

```file
apps/bubbles/app.lisp "lighting" ""
```

## Macros can define macros

It is possible to create a macro that creates other macros ! This is more a use
case for nested `quasi-quote` than most other macro types. I only use this
rarely myself as it can be quite tricky to get correct.

This example is from the `lib/math/vector.inc` library:

```file
lib/math/vector.inc ";macro" "undef"
```

What we are trying to achieve is a set of macros along the lines of:

```vdu
(defmacro vec-xyz ([p1] ... &optional _)
	(if _
		`(nums-xyz [,p1] ... ,_)
		`(nums-xyz [,p1] ...)))
```

This macro itself allows us to use the vector functions like so:

```vdu
(vec-add a b)
	-> (nums-add a b)
(vec-sub a b c)
	-> (nums-sub a b c)
(vec-floor a b)
	-> (fixeds-floor a b)
(vec-frac a)
	-> (fixeds-frac a)
(vec-add (vec-sub a b) (vec-sub c d) e)
	-> (nums-add (nums-sub a b) (nums-sub c d) e)
```

This use of the macro generator:

```vdu
(vec-macro "nums-add" v0 v1)
```

Produces the following macro:

```vdu
(defmacro vec-add (v0 v1 &optional _)
	(if _ `(,nums-add ~(list v0 v1) ,_) `(,nums-add ~(list v0 v1))))
```

## Macros can save you typing

OK, so we arrive at the most obvious thing. :)

In the recent Netspeed application, that times blocks of VP code instructions
to give a benchmark, we need to generate a basic block. And we need maybe
thousands of lines of actual code, not a loop, and we really don't want to type
this all in, and we want to be able to add instructions to the block with no
effort later on etc etc.

If you look in `apps/netspeed/lisp.vp` you will see this macro:

```file
apps/netspeed/lisp.vp "test-block" ""
```

What this macro does is to take a count of lines wanted, and a set of lines to
replicate, to fill that number of lines.

Here is the use case for the register ops test block:

```file
apps/netspeed/lisp.vp "(test-block " "loop"
```

This will generate for us 1000 lines of code, replicating this block as many
times as required to supply them.
