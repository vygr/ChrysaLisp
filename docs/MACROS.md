# Macros

This document covers ChrysaLisp macros, what they are and what you can do with
them. What can be said about Lisp macros that has not allready been said ?

When I first started ChrysaLisp I had never used Lisp in anger, just observed
from the sidelines and thought "that's neat..." . One of the biggest features
of Lisp like languages was allways macros. Not the text substitution idea of
lesser languages, but the real deal.

So what is "the real deal..." ? Sounds like some Lisp guys just smack talking
other languages to me yeah ?

Well no actually ! It really is a big deal once you get into it. It's not just
that it can save you a lot of typing, which it can, but it allows you to create
your own syntax for your applications and things that I don't think I'm
qualified to talk about yet.

So let's start slow and work our way up to my level, basic Amoeba... the Lisp
Gurus are going to die laughing at my attempts to cover this subject...

## Macros are just functions

There is no difference between a macro and a function. Both are functions but a
macro is called automatically during the act of reading in your source code.

After your source code is read in by the `(read)` function, the resulting tree
is scanned by the `(macroexpand)` phase of the REPL.

Any symbol that is the first item in a list, that is bound within the current
environment to be a `macro` defined function, is substituted for the result of
calling that macro function with the paramaters of the rest of the list.

Let's look at a very simple macro.

```vdu
(defmacro ascii-code (_)
	; (ascii-code char) -> num
	(code _))
```

This macro will substitute any occurrence of `(ascii-code "X")` with the code
value of that character. When the source code is evaluated it will only be the
code value of "X" , it won't be the char string "X" . You can refer to the code
of a character in your source without having to know the code value, the macro
does the work for you.

For example these Editor key bindings:

```vdu
key_map_control (xmap-kv
	(ascii-code "M") action-macro-record
	(ascii-code "m") action-macro-playback
	(ascii-code "/") action-comment-block
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	...
	(ascii-code "O") action-ordered-unique)
```

The `key_map_control` xmap is a map of `numbers->lambda` not a map of `string->lambda` !

## Macros can substitute a new list

A macro can return a new list, not just an atom as in the previous example.

```vdu
(defmacro inc (_)
	; (inc num) -> num
	(list '+ _ 1))
```

This macro would turn `(inc x)` into `(+ x 1)`.

## Macros are nothing to do with `quasi-quote`

Quasiquotation is useful within macros but has nothing to do with how macros
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

It might suprise you to learn that a lot of what you take for granted as
langauge constructs within ChrysaLisp are provided as macros ! They are not
actually built in primatiuves, but supplied in the `boot.inc` file.

Take the `(when)` function:

```vdu
(defmacro when (x &rest _)
	; (when tst body)
	`(cond (,x ~_)))
```

This replaces your use of `(when ...)` with a `(cond)` primative. thus
providing you with a nicer syntax to express your intent.

## Macros can do complex substitution

Only the result returned by a macro is substituted for the source form. Your
macro can do complex work and return the result of that work, for example the
`(or)` construct substitutes a single `(cond)` but builds the result before
returning it.

```vdu
(defmacro or (&rest _)
	; (or [tst] ...) -> nil | tst
	(defq out (list cond))
	(each! 0 -1 (lambda (&rest c) (push out c)) (list _))
	out)
```
