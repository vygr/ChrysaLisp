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
value of that character. When the source code is evaluated it will only be the
code value of "X", it won't be the char string "X". You can refer to the code
of a character in your source without having to know the code value, the macro
does the work for you.

For example these Editor key bindings:

```vdu
key_map_control (fmap-kv
	(ascii-code "M") action-macro-record
	(ascii-code "m") action-macro-playback
	(ascii-code "/") action-comment-block
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	...
	(ascii-code "O") action-ordered-unique)
```

The `key_map_control` fmap is a map of `numbers->lambda` not a map of
`string->lambda` ! This source becomes:

```vdu
key_map_control (fmap-kv
	77 action-macro-record
	109 action-macro-playback
	47 action-comment-block
	97 action-select-all
	98 action-select-block
	...
	79 action-ordered-unique)
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
actually built in primitives but macros, supplied in the `boot.inc` file.

Take the `(when)` construct:

```vdu
(defmacro when (x &rest _)
	; (when tst body)
	(if (= (length _) 1)
		`(if ,x ~_)
		`(cond (,x ~_))))
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

```vdu
(defmacro or (&rest _)
	; (or [tst] ...) -> :nil | tst
	(defq out (list cond))
	(each! 0 -1 (lambda (&rest c) (push out c)) (list _))
	out)
```

Here the `(or ...)` is replaced with the `out` list, which starts life as
`(cond)` and for each parameter a new clause is pushed.

And how about the `(case)` construct !!! This replaces your simple use of
`(case)` with a flat map search and evaluate of a `key->result` including
checking if it can optimise the result based on the type of clauses you
provided and if you included a default.

```vdu
(defmacro case (_form &rest _body)
	; (case form [(key|(key ...) body)] ...)
	(defq _default_key :nil _default_clause :nil _atoms :t
		_map (reduce (lambda (_map (_keys &rest _clause_body))
			(unless (list? _keys) (setq _keys (list _keys)))
			(setq _clause_body (prebind (macroexpand
				(if (= (length _clause_body) 1)
					(elem-get 0 _clause_body)
					(cat '(progn) _clause_body)))))
			(or (eql :num (defq _clause_type (pop (type-of _clause_body))))
				(eql :str _clause_type) (setq _atoms :nil))
			(each! 0 -1 (lambda (_key) (cond
				((eql _key :t)
					(setq _default_key :t _default_clause _clause_body))
				(:t  (push (elem-get 0 _map) _key)
					(push (elem-get 1 _map) _clause_body)))) (list _keys)) _map)
			_body (list (list) (list))))
	(cond
		(_default_key
			(push (elem-get 1 _map) _default_clause)
			(if _atoms
				`(elem-get (or (find ,_form ',(elem-get 0 _map)) -2) ',(elem-get 1 _map))
				`(eval (elem-get (or (find ,_form ',(elem-get 0 _map)) -2) ',(elem-get 1 _map)))))
		(:t  (if _atoms
				`(if (defq ,(defq _i (gensym)) (find ,_form ',(elem-get 0 _map)))
					(elem-get ,_i ',(elem-get 1 _map)))
				`(if (defq ,(defq _i (gensym)) (find ,_form ',(elem-get 0 _map)))
					(eval (elem-get ,_i ',(elem-get 1 _map))))))))
```

## Macros can be nested

The `(macroexpand)` function, called by the REPL, expands macros in depth first
order, from left to right, top to bottom.

It's perfectly OK to have nested macro forms, just remember that the lower
macros will get to 'eat' the results of the higher macros !

`(macroexpand)` will keep looping until it finds no more macro substitutions
are possible. The resulting tree will only consist of built in primitives,
atoms and none macro function calls.

Most of the GUI widget trees, for application UIs, are constructed with nested
ui macros, for example the Pcb app UI:

```vdu
(ui-window *window* ()
	(ui-title-bar window_title "" (0xea19) +event_close)
	(ui-tool-bar main_toolbar ()
		(ui-buttons (0xe91d 0xe91e 0xea00 0xea01 0xe9ac 0xe9ad) +event_prev)
		(ui-buttons ("0" "1" "2" "3" "4") +event_show_all
			(:color (const *env_toolbar2_col*)
			:font (const (create-font "fonts/OpenSans-Regular.ctf" 20)))))
	(ui-scroll pcb_scroll +scroll_flag_both
			(:min_width 512 :min_height 256)))
```

This expands into a program to build the UI tree !

## Macros can turn run time work into read time work

Macros are expanded at source read time via the REPL. This happens once before
your source starts to 'run'. Thus you can use macros to move calculations to
'read time' rather than doing them at 'run time'.

A simple example is the `(const)` macro:

```vdu
(defmacro const (_)
	; (const form)
	(eval (macroexpand _)))
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

```vdu
(defmacro undoable (this buffer &rest _)
	`(progn
		(setd buffer (. this :get_buffer))
		(bind '(cx cy) (. this :get_cursor))
		(. buffer :push_undo
			(list :mark (defq mark (. buffer :next_mark)))
			(list :cursor cx cy))
		~_
		(. buffer :push_undo (list :mark mark))))
```

And an example of the macro in use:

```vdu
(defmethod :reflow (this)
	; (. edit :reflow) -> edit
	(undoable this :nil
		(bind '(y y1) (select-paragraph this))
		(each (lambda (line)
				(task-slice)
				(.-> buffer (:insert line) :break))
			(.-> buffer :get_syntax (:text_flow
				(split (.-> buffer (:set_cursor 0 y) (:cut 0 y1))
					(const (cat " " (ascii-char +char_lf))))
				(. buffer :get_wrap_width))))
		(bind '(x y) (. buffer :get_cursor))
		(bind '(x y) (. buffer :constrain x (inc y)))
		(.-> this (:set_cursor x y) (:set_anchor x y)))
	this)
```

Here the paragraph reflow action mutations can be undone in a single step.

## Macros can provide type abstraction

You can write source code that allows easy switching of types by using macros
to abstract them. The Bubbles application uses this idea to allow selection
between fixed point and real number types.

Here is a section of the `apps/bubbles/app.inc` file:

These macros define an interface for creating and converting to/from a 'number'
and the actual numeric type selected.

```vdu
(cond	;pick number format :t/:nil
	(:t	;reals
		(defmacro vec (&rest _) `(reals ~_))
		(defmacro i2n (_) `(n2r ,_))
		(defmacro f2n (_) `(n2r ,_)))
	(:t	;fixed point
		(defmacro vec (&rest _) `(fixeds ~_))
		(defmacro i2n (_) `(n2f ,_))
		(defmacro f2n (_) _)))
```

These macros are then used instead of the raw types, for example in the
lighting function:

```vdu
(defun lighting ((r g b) z)
	;very basic attenuation
	(defq at (/ (const (i2n box_size)) z) r (* r at) g (* g at) b (* b at))
	(+ 0xd0000000
		(<< (n2i (* r (const (i2n 0xff)))) 16)
		(<< (n2i (* g (const (i2n 0xff)))) 8)
		(n2i (* b (const (i2n 0xff))))))
```

## Macros can define macros

It is possible to create a macro that creates other macros ! This is more a use
case for nested `quasi-quote` than most other macro types. I only use this
rarely myself as it can be quite tricky to get correct.

This example is from the `lib/math/vector.inc` library:

```vdu
;macro to define macros that take optional output vector
(defmacro vec-macro (op &rest v)
	`(defmacro ,(sym (cat "vec" (slice (find "-" op) -1 op))) (~v &optional _)
		(if _ `(,,(sym op) ~(list ~v) ,_) `(,,(sym op) ~(list ~v)))))

(vec-macro "nums-add" v0 v1)
(vec-macro "nums-sub" v0 v1)
(vec-macro "nums-min" v0 v1)
(vec-macro "nums-max" v0 v1)
(vec-macro "nums-mul" v0 v1)
(vec-macro "nums-div" v0 v1)
(vec-macro "nums-mod" v0 v1)
(vec-macro "nums-abs" v)
(vec-macro "nums-scale" v s)
(vec-macro "fixeds-frac" v)
(vec-macro "fixeds-floor" v)
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
