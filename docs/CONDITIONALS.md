# Conditionals

In this document we cover the ChrysaLisp conditional statements. The types
available, plus the tricks you can pull with them.

Note that true means *not :nil* and that's significant. Any value other than
`:nil` is *not :nil* !

### if

`(if tst form [else_form])` is a VP native code implemented conditional
statement. It is a simple way to make a test evaluation and evaluate a form if
the test is true and optionally evaluate a separate form if the test evaluates
to `:nil`.

```vdu
(if (= a 0)
	(print "a is 0")
	(print "a is not 0"))
```

The return value of the `(if ...)` is the value of the form evaluated, or `:nil`
if the form is empty.

### when

`(when tst body)` is a way to evaluate a body of statements if the test clause
is true. Not just a single form but an implicit `(progn ...)`.

```vdu
(when (> z (const (i2n focal_len)))
	(defq v (vec x y z) w (/ hsw z) h (/ hsh z))
	(bind '(sx sy sz) (vec-add v (vec-scale (vec-norm
		(vec-add v (vec-sub (elem-get +dlist_light_pos dlist) v))) r)))
	(defq x (+ (* x h) hsw) y (+ (* y h) hsh) r (* r h)
		sx (+ (* sx h) hsw) sy (+ (* sy h) hsh))
	(push out (list (vec-n2f x y z) (vec-n2f sx sy) (n2f r)
		(lighting c z) (lighting (const (vec-i2n 1 1 1)) z))))
```

### unless

`(unless tst body)` is the opposite to `(when ...)`. It just evaluates the test
form and evaluates the body if the result is `:nil`.

```vdu
(unless (eql (defq file (elem-get -2 route)) ".")
	(def (defq node (Button)) :text file :border 0)
	(. node :connect (inc (get :action_event this)))
	(. root :add_child node))
```

### while

`(while tst body)` is a VP native code implemented conditional statement. It is
like `(when ...)` but it will loop while the test clause is true.

```vdu
(while (< b e)
	(push l b)
	(setq b (+ b s)))
```

### until

`(until tst body)` is like `(unless ...)` but will loop until the test clause
is true.

```vdu
(until (def? :is_window window)
	(setq window (penv window)))
```

### cond

`(cond [(tst [body])] ...)` is a VP native code implemented conditional
statement. It takes a list of test forms and following bodies to evaluate if
that test form evaluates as true.

Only the first test that evaluates as true has its body evaluated. Also if the
body is empty the true value returned by the test is the returned value from
the `(cond ...)` statement ! That can be very useful. If no test clause proves
to be true then `:nil` is returned from the `(cond ...)`.

So let's see a few examples:

```vdu
(cond
	((= a 0)
		(print "a is 0"))
	((= a 1)
		(print "a is 1"))
	((= b 0)
		(print "b is 0"))
	((= b 1)
		(print "b is 1"))
	(:t  (print "no test is none :nil!")))
```

Here the tests using symbol `a` have precedence over those with symbol `b` and
the final clause will happen if no other clause.

```vdu
(cond
	(a :nil)
	(:t))
```

Here if `a` is true then return `:nil` else return `:t`. So a simple logical not.

It is possible to evaluate a test and bind the result to a symbol that's then
used in the remaining clauses ! These are not static tests based on the value
of the symbols used at the entry to the `(cond ...)` !

```vdu
(cond
	((= (defq id (getf msg +ev_msg_target_id)) +event_close)
		;close app
		)
	((= id +event_min)
		;minimize app
		)
	((= id +event_max)
		;maximize app
		)
	(:t  ;ui event for window....
		(. *window* :event msg)))
```

### case

`(case key (k0 body) ((k1 k2) body) ... [(:t ...)])` is a variant of cond that
acts like a fast switch. The `key` form is evaluated and a jump is then made to
the matching body clause, or if no match, the optional `:t` clause.

```vdu
(case state
	(:symbol
		(cond
			((defq ink (get (sym (apply cat token)) (get :keywords this)))
				;present in keyword map
				(push col_list ink))
			((eql (elem-get 0 token) "+")
				;is a constant symbol
				(push col_list (get :ink_constants this)))
			((eql (elem-get 0 token) "*")
				;is a global symbol
				(push col_list (get :ink_globals this)))
			((eql (elem-get 0 token) "-")
				;is a negative number
				(push col_list (get :ink_numbers this)))
			(:t  ;default text color)
				(push col_list (get :ink_text this)))))
	((:string1 :string2)
		(push col_list (get :ink_strings this)))
	(:number
		(push col_list (get :ink_numbers this)))
	(:keysym
		(push col_list (get :ink_keysyms this)))
	(:comment
		(push col_list (get :ink_comments this)))
	(:text
		(push col_list (get :ink_text this))))
```

## Tricks with logical statements

`(and ...)` and `(or ...)` can be use as conditional operations as they are
implemented as if a ladder of `(if ...)` by their respective macros.

### and

For the `(and ...)` the forms will be evaluated one by one and will exit if
that clause evaluates as `:nil`. Therefore you can use it to execute a form only
if all the preceding test clauses prove to be true !

```vdu
(and (= (getf msg +ev_msg_type) +ev_type_mouse)
	(/= 0 (getf msg +ev_msg_mouse_buttons))
	(setq mouse_down (getf msg +ev_msg_mouse_buttons)))
```

In this example the mouse_down symbol is only set if the previous 2 clauses
prove to be true.

### or

For the `(or ...)` the forms will be evaluated one by one and will exit if that
clause proves to be true. Therefore you can use it to execute a form only if
all the preceding test clauses prove to be false !

```vdu
(. farm :each (lambda (key val)
	(setq working (or working (. val :find :job)))))
```

In this example the working symbol is set if working is not already set, and if
not then it is set to the value of the `(. val :find :job)` statement. So this
Farm child entry loop will only set the value of working once then quickly skip
all the next clauses.
