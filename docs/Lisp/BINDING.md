# Environments and Symbols

In this document we explore the way in which symbols are bound to values during
function calls. The primitives used to achieve this and the way a user can use
them manually for their own purposes, plus some available implicit binding
calls that work in a similar manner to these function call mechanisms.

## Symbols

We already covered, in the `ENVIRONMENT.md` document, the idea of a symbol
object being bound to a value object. This was achieved by use of the `(def)`
and `(set)` family of functions. This is the most obvious way this can be done.

When a function is called you are using the same concept, but the binding of
symbols to values is done, automatically by the function call mechanism.
Internally, this is done by use of the `(apply)` and `(bind)` functions. BUT
you can have direct access to these for your own purposes.

## Apply and Bind

When you call a function, the arguments that you provide after the function
name are collected together into a list (this is the `rest` of the list) and
they are passed to the function body bound to the formal parameters that you
declared for that function.

```vdu
(defun test (a b) (print a " " b))
```

In this very simple function, we declare that it takes two arguments which we
will refer to as the parameters `a` and `b`.

Inside the function body, we can refer to the parameters and use them as we
wish. The reason we can do this is that the function call mechanism, more on
that later, allocates a new local environment in which it binds the arguments
to the parameters `a` and `b`.

```vdu
(test "Fred" "Blogs")
Fred Blogs
```

`a` was bound to "Fred" and `b` was bound to "Blogs". When the function exits,
it returns a value, the last thing that was evaluated, and then the local
environment is disposed of. So the bindings for `a` and `b` go out of scope.

When the function call was made in effect this happened, under the hood, by the
`(apply)` and `(bind)` combo, which you also have direct access to !.

```vdu
... (test "Fred" "Blogs")
(env-push)
(bind '(a b) (rest (list test "Fred" "Blogs")))
(progn ... body of test ...)
(env-pop)
```

The `(apply)` function does the `(env-push)` and the `(bind)` calls, followed
by running the body of the function as if a `(progn)`, and finally the
`(env-pop)`.

You can call the function directly with `(apply)` yourself ! Sometimes this is
very useful. If you have a sequence you wish to process, but you are not in a
situation where that can be a function call, you can still apply a function to
your sequence !

```vdu
(apply test (list "Fred" "Blogs"))
Fred Blogs
```

You may like to use `(bind)` directly as it implements a concept known as
destructuring, plus you can bind to any type of sequence not just a list ! You
can also `(apply)` any type of sequence to a function ! Whereas you cannot do
that by using a function call. Normal function calling always passes the `rest`
arguments as a list.

Destructuring can be done as follows. Just use nested list syntax for the
parameters to descend into the structure of the given arguments. If you wish to
ignore an argument the standard is to bind it to the _ symbol.

```vdu
(bind '((x y (z0 z1 z2) _)) (list (list 1 2 (list 3 4 5) 6)))
x
1
y
2
z0
3
z1
4
z2
5
```

## &optional, &rest, &most and &ignore

As well as ordinary symbol declarations for the parameters, you have control
over how binding proceeds. You have the ability to declare parameters as
optional or group certain parameters together based on their position etc.

We can declare a function as taking optional parameters. If they are given they
act just like normal parameters, if not present, they will be bound to `:nil`.

```vdu
(defun test (a &optional b) (print a " " b))
(test 1 2)
1 2
(test 3)
3 :nil
```

You can ask to take the rest of the parameters as a single list. Regardless of how many there are. If there are no more remaining, you will receive an empty list.

```vdu
(defun test (a &rest b) (print a " " b))
(test 1 2 3)
1 (2 3)
(test 3)
3 ()
```

In a similar manner to `&rest`, taking the remaining list, `&most` takes all,
but the last item. Notice ypu can also combine these various binding options !

```vdu
(defun test (&most a &optional b) (print a " " b))
(test 1 2 3)
(1 2) 3
(test 3)
(3) :nil
```

If you wish to cleanly, ignore remaining arguments, you can use `&ignore`.

```vdu
(defun test (a &optional b &ignore) (print a " " b))
(test 1 2 3 4)
1 2
(test 3)
3 :nil
(test 1 2 3 4 5 6 7 8 9)
1 2
```

Again, you can use bind directly to access this mechanism yourself.

```vdu
(bind '(x &optional y &rest z) (array 1 2 3 4 5 6 7 8 9)
x
1
y
2
z
(3 4 5 6 7 8 9)
(bind '(&most x &rest y) "abcdef")
x
"abcde"
y
"f"
```

`&optional`, `&rest`, `&most` and `&ignore` also work as expected within
destructuring bindings.

```vdu
(bind '((x1 y1) &ignore) '((0 1)(2 3)(4 5)))
x1
0
y1
1
```

## (first), (second), (third), (last), (rest) and (most)

 There also exist implicit functions that perform a similar action to these
 optional binding specifiers. They allow you to pick implicit elements from a
 sequence or implicit slices from a sequence. They don't return an error
 message, just `:nil`, or the empty sequence, if the element or range does not
 exist.

 ```vdu
(first '(1 2 3))
1
(second '(1 2 3))
2
(third '(1 2 3))
3
(last '(1 2 3 4))
4
(rest '(1 2 3 4))
(2 3 4)
(most '(1 2 3 4))
(1 2 3)
(first "")
:nil
(rest "abcdef")
"abcde"
(most "abcdef")
"bcdef"
(most "")
""
(rest "")
""
```
