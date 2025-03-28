# Iteration, Predication and Sequences

This document covers the topic of iteration of sequences and the functions
available to operate on sequences of elements. There are several ChrysaLisp
built in types that inherit from `class/seq`, this class defines an interface
to extract individual elements from a sequence, discover the length, cut and
splice elements and slices of said sequences together and find the index of an
element in a sequence of those elements.

These built in interface features are used by higher level functions to expose
and extend them to Lisp level code.

## Sequences

Classes that inherit from `class/seq`:

* class/array

* class/list

* class/nums

* class/fixeds

* class/reals

* class/str

* class/sym

* gui/path

```vdu
(list 1 2 3 4)
(list "a" "b")
(array 5 6 7 8)
"ABCDEFabcdef"
'qwerty
```

To discover the length of a sequence, use the `(length seq)` function:

```lisp
(length (list 1 2 3))
```

```lisp
(length "ABCDEF")
```

Extraction of a single element with `(elem-get seq idx)`. Negative indexes mean
to index from the back of the sequence ! Very useful !

```lisp
(elem-get "abcd" 1)
```

```lisp
(elem-get (array 9 8 7) -2)
```

Note that negative indexes start at -1 representing the element that would be 1
off the end of the sequence, so -2 means the final elements index. There is a
good reason for this as will be illustrated with the `(slice)` function below.

Extracting a 'slice' of a sequence with `(slice seq start end)`:

```lisp
(slice "ABCDEF" 0 3)
```

```lisp
(slice "ABCDEF" 1 -2)
```

Splice sequences together by using `(cat seq ...)`:

```lisp
(cat (list 1 2 3) (list 5 6 7))
```

```lisp
(cat "ABC" 'def "qwerty")
```

```lisp
(cat (fixeds 1.5 6.7 8.9) (fixeds 5.6 1.23 9.01 8.9))
```

```lisp
(apply cat (list "a" "b" "c"))
```

Search for an element with `(find elem seq [idx])` and `(rfind elem seq
[idx])`, they return `:nil` if the element is not found.

```lisp
(find "a" "def")
```

```lisp
(find "a" "defopqaui")
```

```lisp
(find 5 (array 1 2 3 4 5))
```

```lisp
(find 'a (list 'd 't 'y 'a 'j 'k 'd))
```

```lisp
(rfind 'd (list 'd 't 'y 'a 'j 'k 'd))
```

## Implicit indexing and slicing

Special functions are provided that mirror the binding actions available via
the `(bind)` call. `(first seq)`, `(second seq)`, `(third seq)`, `(last seq)`,
`(rest seq)` and `(most seq)`.

You can view theses as an error free way to get elements and slices. They
return `:nil` or the empty slice if the element or slice range is unavailable.

More detail is provided on these and function binding options in the
`binding.md` document.

## Iteration

You can iterate over sequences or slices of sequences, forwards or backwards by
use of the `(each! lambda seqs [start end])` function. You provide a function
that will be called for the group of elements from each index position.
`(each)` and `(reach)` are macros that assume the index values cover the
full extent of the sequence and take the sequence list as arguments rather than
an explicit list.

Any elements over the minimum length of the given sequences are ignored.

The function being called can access the current index value by use of the
'(!)' function ! Very useful !

```vdu
(each! print (list '(1 2 3) "ABC" (array 7 8 9 0)))
```

```vdu
1A7
2B8
3C9
```

```vdu
(each! (# (print (+ %0 %1))) (list '(1 2 3) '(7 2 4)))
```

```vdu
8
4
7
```

```vdu
(each (const print) "ABCDEF" "123456")
```

```vdu
A1
B2
C3
D4
E5
F6
```

```vdu
(each! print (list "ABCDEF" "123456") -2 1)
```

```vdu
E5
D4
C3
B2
```

## Predication

You can predicate over sequences or slices of sequences, forwards or backwards
by use of the `(some! lambda seqs [mode start end])` function. You provide a
function that will be called for the group of elements from each index
position, you can decide if it'll exit if that function returns a `:nil` or
not. `(some)` `(every)` `(notany)` and `(notevery)` are macros that assume the
index values cover the full extent of the sequence and set the break out
option, plus take the sequence list as arguments rather than an explicit list.

The break out value is returned ! This means you can use these functions as a
breakable for loop or a search loop !

The function being called can access the current index value by use of the
'(!)' function ! Very useful !

```lisp
(some! = (list '(1 2 3) '(5 6 3)))
```

```lisp
(some (# (if (eql %0 "a") (!))) "defhqaio")
```

```lisp
(some > (array 1 2 3) (array 5 6 7))
```

```lisp
(some < (array 1 2 3) (array 5 6 7))
```

```lisp
(every < (array 1 2 3) (array 5 6 7))
```

## Map and Reduce

You can iterate over sequences or slices of sequences, forwards or backwards,
while collecting the results of a function that will be called with the grouped
elements from each index position, by use of the `(map! lambda seqs [list start
end])` function. `(map)` and `(rmap)` are macros that assume the index
values cover the full extent of the sequences and take those sequences as
arguments rather than an explicit list.

Reduction, with `(reduce! lambda seqs init [start end])`, transforms sequences
or slices of sequences by providing an accumulated item along with each grouped
elements to the function you provide. The output of that function becomes the
item for the next iteration and is the returned result. `(reduce)` and
`(rreduce)` are macros that assume the index values cover the full extent of
a sequence and take that sequence as an argument, they allow an optional
initial item.

The function being called can access the current index value by use of the
'(!)' function ! Very useful !

```lisp
(map + '(1 2 3) '(6 7 8) '(1 7 6))
```

```lisp
(rmap + '(1 2 3) '(6 7 8) '(1 7 6))
```

```lisp
(map str '(1 2 3) '(6 7 8) '(1 7 6))
```

```lisp
(reduce + '(1 2 3))
```

```lisp
(reduce push "ABCD" (list))
```

```lisp
(rreduce push "ABCD" (list))
```

## Arrays

Arrays are sequences that also allow for writing to elements with the
`(elem-set array index val)` function. They can be wrapped by the `(dim (nums x
y z ...) array)` function to allow dimensioned access to the underling array
with `(dim-set array (nums x y z ...) val)` and `(dim-get array (nums x y z
...))`.

They can be acted on as a stack by use of `(push array val ...)` and `(pop
array)`. These functions act on the array 'in place', existing references to
the array are still valid but the contents will be mutated !

Filtering, with `(filter-array lambda array)`, transforms an array by producing
an array of all the elements that pass the filter test function.

Arrays can be cleared using `(clear array ...)` and the capacity set with `(cap
length array ...)`. These functions act on the arrays 'in place', existing
references to the arrays are still valid but the contents will be mutated !

Classes that inherit from `class/array`:

* class/list

* class/nums

* class/fixeds

* class/reals

* gui/path

Writing of a single element with `(elem-set)`. Negative indexes mean to index
from the back of the sequence ! Very useful !

```lisp
(defq a (list "a" "b" "c"))
(elem-set a 1 "z")
a
```

```lisp
(defq a (array 9 8 7))
(elem-set a -2 10)
a
```

Writing and reading of single elements using `(dim-get)` and `(dim-set)`.

```lisp
(defq d (dim (nums 2 2) (defq a (list 0 1 2 3))))
(dim-get d (nums 0 1))
```

```lisp
(defq d (dim (nums 2 2) (defq a (list 0 1 2 3))))
(dim-set d (nums 0 1) "z")
a
```

```lisp
(defq d (dim (nums 2 2) (defq a (list 0 1 2 3))))
(dim-set d (nums 1 1) "x")
a
```

Pushing and popping elements.

```vdu
(defq a (list 0 1 2 3))
(while (defq e (pop a)) (print e))
```

```vdu
3
2
1
0
```

```lisp
(defq a (list))
(push a 6 8 "z" "y" 'q)
```

```vdu
(defq a (list 'q 'y 'z 8 6))
(while (defq e (pop a)) (print e))
```

```vdu
6
8
z
y
q
```

Filtering and reversing.

```lisp
(filter-array (# (< %0 3)) '(0 1 2 3 4 5 6 7 8 9))
```

```lisp
(filter-array odd? (nums 3 4 5))
```

```lisp
(reverse '(0 1 2 3 4 5 6 7 8 9))
```

```lisp
(reverse (path 1.0 2.0 3.0 5.5))
```

```lisp
(reverse "hi there!")
```
