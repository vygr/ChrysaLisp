# Iteration and Sequences

This document covers the topic of iteration of sequences and the functions
available to operate on sequences of elements. There are several ChrysaLisp
built in types that inherit from `class/seq`, this class defines an interface
to extract individual elements from a sequence, discover the length, cut and
splice elements and slices of said sequences together and find the index of an
element in a sequences of those elements.

These built in interface features are used by higher level functions to expose
and extend them to Lisp level code.

## Sequences

Classes that inherit from `class/seq`:

* `class/array`
* `class/list`
* `class/nums`
* `class/fixeds`
* `class/reals`
* `class/str`
* `class/sym`

```lisp
(list 1 2 3 4)
(list "a" "b")
(array 5 6 7 8)
"ABCDEFabcdef"
'qwerty
```

To discover the length of a sequences, use the `(length)` function:

```lisp
(length (list 1 2 3))
3
(length "ABCDEF")
6
```

Extraction of a single element with `(elem)`. Negative indexes mean to index
from the back of the sequence ! Very useful !

```lisp
(elem 1 "abcd")
"b"
(elem -2 (array 9 8 7))
7
```

Note that negative indexes start at -1 representing the element that would be 1
off the end of the sequence, so -2 means the final elements index. There is a
good reason for this as will be illustrated with the `(slice)` function below.

Extracting a 'slice' of a sequence with `(slice)`:

```lisp
(slice 0 3 "ABCDEF")
"ABC"
(slice 1 -2 "ABCDEF")
"BCDE"
```

Splice sequences together by using `(cat)`:

```lisp
(cat (list 1 2 3) (list 5 6 7))
(1 2 3 5 6 7)
(cat "ABC" 'def "qwerty")
"ABCdefqwerty"
(cat (fixeds 1.5 6.7 8.9) (fixeds 5.6 1.23 9.01 8.9))
(1.50000 6.69999 8.89999 5.59999 1.22999 9.00999 8.89999)
(apply cat (list "a" "b" "c"))
"abc"
```

Search for an element with `(find)` and `(find-rev)`:

```lisp
(find "a" "defopqaui")
6
(find 5 (array 1 2 3 4 5))
4
(find "a" "def")
nil
(find 'a (list 'd 't 'y 'a 'j 'k 'd))
3
(find-rev 'd (list 'd 't 'y 'a 'j 'k 'd))
6
```
