# Character classes, and membership testing.

In ChrysaLisp a character class is represented by a simple string. But these
strings are created by the `(char-class str) -> cls` function.

The result string is just a string, but the elements of that string have been
sorted into character order and any escaped characters or specifed ranges have
been expanded. Furthermore these strings are `interned` so that any duplicates
are removed.

You can use fast binary search functions to test for membership presence or to
traverse across a string based on the membership of the contained characters.
In this way it is easy to create fast and simple line parsers or tokenizers.

## Creating a character class with `char-class`

A simple example of creating a character class.

```lisp
(char-class "0-9bzxacy\\\t")
```

Notice how the characters are sorted into order ! And that the escaped
characters and ranges have been processed.

Some predefined character classes in `lib/text/charclass.inc`.

```file
lib/text/charclass.inc "(defq +char" ""
```

## Testing for membership with `bfind`

`(bfind char cls) -> idx | :nil` is a binary search of a string for a given
character. It returns the index of the found character or `:nil`.

```lisp
(bfind "C" +char_class_upper)
```

## Skipping members with `bskip`

`(bskip cls str idx) -> idx` will skip all charaters from the given index that
ARE members of the given character class.

```lisp
(bskip +char_class_lower "abcdABCD" 0)
```

If no characters can be skipped then the index is unchanged.

```lisp
(bskip +char_class_lower "abcdABCD" 4)
```

Likewise if the index hits the end of the string, the index is the length of
the string.

```lisp
(bskip +char_class_lower "abcdABCD" 8)
```

## Skipping none members with `bskipn`

`(bskipn cls str idx) -> idx` will skip all charaters from the given index that
ARE NOT members of the given character class.

```lisp
(bskipn +char_class_lower "ABCDabcd" 0)
```

```lisp
(bskipn +char_class_lower "ABCDabcd" 4)
```

## Simple line parsers

It's very easy to create fast state machines to parse lines of text given these
primatives.

For example look at the Syntax highlighting class !

```file
lib/text/syntax.inc "while" "set this"
```

Or even simpler, the options argument tokenising.

```file
lib/options/options.inc "options-split" ""
```
