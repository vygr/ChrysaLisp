# Text search and regular expressions

Chrysalisp provides class libraries for text searching, both plaintext and
regular expressions. You can access the classes if required, although this is
normally done through a set of provided functions that will build a query given
your requirements.

These classes, `Substr` and `Regexp` follow a common API defined in the search
base class, `lib/text/search.inc`.

```file
lib/text/search.inc
```

The syntax for regular expressions is similar to that used in the VIM text
editor. Escape shortcuts are provided for many common character classes. Please
refer to the `grep` command reference documentation for the complete list.

## Provided matching and searching functions

To ask the question if a pattern is present within a string you use the
`(match? str pattern) -> :nil | :t` or `(found? str pattern) -> :nil | :t`
functions. The former is for regular expressions, the latter for plaintext.

```lisp
(found? "abc xyz qwe" "xyz")
```

```lisp
(match? "abc xyz qwe" "xyz|abc")
```

If you wish to obtain a list of all the matches and sub matches, within a
string, then use the `(matches str pattern) -> slices` and `(substr str
pattern) -> slices` functions.

```lisp
(substr "abc xyz qwe abc" "abc")
```

And likewise for regular expressions:

```lisp
(matches "abc xyz 123 abc" "[x-y]+|[0-9]+")
```

Within regular expression patterns placing brackets around an expression will
return the sub matches along with the main match.

```lisp
(matches "abc xyz 123 abc 567" "(\d+)\D+(\d+)")
```

There can also be more than one match within a search.

```lisp
(matches "abc xyz 123 abc 567 xyz 123 abc 567" "(\d+)\D+(\d+)")
```

The results are slice indices that you can use to retrieve the resulting
substrings or matches from the original string.

For example, the `forward` command, work function:

```file
cmd/forward.lisp "defun work" ""
```

## Building a query

Often, you won't know whether the pattern is a regular expression or not. For
example, in the `Editor`, toggles all provided for search features, whole
words, regular expression, etc.

You can use the `(query pattern word_flag regexp_flag) -> (search pattern
meta)` function and let it build you the correct query to use, including the
correct search instance and meta data.

Here, in the `Docs` app find down action, you can see the it passes the search
settings into the query function and uses the returned engine and resulting
meta data to perform the search.

```file
apps/docs/search.inc "action-find-down" ""
```

## Greedy operators in regular expressions

The regular expression wildcard repeat operators are what is known as greedy.
This means they will return the longest match that they can. For example, here
this search finds everything between the furthest separated `-`.

```lisp
(matches "the -dog sat- on -the cat-" "-(.+)-")
```

Sometimes in these scenarios, you can use a combination of matches in order to
achieve what you desire. This example is from the pipe command line splitting
function. It wants to split a string on the pipe symbols, but not if they are
within quoted arguments, so it uses a combination of finding all the quotes,
and then only splitting where the pipe matches are not within pairs of these.

```file
lib/task/pipe.inc "pipe-split" ""
```
