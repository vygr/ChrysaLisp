# Regexp

## Search

```code
(Regexp [num_buckets]) -> regexp
```

### :compile

```code
(. regexp :compile pattern [whole_words]) -> :nil | meta

the pattern is made group 0. For whole words the word break tests
go outside that group, so they apply to the pattern as a whole, not
just to its first and last alternatives.
```

### :match?

```code
(. regexp :match? text meta) -> :t | :nil
```

### :search

```code
(. regexp :search text meta) -> matches
```

