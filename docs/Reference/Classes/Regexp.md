# Regexp

```code
(Regexp) -> regexp
```

### :compile

```code
(. regexp :compile pattern) -> :nil | meta
```

### :match?

```code
(. regexp :match? text pattern &optional meta) -> :t | :nil
```

### :search

```code
(. regexp :search text pattern &optional meta) -> (slices subslices submatches)
```

