# Substr

```code
(Substr) -> substr
```

### :compile

```code
(. substr :compile pattern) -> :nil | meta
```

### :match?

```code
(. substr :match? text pattern &optional meta) -> :t | :nil
```

### :search

```code
(. substr :search text pattern &optional meta) -> (slices subslices submatches)
```

