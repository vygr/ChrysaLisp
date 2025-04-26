# Substr

## Search

```code
(Substr [num_buckets]) -> substr
```

### :compile

```code
(. substr :compile pattern) -> :nil | meta
```

### :match?

```code
(. substr :match? text pattern [meta]) -> :t | :nil
```

### :search

```code
(. substr :search text pattern [meta]) -> matches
```

