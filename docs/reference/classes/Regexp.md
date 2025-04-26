# Regexp

## Search

```code
(Regexp [num_buckets]) -> regexp
```

### :compile

```code
(. regexp :compile pattern) -> :nil | meta
```

### :match?

```code
(. regexp :match? text pattern [meta]) -> :t | :nil
```

### :search

```code
(. regexp :search text pattern [meta]) -> matches
```

