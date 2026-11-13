# Emap

```image
docs/reference/classes/Emap.cwb
```

## Map

```code
(Emap [num_buckets]) -> emap
```

### :copy

```code
(. emap :copy) -> emap
```

### :deep_copy

```code
(. emap :deep_copy) -> emap
```

### :each

```code
(. emap :each lambda)
```

### :empty

```code
(. emap :empty [count]) -> emap | :nil

all of it, with a count too: an env is let go of as one, so an
emap is not for what is to be let go of a part at a time
```

### :empty?

```code
(. emap :empty?) -> :t | :nil
```

### :erase

```code
(. emap :erase key) -> emap
```

### :find

```code
(. emap :find key) -> :nil | val
```

### :insert

```code
(. emap :insert key val) -> emap
```

### :memoize

```code
(. emap :memoize key lambda) -> val
```

### :move

```code
(. emap :move) -> emap
```

### :resize

```code
(. emap :resize num_buckets) -> emap
```

### :size

```code
(. emap :size) -> size
```

### :some

```code
(. emap :some lambda) -> :nil | val

each key and value till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. emap :tolist) -> list
```

### :update

```code
(. emap :update key lambda) -> val
```

