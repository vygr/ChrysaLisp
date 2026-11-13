# Lmap

```image
docs/reference/classes/Lmap.cwb
```

## Map

```code
(Lmap) -> lmap
```

### :copy

```code
(. lmap :copy) -> lmap
```

### :deep_copy

```code
(. lmap :deep_copy) -> lmap
```

### :each

```code
(. lmap :each lambda) -> lmap
```

### :empty

```code
(. lmap :empty [count]) -> lmap | :t | :nil

all of it, or with a count so many items let go of, the last, :t
while there is more
```

### :empty?

```code
(. lmap :empty?) -> :t | :nil
```

### :erase

```code
(. lmap :erase key) -> lmap
```

### :find

```code
(. lmap :find key) -> :nil | val
```

### :insert

```code
(. lmap :insert key val) -> lmap
```

### :memoize

```code
(. lmap :memoize key lambda) -> val
```

### :move

```code
(. lmap :move) -> lmap
```

### :resize

```code
(. lmap :resize num_buckets) -> lmap
```

### :size

```code
(. lmap :size) -> size
```

### :some

```code
(. lmap :some lambda) -> :nil | val

each key and value till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. lmap :tolist) -> list
```

### :update

```code
(. lmap :update key lambda) -> val
```

