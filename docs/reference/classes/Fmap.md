# Fmap

```image
docs/reference/classes/Fmap.cwb
```

## Map

```code
(Fmap [num_buckets]) -> fmap
```

### :copy

```code
(. fmap :copy) -> fmap
```

### :deep_copy

```code
(. fmap :deep_copy) -> fmap
```

### :each

```code
(. fmap :each lambda) -> fmap
```

### :empty

```code
(. fmap :empty [count]) -> fmap | :t | :nil

all of it, or with a count about so many items let go of, a bucket
at a time in turn, :t while there is more: a big one let go of a
part at a time, and as good as ever meanwhile
```

### :empty?

```code
(. fmap :empty?) -> :t | :nil
```

### :erase

```code
(. fmap :erase key) -> fmap
```

### :find

```code
(. fmap :find key) -> :nil | val
```

### :insert

```code
(. fmap :insert key val) -> fmap
```

### :memoize

```code
(. fmap :memoize key lambda) -> val
```

### :move

```code
(. fmap :move) -> fmap
```

### :resize

```code
(. fmap :resize num_buckets) -> fmap
```

### :size

```code
(. fmap :size) -> size
```

### :some

```code
(. fmap :some lambda) -> :nil | val

each key and value till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. fmap :tolist) -> list
```

### :update

```code
(. fmap :update key lambda) -> val
```

