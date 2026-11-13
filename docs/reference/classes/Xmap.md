# Xmap

```image
docs/reference/classes/Xmap.cwb
```

## Map

```code
(Xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
```

### :copy

```code
(. xmap :copy) -> xmap
```

### :deep_copy

```code
(. xmap :deep_copy) -> xmap
```

### :each

```code
(. xmap :each lambda) -> xmap
```

### :empty

```code
(. xmap :empty [count]) -> xmap | :t | :nil

all of it, or with a count about so many items let go of, a bucket
at a time in turn, :t while there is more: a big one let go of a
part at a time, and as good as ever meanwhile
```

### :empty?

```code
(. xmap :empty?) -> :t | :nil
```

### :erase

```code
(. xmap :erase key) -> xmap
```

### :find

```code
(. xmap :find key) -> :nil | val
```

### :insert

```code
(. xmap :insert key val) -> xmap
```

### :memoize

```code
(. fmap :memoize key lambda) -> val
```

### :move

```code
(. xmap :move) -> xmap
```

### :resize

```code
(. xmap :resize num_buckets) -> xmap
```

### :size

```code
(. xmap :size) -> size
```

### :some

```code
(. xmap :some lambda) -> :nil | val

each key and value till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. xmap :tolist) -> list
```

### :update

```code
(. xmap :insert key lambda) -> val
```

