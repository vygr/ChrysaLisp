# Xmap

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

### :empty

```code
(. xmap :empty) -> xmap
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

### :move

```code
(. xmap :move) -> xmap
```

### :resize

```code
(. xmap :resize num_buckets) -> xmap
```

### :update

```code
(. xmap :insert key lambda) -> xmap
```

