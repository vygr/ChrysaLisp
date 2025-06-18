# Fmap

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
(. fmap :each lambda)
```

### :empty

```code
(. fmap :empty) -> fmap
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

### :update

```code
(. fmap :update key lambda) -> val
```

