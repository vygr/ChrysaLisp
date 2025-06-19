# Lmap

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
(. lmap :empty) -> lmap
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

### :update

```code
(. lmap :update key lambda) -> val
```

