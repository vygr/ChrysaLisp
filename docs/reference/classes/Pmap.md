# Pmap

## Map

```code
(Pmap) -> pmap
```

### :copy

```code
(. pmap :copy) -> pmap
```

### :deep_copy

```code
(. pmap :deep_copy) -> pmap
```

### :each

```code
(. pmap :each lambda) -> pmap
```

### :empty

```code
(. pmap :empty) -> pmap
```

### :empty?

```code
(. pmap :empty?) -> :t | :nil
```

### :erase

```code
(. pmap :erase key) -> pmap
```

### :find

```code
(. pmap :find key) -> :nil | val
```

### :insert

```code
(. pmap :insert key val) -> pmap
```

### :memoize

```code
(. pmap :memoize key lambda) -> val
```

### :move

```code
(. pmap :move) -> pmap
```

### :resize

```code
(. pmap :resize num_buckets) -> pmap
```

### :size

```code
(. pmap :size) -> size
```

### :tolist

```code
(. pmap :tolist) -> list
```

### :update

```code
(. pmap :update key lambda) -> val
```

