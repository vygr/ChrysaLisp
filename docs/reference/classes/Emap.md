# Emap

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

### :defer

```code
(. emap :defer key lambda) -> val
```

### :empty

```code
(. emap :empty) -> emap
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

### :move

```code
(. emap :move) -> emap
```

### :resize

```code
(. emap :resize num_buckets) -> emap
```

### :update

```code
(. emap :update key lambda) -> val
```

