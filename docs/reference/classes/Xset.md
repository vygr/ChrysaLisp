# Xset

## Set

```code
(Xset [num_buckets cmp_fnc hash_fnc]) -> xset
```

### :copy

```code
(. xset :copy) -> xset
```

### :deep_copy

```code
(. xset :deep_copy) -> xset
```

### :difference

```code
(. xset :difference set) -> xset
```

### :each

```code
(. xset :each lambda) -> xset
```

### :empty

```code
(. xset :empty) -> xset
```

### :empty?

```code
(. xset :empty?) -> :t | :nil
```

### :erase

```code
(. xset :erase key) -> xset
```

### :find

```code
(. xset :find key) -> :nil | key
```

### :insert

```code
(. xset :insert key) -> xset
```

### :inserted

```code
(. xset :inserted key) -> :nil | xset
```

### :intern

```code
(. xset :intern key) -> key
```

### :intersect

```code
(. xset :intersect set) -> xset
```

### :move

```code
(. xset :move) -> xset
```

### :not_intersect

```code
(. xset :not_intersect set) -> xset
```

### :resize

```code
(. xset :resize num_buckets) -> xset
```

### :size

```code
(. xset :size) -> size
```

### :union

```code
(. xset :union set) -> xset
```

