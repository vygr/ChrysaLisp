# Fset

## Set

```code
(Fset [num_buckets]) -> fset
```

### :copy

```code
(. fset :copy) -> fset
```

### :deep_copy

```code
(. fset :deep_copy) -> fset
```

### :difference

```code
(. fset :difference fset) -> fset
```

### :each

```code
(. fset :each lambda)
```

### :empty

```code
(. fset :empty) -> fset
```

### :empty?

```code
(. fset :empty?) -> :t | :nil
```

### :erase

```code
(. fset :erase key) -> fset
```

### :find

```code
(. fset :find key) -> :nil | key
```

### :insert

```code
(. fset :insert key) -> fset
```

### :inserted

```code
(. fset :inserted key) -> :nil | fset
```

### :intern

```code
(. fset :intern key) -> key
```

### :intersect

```code
(. fset :intersect fset) -> fset
```

### :move

```code
(. fset :move) -> fset
```

### :not_intersect

```code
(. fset :not_intersect fset) -> fset
```

### :resize

```code
(. fset :resize num_buckets) -> fset
```

### :union

```code
(. fset :union fset) -> fset
```

