# Fset

```image
docs/reference/classes/Fset.cwb
```

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

### :each

```code
(. fset :each lambda) -> fset
```

### :empty

```code
(. fset :empty [count]) -> fset | :t | :nil

all of it, or with a count about so many items let go of, a bucket
at a time in turn, :t while there is more: a big one let go of a
part at a time, and as good as ever meanwhile
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
(. fset :intersect set) -> fset
```

### :move

```code
(. fset :move) -> fset
```

### :resize

```code
(. fset :resize num_buckets) -> fset
```

### :size

```code
(. fset :size) -> size
```

### :some

```code
(. fset :some lambda) -> :nil | val

each key till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. fset :tolist) -> list
```

