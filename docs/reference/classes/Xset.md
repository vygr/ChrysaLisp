# Xset

```image
docs/reference/classes/Xset.cwb
```

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

### :each

```code
(. xset :each lambda) -> xset
```

### :empty

```code
(. xset :empty [count]) -> xset | :t | :nil

all of it, or with a count about so many items let go of, a bucket
at a time in turn, :t while there is more: a big one let go of a
part at a time, and as good as ever meanwhile
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

### :resize

```code
(. xset :resize num_buckets) -> xset
```

### :size

```code
(. xset :size) -> size
```

### :some

```code
(. xset :some lambda) -> :nil | val

each key till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. xset :tolist) -> list
```

