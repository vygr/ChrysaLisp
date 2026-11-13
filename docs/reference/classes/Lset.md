# Lset

```image
docs/reference/classes/Lset.cwb
```

## Set

```code
(Lset) -> lset
```

### :copy

```code
(. lset :copy) -> lset
```

### :deep_copy

```code
(. lset :deep_copy) -> lset
```

### :each

```code
(. lset :each lambda) -> lset
```

### :empty

```code
(. lset :empty [count]) -> lset | :t | :nil

all of it, or with a count so many items let go of, the last, :t
while there is more
```

### :empty?

```code
(. lset :empty?) -> :t | :nil
```

### :erase

```code
(. lset :erase key) -> lset
```

### :find

```code
(. lset :find key) -> :nil | key
```

### :insert

```code
(. lset :insert key) -> lset
```

### :inserted

```code
(. lset :inserted key) -> :nil | lset
```

### :intern

```code
(. lset :intern key) -> key
```

### :intersect

```code
(. lset :intersect set) -> lset
```

### :move

```code
(. lset :move) -> lset
```

### :resize

```code
(. lset :resize num_buckets) -> lset
```

### :size

```code
(. lset :size) -> size
```

### :some

```code
(. lset :some lambda) -> :nil | val

each key till the lambda gives other than :nil, what it gave
```

### :tolist

```code
(. lset :tolist) -> list
```

