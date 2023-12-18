# Global

## Fmap

```code
(Global fnc_create fnc_destroy) -> global
```

### :close

```code
(. global :close)

close tasks
```

### :refresh

```code
(. global :refresh [timeout]) -> :t | :nil

scan known nodes and update node map
```

### :size

```code
(. global :size) -> size

size of tasks
```

