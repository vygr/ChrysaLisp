# Farm

## Fmap

```code
(Farm fnc_create fnc_destroy size) -> farm
```

### :close

```code
(. farm :close) -> farm

close tasks
```

### :refresh

```code
(. farm :refresh [timeout]) -> :t | :nil

scan known nodes and update map, merge local node here as well
```

### :restart

```code
(. farm :restart key val) -> farm

restart task
```

