# Farm

## Fmap

```code
(Farm fnc_create fnc_destroy size) -> farm
```

### :close

```code
(. farm :close)

close tasks
```

### :refresh

```code
(. farm :refresh [timeout]) -> :t | :nil

scan known nodes and update map
```

### :restart

```code
(. farm :restart key val)

restart task
```

