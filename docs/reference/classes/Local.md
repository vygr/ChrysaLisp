# Local

## Fmap

```code
(Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local
```

### :add_node

```code
(. local :add_node node)

add new node
```

### :close

```code
(. local :close)

close tasks
```

### :refresh

```code
(. local :refresh [timeout]) -> :t | :nil

scan known nodes and update map
```

### :restart

```code
(. local :restart key val)

restart task
```

