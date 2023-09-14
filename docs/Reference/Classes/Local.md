# Local

## Fmap

```code
(Local fnc_create fnc_destroy &optional max_size init_size size) -> local
```code

### :add_node

```code
(. local :add_node node)

add new node
```code

### :close

```code
(. local :close)

close tasks
```code

### :refresh

```code
(. local :refresh [timeout]) -> :t | :nil

scan known nodes and update map
```code

### :restart

```code
(. local :restart key val)

restart task
```code

