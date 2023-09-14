# Farm

## Fmap

```code
(Farm fnc_create fnc_destroy size) -> farm
```code

### :close

```code
(. farm :close)

close tasks
```code

### :refresh

```code
(. farm :refresh [timeout]) -> :t | :nil

scan known nodes and update map
```code

### :restart

```code
(. farm :restart key val)

restart task
```code

