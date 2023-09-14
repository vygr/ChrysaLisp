# Tree

## Flow

```code
(Tree event) -> tree
```code

### :action

```code
(. tree :action event) -> tree
```code

### :add_route

```code
(. tree :add_route route) -> tree
```code

### :collapse

```code
(. tree :collapse) -> tree
```code

### :empty

```code
(. tree ::empty) -> tree
```code

### :expand

```code
(. tree :expand) -> tree
```code

### :find_node

```code
(. tree :find_node route) -> node | :nil
```code

### :get_relative

```code
(. tree :get_relative route) -> (x y w h)
```code

### :get_route

```code
(. tree :get_route node) -> tree
```code

### :populate

```code
(. tree :populate &optional root exts n) -> tree
```code

### :select

```code
(. tree :select route) -> tree
```code

