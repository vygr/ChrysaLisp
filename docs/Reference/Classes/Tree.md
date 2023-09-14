# Tree

## Flow

```code
(Tree event) -> tree
```

### :action

```code
(. tree :action event) -> tree
```

### :add_route

```code
(. tree :add_route route) -> tree

routes are:
"a/b/c/file"
"a/b/c/."
```

### :collapse

```code
(. tree :collapse) -> tree
```

### :empty

```code
(. tree ::empty) -> tree

empty tree
```

### :expand

```code
(. tree :expand) -> tree
```

### :find_node

```code
(. tree :find_node route) -> node | :nil

routes are:
"a/b/c/node"
```

### :get_relative

```code
(. tree :get_relative route) -> (x y w h)

relative position of route
```

### :get_route

```code
(. tree :get_route node) -> tree
```

### :populate

```code
(. tree :populate &optional root exts n) -> tree

load up a file tree
```

### :select

```code
(. tree :select route) -> tree

highlight a tree route
```

