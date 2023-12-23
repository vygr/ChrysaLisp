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
(. tree :empty) -> tree

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

### :get_route

```code
(. tree :get_route node) -> route
```

### :highlight

```code
(. tree :highlight route [state]) -> tree

highlight/lolight a tree route
```

### :populate

```code
(. tree :populate [root exts n mode]) -> tree

load up a file tree
```

### :select

```code
(. tree :select route) -> tree

select a tree route
```

