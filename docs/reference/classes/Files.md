# Files

## Flow

```code
(Files title event) -> files
```

### :action

```code
(. files :action event) -> files
```

### :add_route

```code
(. files :add_route route) -> files

routes are:
"a/b/c/file"
"a/b/c/."
```

### :collapse

```code
(. files :collapse) -> files
```

### :empty

```code
(. files :empty) -> files
```

### :expand

```code
(. files :expand) -> files
```

### :find_node

```code
(. files :find_node route) -> node | :nil

routes are:
"a/b/c/node"
```

### :get_route

```code
(. files :get_route node) -> route
```

### :highlight

```code
(. files :highlight route [state]) -> files

highlight/lolight a tree route
```

### :layout_tree

```code
(. files :layout_tree) -> files
```

### :populate

```code
(. files :populate[root exts n mode]) -> files

load up a file tree
```

### :select_node

```code
(. files :select_node route) -> files
```

