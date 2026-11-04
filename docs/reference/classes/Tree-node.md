# Tree-node

```image
docs/reference/classes/Tree-node.cwb
```

```code
(Tree-node [name]) -> tree_node
```

### :add_node

```code
(. tree_node :add_node child) -> tree_node

a child added last, out of the parent it had first
```

### :children

```code
(. tree_node :children) -> children
```

### :each_node

```code
(. tree_node :each_node fnc) -> tree_node

(fnc node)
fnc of this node and of every one under it, a parent before its
children and they in order. What is to come is a stack, the depth
of the tree costs nothing. fnc is called from inside it, so the
names it uses are not these: fnc stack node. The children are pushed
last first, so the first comes off first, by (reduce!) of push,
which gives back the stack to pop the next from: 10.9 us a walk of
111, (reach) and a push 12.5, a loop of (elem-get) 18
```

### :find_nodes

```code
(. tree_node :find_nodes name) -> nodes

it and those under it whose names begin so, in the order of a walk
```

### :get_name

```code
(. tree_node :get_name) -> str
```

### :get_parent

```code
(. tree_node :get_parent) -> tree_node | :nil
```

### :root

```code
(. tree_node :root) -> tree_node

the top of the tree it is in
```

### :set_children

```code
(. tree_node :set_children children) -> tree_node

these are its children, in place of those it had, and none of
another's: each is given this as its parent
```

### :set_parent

```code
(. tree_node :set_parent parent) -> tree_node
```

### :sub_node

```code
(. tree_node :sub_node) -> tree_node

this one taken out of its parent
```

### :walk_nodes

```code
(. tree_node :walk_nodes fnc_in fnc_out) -> tree_node

if fnc_in returns :nil, it'll step down into that nodes children.
fnc_out is always called to balance calls to fnc_in.
```

