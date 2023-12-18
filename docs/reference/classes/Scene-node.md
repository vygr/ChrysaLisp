# Scene-node

```code
(Scene-node &optional name) -> scene_node
```

### :add_node

```code
(. scene_node :add_node child) -> scene_node
```

### :children

```code
(. scene_node :children) -> children
```

### :find_nodes

```code
(. scene_node :find_nodes name) -> nodes
```

### :get_matrix

```code
(. scene_node :get_matrix) -> matrix
```

### :get_parent

```code
(. scene_node :get_parent) -> scene_node | :nil
```

### :set_parent

```code
(. scene_node :set_parent parent) -> scene_node
```

### :set_rotation

```code
(. scene_node :set_rotation x y z) -> scene_node
```

### :set_scale

```code
(. scene_node :set_scale x y z) -> scene_node
```

### :set_translation

```code
(. scene_node :set_translation x y z) -> scene_node
```

### :sub_node

```code
(. scene_node :sub_node) -> scene_node
```

