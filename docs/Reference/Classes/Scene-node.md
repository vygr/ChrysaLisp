# Scene-node

```code
(Scene-node &optional name) -> scene_node
```code

### :add_node

```code
(. scene_node :add_node child) -> scene_node
```code

### :children

```code
(. scene_node :children) -> children
```code

### :find_nodes

```code
(. scene_node :find_nodes name) -> nodes
```code

### :get_matrix

```code
(. scene_node :get_matrix) -> matrix
```code

### :get_parent

```code
(. scene_node :get_parent) -> scene_node | :nil
```code

### :set_parent

```code
(. scene_node :set_parent parent) -> scene_node
```code

### :set_rotation

```code
(. scene_node :set_rotation x y z) -> scene_node
```code

### :set_scale

```code
(. scene_node :set_scale x y z) -> scene_node
```code

### :set_translation

```code
(. scene_node :set_translation x y z) -> scene_node
```code

### :sub_node

```code
(. scene_node :sub_node) -> scene_node
```code

