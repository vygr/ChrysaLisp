# Scene

## Scene-node

```code
(Scene [name]) -> scene_node
```

### :render

```code
(. scene_node :render canvas size left right top bottom near far mode) -> scene_node

with mode the faces of the meshes are drawn, by a vertex shader and
a pixel shader, with a depth buffer. Without it their vertices are,
as dots
```

