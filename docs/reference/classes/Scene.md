# Scene

## Scene-node

```code
(Scene [name]) -> scene_node
```

### :draw

```code
(. scene :draw canvas draws) -> scene

the faces of a frame, drawn here, by the shaders as native code,
with a depth buffer. The canvas is not swapped
```

### :draws

```code
(. scene :draws left right top bottom near far height) -> ((mesh vblock pblock y y1) ...)

what is drawn for a frame of the faces, as it is seen now, a frame
of that many rows. For each object that has a mesh, the number of
the mesh, the inputs of the vertex shader and of the pixel shader
for it, as the blocks they travel in, and the rows it may be on.
It is what a child that draws a strip is sent, and what is drawn
from here, so the two are the same to the bit.
```

### :mesh

```code
(. scene :mesh id) -> str

the mesh of that number, as the shaders want it, for a child that
has asked for it
```

### :render

```code
(. scene :render canvas size left right top bottom near far mode) -> scene

with mode the faces of the meshes are drawn, by a vertex shader and
a pixel shader, with a depth buffer. Without it their vertices are,
as dots
```

