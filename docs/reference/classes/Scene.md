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
Objects that are see through come last, the furthest first.
It is what a child that draws a strip is sent, and what is drawn
from here, so the two are the same to the bit.
An object with :smooth is lit smooth, a normal a vertex. One with
:shaders, the files of a vertex and a pixel shader, is drawn with
those, they have the inputs of the scene's own, and its draw has
the files on the end, for what draws it to choose them by.
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

### :set_shaders

```code
(. scene :set_shaders files) -> scene

the files of the vertex and the pixel shader every object of the
scene is drawn with, that has not two of its own. :nil is the
scene's usual two
```

