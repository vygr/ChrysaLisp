# Render-tris

```image
docs/reference/classes/Render-tris.cwb
```

## Render

```code
(Render-tris canvas vfile pfile [cull]) -> render

frames of triangles on a canvas, drawn with the vertex shader and
the pixel shader of those two files. With cull a triangle that faces
away is left out, :front those that face us. For the nodes to draw,
and for the GPU on a node with no GUI, the canvas is one whose pixels
are shared, (canvas-shared). It starts on the GPU if this node has
one
```

### :forget

```code
(. render :forget) -> render

no mesh is known any more, and the farm, whose children have
them, is let go of: for a new scene
```

### :frame

```code
(. render :frame draws) -> :nil | :t

a frame, a list of (id vvals pvals [pictures]): the number of a
mesh, the inputs of the vertex shader and of the pixel shader for
it, each a list of (name value), and the pictures the pixel shader
reads from, canvases, in the order it has them. Or each as
(shader-strip) takes it, (id vblock pblock [y y1 pictures]), the
blocks made, by an app with a :here_fn. :t if it is whole and
shown, it was drawn here. :nil if it is out with a farm, and
(:busy) says when it no longer is; no more is given till then
```

### :here

```code
(. render :here draws) -> :t

a frame by native code in this task, a thing after another into the
one depth buffer, and shown
```

### :mail

```code
(. render :mail idx msg) -> :nil | :t

what came to mailbox idx of (:mboxes). :t if a frame that was out
is now whole and shown, the app may give the next
```

### :mesh

```code
(. render :mesh id verts) -> render

a mesh frames are drawn of, by a number of the app's, never given
to another while this render lasts: verts a doubles, the numbers
of a vertex one after another as the vertex shader has its attrs
```

