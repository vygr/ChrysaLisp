# Render-tris

```image
docs/reference/classes/Render-tris.cwb
```

```code
(Render-tris canvas vfile pfile [cull]) -> render

frames of triangles on a canvas, drawn with the vertex shader and
the pixel shader of those two files. With cull a triangle that faces
away is left out, :front those that face us. For the nodes to draw,
and for the GPU on a node with no GUI, the canvas is one whose pixels
are shared, (canvas-shared). It starts on the GPU if this node has
one
```

### :busy

```code
(. render :busy) -> :nil | :t

a frame is out with a farm, the next is not to be given yet
```

### :canvas

```code
(. render :canvas canvas) -> render

another canvas to draw on from the next frame, the app's was made
again, of another size say. A frame that is out for the old one
is not waited for. The farm is kept, its children find the pixels
and the texture a frame is for from the frame itself
```

### :close

```code
(. render :close) -> render

the farm let go of, the app is closing
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

### :how

```code
(. render :how) -> str

how the last frame was drawn, and how long it took, for a line
of status. A frame the GPU drew straight into the canvas's texture
has no time: it was handed over, and when the GPU was done with it
is not known here
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

### :mode

```code
(. render :mode [mode]) -> :gpu | :cpu

how frames are drawn, and with a mode, :gpu or :cpu, how they are
to be from the next. The GPU is not to be had on a node with none,
or once its child has said it can not
```

### :read

```code
(. render :read select before) -> (idx msg)

the next mail of the app's mailboxes, select, or of the render's
own, which are read as if they were in select before the one of
that index: an app has its timers last, and puts them after these
too, so a timer that is always due does not leave them unread.
idx is the app's mailbox the mail came to, and msg the mail. :nil
for idx is mail of the render's, dealt with: if a frame was out,
(:busy) says whether it still is
```

### :tick

```code
(. render :tick) -> render

called from the app's timer: now and then a child that has gone,
or been too long, is started again. And a farm that has had no
frame for a while is let go of, its work has stopped, the next
frame makes a new one
```

