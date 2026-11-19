# Render-shade

```image
docs/reference/classes/Render-shade.cwb
```

## Render

```code
(Render-shade canvas file [lines]) -> render

frames of a pixel shader, of that file, over all of a canvas. By the
GPU, a farm of one on this node, which draws the frame straight into
the canvas's texture where the node has the GUI, and reads it back onto
the canvas's pixels where it has not; or by the nodes, a tile of so
many lines of the frame each, 8 if not given, shown as it fills. The
canvas is one whose pixels are shared, (canvas-shared), for the nodes
to shade straight onto: a child that can not reach them sends its
pixels back
:direct is the GPU to draw into the texture where it can. An app that
wants every frame's pixels, to keep them, has it :nil: they are then
always on the canvas's pixels, read back, and the same to the bit
whatever node drew them
:farm is the nodes to draw a frame the GPU does not. With it :nil such
a frame is drawn here, in the app's own task, by native code, (:here):
for a small picture drawn now and then, not worth a child a node
```

### :frame

```code
(. render :frame vals) -> :nil | :t

a frame, the inputs of the shader for it, a list of (name value).
:t if it is whole and shown, it was drawn here. :nil if it is out
with a farm, (:busy) says when it no longer is; no more is given
till then
```

### :here

```code
(. render :here vals) -> :t

a frame by native code in this task, and shown
```

### :mail

```code
(. render :mail idx msg) -> :nil | :t

what came to mailbox idx of the render's. :t if a frame that was
out is now whole and shown, the app may give the next
```

