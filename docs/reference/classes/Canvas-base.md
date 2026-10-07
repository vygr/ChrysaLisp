# Canvas-base

## View

### :constraint

```code
(. canvas :constraint) -> (width height)

with the pixmap freed the size is that of the texture
```

### :draw

```code
(. canvas :draw) -> canvas
```

### :exchange

```code
(. canvas :exchange that) -> canvas

the two canvases exchange what they show, their textures. So one
can be off screen, drawn into by a shader a strip at a time, and
put on show when the frame is whole. They must be the same size.
```

### :fbox

```code
(. canvas :fbox x y width height) -> canvas
```

### :fill

```code
(. canvas :fill argb) -> canvas
```

### :flip_x

```code
(. canvas :flip_x canvas) -> canvas
```

### :fpoly

```code
(. canvas :fpoly x y winding_mode paths) -> canvas
```

### :free

```code
(. canvas :free) -> canvas

let go of the pixmap, the texture stays as it is. For a canvas
that is only ever shown, it can not be drawn on after this.
```

### :get_clip

```code
(. canvas :get_clip) -> (cx cy cx1 cy1)
```

### :get_color

```code
(. canvas :get_color) -> argb
```

### :next_frame

```code
(. canvas :next_frame) -> canvas
```

### :plot

```code
(. canvas :plot x y) -> canvas
```

### :resize

```code
(. canvas :resize canvas) -> canvas
```

### :set_canvas_flags

```code
(. canvas :set_canvas_flags flags) -> canvas
```

### :set_clip

```code
(. canvas :set_clip x y x1 y1) -> canvas

what is drawn from here on is kept to this part of the pixmap. It
is cut down to the pixmap, and with no pixmap it is left alone.
```

### :set_color

```code
(. canvas :set_color argb) -> canvas
```

### :shade

```code
(. canvas :shade shader block [x y x1 y1]) -> :nil | :error | canvas

the GPU draws the shader into the texture of the canvas, all of
it, or the part given, in pixels of the texture. One draw is on
the go at a time, :nil is the GPU still busy with the last, or
the driver still building the shader, it has drawn nothing, try
again. :error is a shader the driver could not build. A frame a
slow GPU takes long over is best drawn as strips, so the GUI can
be drawn in between.
```

### :shade_tris

```code
(. canvas :shade_tris frame) -> :nil | :error | canvas

the GPU draws a frame of triangles into the texture of the
canvas, with a depth buffer. The frame is a count, 8 bytes, then
for each thing drawn, 8 bytes each, its pair, its mesh, the
length of the vertex shader's block and of the pixel shader's,
then the two blocks, each made up to a whole 8 bytes. :nil is
the GPU still busy, or a pair still being built, nothing was
drawn, try again. :error is a pair the driver could not build.
```

### :swap

```code
(. canvas :swap flags) -> canvas

+swap_write, the pixmap to the texture, to which a +pixmap_mode
and any +swap_flag can be added. Or +swap_read, the texture back
to the pixmap.
```

### :tile

```code
(. canvas :tile data x1 y1 x2 y2) -> area
```

