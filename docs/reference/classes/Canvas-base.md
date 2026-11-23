# Canvas-base

```image
docs/reference/classes/Canvas-base.cwb
```

## View

### :clear

```code
(. canvas :clear argb x y width height) -> canvas

a box of the pixmap, in its pixels, is made the color. It is set,
not laid over what is there: with 0 the box is made clear, to be
drawn in again, which nothing that draws can do. (:fill) is this
of all of it. The function is looked for when this is called, a
system from before it was written has none, and can still load this
```

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
can be off screen, drawn into by the GPU, (canvas-target), and put
on show when the frame is whole. They must be the same size.
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

what is drawn from here is this color, a paint let go of
```

### :set_linear

```code
(. canvas :set_linear x y x1 y1 stops [spread m]) -> canvas

what is drawn from here is a linear gradient, t 0 at x y and 1 at x1
y1, its bands across that line; m, default none, the matrix from the
space they are in to the pixels :fpoly draws in, as an SVG's is,
under which the bands may lie at a slant to the line. stops are
((offset argb) ...), offsets 0 to 1, spread +spread_pad, the
default, +spread_reflect or +spread_repeat. Of no length it is the
color of the last stop
```

### :set_radial

```code
(. canvas :set_radial cx cy r stops [spread m]) -> canvas

what is drawn from here is a radial gradient, t 0 at cx cy and 1 at r
from it, as (:set_linear): under m it may be an ellipse. Of no
radius it is the color of the last stop
```

### :swap

```code
(. canvas :swap flags) -> canvas

+swap_write, the pixmap to the texture, to which a +pixmap_mode
and any +swap_flag can be added. Or +swap_read, the texture back
to the pixmap. On a node with no GUI there is no texture, the swap
is :nil and there is nothing on the screen to be dirty
```

### :tile

```code
(. canvas :tile data x1 y1 x2 y2) -> area
```

