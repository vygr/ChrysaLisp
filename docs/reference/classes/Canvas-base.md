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

### :ftri

```code
(. canvas :ftri tri) -> canvas
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

### :set_color

```code
(. canvas :set_color argb) -> canvas
```

### :shade

```code
(. canvas :shade shader block) -> canvas

the GPU draws the shader into the texture of the canvas
```

### :swap

```code
(. canvas :swap flags) -> canvas
```

### :tile

```code
(. canvas :tile data x1 y1 x2 y2) -> area
```

