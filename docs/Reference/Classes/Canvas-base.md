# Canvas-base

## View

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

### :fpoly

```code
(. canvas :fpoly x y winding_mode paths) -> canvas
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

### :pref_size

```code
(. canvas :pref_size) -> (width height)
```

### :resize

```code
(. canvas :resize canvas) -> canvas
```

### :save

```code
(. canvas :save file format) -> :nil | canvas
```

### :set_canvas_flags

```code
(. canvas :set_canvas_flags flags) -> canvas
```

### :set_color

```code
(. canvas :set_color argb) -> canvas
```

### :swap

```code
(. canvas :swap flags) -> canvas
```

