# Canvas-base

## View

### :draw

```code
(. canvas :draw) -> canvas
```code

### :fbox

```code
(. canvas :fbox x y width height) -> canvas
```code

### :fill

```code
(. canvas :fill argb) -> canvas
```code

### :fpoly

```code
(. canvas :fpoly x y winding_mode paths) -> canvas
```code

### :ftri

```code
(. canvas :ftri tri) -> canvas
```code

### :get_clip

```code
(. canvas :get_clip) -> (cx cy cx1 cy1)
```code

### :get_color

```code
(. canvas :get_color) -> argb
```code

### :next_frame

```code
(. canvas :next_frame) -> canvas
```code

### :plot

```code
(. canvas :plot x y) -> canvas
```code

### :pref_size

```code
(. canvas :pref_size) -> (width height)
```code

### :resize

```code
(. canvas :resize canvas) -> canvas
```code

### :save

```code
(. canvas :save file format) -> :nil | canvas
```code

### :set_canvas_flags

```code
(. canvas :set_canvas_flags flags) -> canvas
```code

### :set_color

```code
(. canvas :set_color argb) -> canvas
```code

### :swap

```code
(. canvas :swap flags) -> canvas
```code

