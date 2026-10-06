# :canvas

## :view

## Lisp Bindings

### (create-canvas width height scale)

### (create-canvas-pixmap pixmap)

### (canvas-fbox canvas x y w h)

### (canvas-fill canvas argb)

### (canvas-flip-x canvas canvas)

### (canvas-fpoly canvas x y mode list)

### (canvas-ftri canvas path)

### (canvas-next-frame canvas)

### (canvas-plot canvas x y)

### (canvas-resize canvas canvas)

### (canvas-shade canvas shader block)

### (canvas-shader-create vertex fragment)

### (canvas-shader-destroy shader)

### (canvas-shader-format)

### (canvas-swap canvas flags)

### (canvas-tile canvas data x1 y1 x2 y2)

## VP methods

### :create -> gui/canvas/create

```code
inputs
:r0 = width (pixels)
:r1 = height (pixels)
:r2 = aa scale (uint)
outputs
:r0 = 0 if error, else canvas object (ptr)
trashes
:r0-:r7, :r14, :f0-:f15
```

### :create_pixmap -> gui/canvas/create_pixmap

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = 0 if error, else canvas object (ptr)
trashes
:r0-:r5, :r14, :f0-:f15
```

### :deinit -> gui/canvas/deinit

```code
inputs
:r0 = canvas object (ptr)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :fbox -> gui/canvas/fbox

```code
inputs
:r0 = canvas object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = w (pixels)
:r10 = h (pixels)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r9
```

### :fpoly -> gui/canvas/fpoly

```code
inputs
:r0 = canvas object (ptr)
:r1 = x (fixed)
:r2 = y (fixed)
:r3 = winding mode (winding_odd_even, winding_none_zero)
:r4 = list of path objects (ptr)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :ftri -> gui/canvas/ftri

```code
inputs
:r0 = canvas object (ptr)
:r1 = x0 (fixed)
:r2 = y0 (fixed)
:r3 = x1 (fixed)
:r4 = y1 (fixed)
:r5 = x2 (fixed)
:r6 = y2 (fixed)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14
```

### :init -> gui/canvas/init

```code
inputs
:r0 = canvas object (ptr)
:r1 = vtable (pptr)
:r2 = width (pixels)
:r3 = height (pixels)
:r4 = aa scale (uint)
outputs
:r0 = canvas object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7, :f0-:f15
```

### :init_pixmap -> gui/canvas/init_pixmap

```code
inputs
:r0 = canvas object (ptr)
:r1 = vtable (pptr)
:r2 = pixmap object (ptr)
outputs
:r0 = canvas object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :f0-:f15
```

### :pick -> gui/canvas/pick

```code
inputs
:r0 = canvas object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
outputs
:r0 = canvas object (ptr)
:r1 = color (argb)
trashes
:r1-:r8
```

### :plot -> gui/canvas/plot

```code
inputs
:r0 = canvas object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r8
```

### :set_clip -> gui/canvas/set_clip

```code
inputs
:r0 = canvas object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r2, :r7-:r10
```

### :set_edges -> gui/canvas/set_edges

```code
inputs
:r0 = canvas object (ptr)
:r1 = list of path objects (ptr)
:r2 = x (fixed)
:r3 = y (fixed)
:r4 = y scale (int)
outputs
:r0 = canvas object (ptr)
:r11 = min_x (fixed)
:r12 = min_y (fixed)
:r13 = max_x (fixed)
:r14 = max_y (fixed)
trashes
:r1-:r14, :f0-:f15
```

### :shade -> gui/canvas/shade

```code
inputs
:r0 = canvas object (ptr)
:r1 = shader handle (long)
:r2 = inputs block str object (ptr)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :f0-:f15
info
the GPU draws the shader into the texture of the canvas, the
pixmap is not used, and is not changed
```

### :span -> gui/canvas/span

```code
inputs
:r0 = canvas object (ptr)
:r1 = coverage (long)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r9
info
coverage is 0x0 to 0x80
```

### :span_noclip -> gui/canvas/span_noclip

```code
inputs
:r0 = canvas object (ptr)
:r1 = coverage (long)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r9
info
coverage is 0x0 to 0x80
```

### :swap -> gui/canvas/swap

```code
inputs
:r0 = canvas object (ptr)
:r1 = canvas upload flags (uint)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :vtable -> gui/canvas/vtable

