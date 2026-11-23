# :canvas

```image
docs/reference/vp_classes/canvas.cwb
```

## :view

## Lisp Bindings

### (canvas-clear canvas argb x y w h)

### (create-canvas width height scale)

### (create-canvas-pixmap pixmap)

### (canvas-fbox canvas x y w h)

### (canvas-fill canvas argb)

### (canvas-flip-x canvas canvas)

### (canvas-fpoly canvas x y mode list)

### (canvas-paint canvas kind data)

### (canvas-plot canvas x y)

### (canvas-resize canvas canvas)

### (canvas-swap canvas flags)

### (canvas-target canvas)

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
:r0-:r7, :r14, :d0-:d15
```

### :create_pixmap -> gui/canvas/create_pixmap

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = 0 if error, else canvas object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :deinit -> gui/canvas/deinit

```code
inputs
:r0 = canvas object (ptr)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r4, :d0-:d15
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
:r1-:r9, :r12, :d0-:d1
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
:r1-:r14, :d0-:d15
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
:r1-:r7, :d0-:d15
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
:r1-:r5, :d0-:d15
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

### :read -> gui/canvas/read

```code
inputs
:r0 = canvas object (ptr)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :d0-:d15
info
the texture of the canvas is read back into its pixmap. The
canvas is given a new pixmap, premultiplied, so one that was
freed is made again, and one that is shared is left alone.
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
:r1-:r14, :d0-:d15
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
:r1-:r9, :r12, :d0-:d1
info
coverage is 0x0 to 0x80
```

### :span_linear -> gui/canvas/span_linear

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
t is u, a0 + ax x + ay y of the paint, and the step of the 256 its
top 8 bits of fraction
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
coverage is 0x0 to 0x80. The canvas's span field, which :fpoly, :fbox
and :span call, may be this or any of the spans of gui/canvas/paint.vp:
what a call through it trashes is the union of theirs, as a virtual
method's is of its overrides, cmd/trace.lisp +pointer_families. :fpoly
and :fbox keep nothing in a register over it but :r0, they load all
they want again from their frames, so a span may trash all but :r0
```

### :span_radial -> gui/canvas/span_radial

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
:r1-:r9, :r12, :d0-:d1
info
t is the length of u and v, the root of :sys_math :q_sqrt
```

### :span_row -> gui/canvas/span_row

```code
inputs
:r0 = canvas object (ptr)
:r1 = coverage (long)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r2 = the colors of x to x1, argb not premultiplied (puint)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r9
info
coverage is 0x0 to 0x80. Each pixel as :span_noclip does its one color,
the alpha by the coverage, premultiplied, then blended, so a row of one
color is what :span_noclip draws of it to the byte
```

### :swap -> gui/canvas/swap

```code
inputs
:r0 = canvas object (ptr)
:r1 = canvas upload flags (uint)
outputs
:r0 = canvas object (ptr)
trashes
:r1-:r14, :d0-:d15
```

### :vtable -> gui/canvas/vtable

