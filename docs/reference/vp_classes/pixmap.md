# :pixmap

## :obj

## Lisp Bindings

### (pixmap-as-argb pixmap) -> pixmap

### (pixmap-from-argb32 pixel type) -> pixel

### (pixmap-read pixmap stream type) -> :nil | pixmap

### (pixmap-shared width height key) -> :nil | pixmap

### (pixmap-to-argb32 pixel type) -> argb32

### (pixmap-write pixmap stream type) -> pixmap

## VP methods

### :as_argb -> gui/pixmap/as_argb

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r8
```

### :as_premul -> gui/pixmap/as_premul

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r7
```

### :clear -> gui/pixmap/clear

```code
inputs
:r0 = pixmap object (ptr)
:r1 = color (argb)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r4, :r7-:r11
info
every pixel of the box, as much of it as is on the pixmap, is made
the color. It is set, not laid over what is there, so a color with
no alpha makes the box clear, which nothing that draws can
```

### :create -> gui/pixmap/create

```code
inputs
:r0 = width (pixels)
:r1 = height (pixels)
:r2 = type (int)
outputs
:r0 = 0 if error, else pixmap object (ptr)
trashes
:r0-:r7, :f0-:f15
```

### :deinit -> gui/pixmap/deinit

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :fill -> gui/pixmap/fill

```code
inputs
:r0 = pixmap object (ptr)
:r1 = color (argb)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r4, :r7-:r11
info
all of it is a clear of all of it
```

### :flip_x -> gui/pixmap/flip_x

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r2-:r11
```

### :from_argb32 -> gui/pixmap/from_argb32

```code
inputs
:r1 = col (uint)
:r2 = pixel type (uint)
outputs
:r1 = col (uint)
trashes
:r1, :r3-:r5
```

### :init -> gui/pixmap/init

```code
inputs
:r0 = pixmap object (ptr)
:r1 = vtable (pptr)
:r2 = width (pixels)
:r3 = height (pixels)
:r4 = type (int)
outputs
:r0 = pixmap object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r3
```

### :next_frame -> gui/pixmap/next_frame

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r8, :r14, :f0-:f15
```

### :resize -> gui/pixmap/resize

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r13, :f0-:f15
```

### :resize_2 -> gui/pixmap/resize_2

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r2-:r12
```

### :resize_3 -> gui/pixmap/resize_3

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r2-:r13
```

### :to_argb -> gui/pixmap/to_argb

```code
inputs
:r1 = color premul (argb)
outputs
:r1 = color (argb)
trashes
:r1-:r4
```

### :to_argb32 -> gui/pixmap/to_argb32

```code
inputs
:r1 = col (uint)
:r2 = pixel type (uint)
outputs
:r1 = col (uint)
trashes
:r1-:r8
```

### :to_premul -> gui/pixmap/to_premul

```code
inputs
:r1 = color (argb)
outputs
:r1 = color premul (argb)
trashes
:r1-:r3
```

### :upload -> gui/pixmap/upload

```code
inputs
:r0 = pixmap object (ptr)
:r1 = pixmap upload flags (uint)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :vtable -> gui/pixmap/vtable

