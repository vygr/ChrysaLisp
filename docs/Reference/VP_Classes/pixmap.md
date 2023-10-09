# pixmap

## obj

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

### :create -> gui/pixmap/create

```code
inputs
:r0 = width (pixels)
:r1 = height (pixels)
:r2 = type (int)
outputs
:r0 = 0 if error, else pixmap object (ptr)
trashes
:r1-:r7
```

### :deinit -> gui/pixmap/deinit

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14
```

### :fill -> gui/pixmap/fill

```code
inputs
:r0 = pixmap object (ptr)
:r1 = color (argb)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r4
```

### :from_argb32 -> gui/pixmap/from_argb32

```code
inputs
:r1 = col (uint)
:r2 = pixel type (uint)
outputs
:r1 = col (uint)
trashes
:r1-:r5
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
:r1-:r14
```

### :lisp_load_cpm -> gui/pixmap/lisp_load_cpm

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(pixmap-load-cpm stream)
```

### :lisp_load_tga -> gui/pixmap/lisp_load_tga

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(pixmap-load-tga stream)
```

### :lisp_save_cpm -> gui/pixmap/lisp_save_cpm

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(pixmap-save-cpm pixmap stream format)
```

### :load_cpm -> gui/pixmap/load_cpm

```code
inputs
:r5 = stream object (ptr)
outputs
:r0 = 0 if error, else pixmap object (ptr)
trashes
:r0-:r14
```

### :load_tga -> gui/pixmap/load_tga

```code
inputs
:r5 = stream object (ptr)
outputs
:r0 = 0 if error, else pixmap object (ptr)
trashes
:r0-:r14
```

### :next_frame -> gui/pixmap/next_frame

```code
inputs
:r0 = pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14
```

### :resize -> gui/pixmap/resize

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14
```

### :resize_2 -> gui/pixmap/resize_2

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14
```

### :resize_3 -> gui/pixmap/resize_3

```code
inputs
:r0 = pixmap object (ptr)
:r1 = source pixmap object (ptr)
outputs
:r0 = pixmap object (ptr)
trashes
:r1-:r14
```

### :save_cpm -> gui/pixmap/save_cpm

```code
inputs
:r4 = pixmap object (ptr)
:r6 = stream object (ptr)
:r7 = format (uint)
outputs
:r0 = pixmap object (ptr)
trashes
:r0-:r14
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
:r1-:r14
```

### :vtable -> gui/pixmap/vtable

