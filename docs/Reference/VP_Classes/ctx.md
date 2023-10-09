# ctx

### :blit -> gui/ctx/blit

```code
inputs
:r0 = view object (ptr)
:r1 = texture id (ulong)
:r2 = color mod (argb)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r14
```

### :box -> gui/ctx/box

```code
inputs
:r0 = view object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r14
```

### :filled_box -> gui/ctx/filled_box

```code
inputs
:r0 = view object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r14
```

### :lisp_blit -> gui/ctx/lisp_blit

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
;(ctx-blit view tid col x y w h)
```

### :lisp_box -> gui/ctx/lisp_box

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
;(ctx-box view x y w h)
```

### :lisp_filled_box -> gui/ctx/lisp_filled_box

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
;(ctx-filled-box view x y w h)
```

### :lisp_set_color -> gui/ctx/lisp_set_color

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
;(ctx-set-color view col)
```

### :set_color -> gui/ctx/set_color

```code
inputs
:r0 = view object (ptr)
:r1 = color (argb)
trashes
:r0-:r14
```

