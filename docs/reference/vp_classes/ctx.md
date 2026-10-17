# :ctx

```image
docs/reference/vp_classes/ctx.cwb
```

## Lisp Bindings

### (ctx-blit view mth texture col x y w h [sx sy])

### (ctx-box view mth x y w h)

### (ctx-filled-box view mth x y w h)

### (ctx-filled-region view mth region)

### (ctx-set-color view mth col)

## VP methods

### :blit -> gui/ctx/blit

```code
inputs
:r0 = view object (ptr)
:r1 = 0, else texture object (ptr)
:r2 = color mod (argb)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
:r11 = sx (pixels)
:r12 = sy (pixels)
trashes
:r1-:r5, :r7-:r8, :f0-:f15
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
:r1-:r3, :f0-:f15
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
:r1-:r3, :f0-:f15
```

### :reserve -> gui/ctx/reserve

```code
inputs
:r0 = view object (ptr)
:r1 = longs (uint)
outputs
:r0 = view object (ptr)
:r1 = the first of them (plong)
trashes
:r1-:r3, :f0-:f15
info
the view's list of draws that many longer, made if it has none
```

### :set_color -> gui/ctx/set_color

```code
inputs
:r0 = view object (ptr)
:r1 = color (argb)
trashes
:r1-:r4, :f0-:f15
```

