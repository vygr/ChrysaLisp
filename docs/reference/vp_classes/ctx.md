# :ctx

## Lisp Bindings

### (ctx-blit view tid col x y w h)

### (ctx-box view x y w h)

### (ctx-filled-box view x y w h)

### (ctx-filled-region view region)

### (ctx-set-color view col)

## VP methods

### :blit -> gui/ctx/blit

```code
inputs
:r0 = view object (ptr)
:r1 = texture id (long)
:r2 = color mod (argb)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r4, :r7-:r14, :f0-:f15
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
:r0, :r3, :r7-:r14, :f0-:f15
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
:r0, :r3, :r7-:r14, :f0-:f15
```

### :set_color -> gui/ctx/set_color

```code
inputs
:r0 = view object (ptr)
:r1 = color (argb)
trashes
:r0-:r3, :r13-:r14, :f0-:f15
```

