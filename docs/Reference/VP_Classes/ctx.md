# ctx

## Lisp Bindings

### (ctx-blit view tid col x y w h)

### (ctx-box view x y w h)

### (ctx-filled-box view x y w h)

### (ctx-set-color view col)

## VP methods

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

### :set_color -> gui/ctx/set_color

```code
inputs
:r0 = view object (ptr)
:r1 = color (argb)
trashes
:r0-:r14
```

