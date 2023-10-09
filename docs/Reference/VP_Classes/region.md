# region

## VP methods

### :bounds -> gui/region/bounds

```code
inputs
:r1 = region listhead (ptr)
outputs
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
trashes
:r1, :r7-:r14
```

### :clip_rect -> gui/region/clip_rect

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r3, :r11-:r14
```

### :copy_rect -> gui/region/copy_rect

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r2 = dest region listhead (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r4, :r11-:r14
```

### :copy_region -> gui/region/copy_region

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r2 = dest region listhead (ptr)
:r3 = copy region listhead (ptr)
:r7 = x translation (pixels)
:r8 = y translation (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r14
```

### :cut_rect -> gui/region/cut_rect

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r2 = dest region listhead (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r6, :r11-:r14
```

### :free -> gui/region/free

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
outputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
trashes
:r2-:r4
```

### :paste_rect -> gui/region/paste_rect

```code
inputs
:r0 = region heap (ptr)
:r1 = dest region listhead (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r14
```

### :paste_region -> gui/region/paste_region

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r2 = dest region listhead (ptr)
:r7 = x translation (pixels)
:r8 = y translation (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r14
```

### :remove_rect -> gui/region/remove_rect

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = x1 (pixels)
:r10 = y1 (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r5, :r11-:r14
```

### :remove_region -> gui/region/remove_region

```code
inputs
:r0 = region heap (ptr)
:r1 = source region listhead (ptr)
:r2 = dest region listhead (ptr)
:r7 = x translation (pixels)
:r8 = y translation (pixels)
outputs
:r0 = region heap (ptr)
trashes
:r1-:r14
```

### :translate -> gui/region/translate

```code
inputs
:r1 = region listhead (ptr)
:r7 = x translation (pixels)
:r8 = y translation (pixels)
trashes
:r1, :r11-:r14
```

