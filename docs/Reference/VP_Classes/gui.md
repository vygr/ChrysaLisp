# gui

## Lisp Bindings

### (gui-deinit)

### (gui-event)

### (gui-info)

### (gui-init screen)

### (gui-update mx my flags)

## VP methods

### :composite -> gui/gui/composite

```code
inputs
:r0 = root view object (ptr)
outputs
:r7 = x (pixels)
:r8 = y (pixels)
:r9 = width (pixels)
:r10 = height (pixels)
trashes
:r0-:r14
info
the dirty region bounds is returned for use by the :flush call !
```

### :update -> gui/gui/update

```code
inputs
:r0 = mouse x
:r1 = mouse y
:r2 = update flags
```

