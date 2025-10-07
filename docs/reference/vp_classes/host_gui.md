# host_gui

## Lisp Bindings

### (gui-deinit)

### (gui-event)

### (gui-info)

### (gui-init screen)

### (gui-update mx my flags)

## VP methods

### :begin_composite -> :nil

### :blit -> :nil

### :box -> :nil

### :clip_free -> :nil

### :clip_get -> :nil

### :clip_put -> :nil

### :composite -> service/gui/composite

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

### :create_texture -> :nil

### :deinit -> :nil

### :destroy_texture -> :nil

### :end_composite -> :nil

### :filled_box -> :nil

### :flush -> :nil

### :init -> :nil

### :poll_event -> :nil

### :resize -> :nil

### :set_clip -> :nil

### :set_color -> :nil

### :set_texture_color -> :nil

### :update -> service/gui/update

```code
inputs
:r0 = mouse x
:r1 = mouse y
:r2 = update flags
```

