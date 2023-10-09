# gui

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

### :lisp_deinit -> gui/gui/lisp_deinit

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
;(gui-deinit)
```

### :lisp_event -> gui/gui/lisp_event

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
;(gui-event)
```

### :lisp_info -> gui/gui/lisp_info

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
;(gui-info)
```

### :lisp_init -> gui/gui/lisp_init

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
;(gui-init screen)
```

### :lisp_update -> gui/gui/lisp_update

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
;(gui-update mx my flags)
```

### :update -> gui/gui/update

```code
inputs
:r0 = mouse x
:r1 = mouse y
:r2 = update flags
```

