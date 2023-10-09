# vdu

## view

### :create -> gui/vdu/create

### :deinit -> gui/vdu/deinit

```code
inputs
:r0 = vdu object (ptr)
outputs
:r0 = vdu object (ptr)
trashes
:r1-:r14
```

### :draw -> gui/vdu/draw

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :init -> gui/vdu/init

```code
inputs
:r0 = vdu object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = vdu object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :lisp_configure -> gui/vdu/lisp_configure

### (vdu-configure vdu)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_create -> gui/vdu/lisp_create

### (create-vdu)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_load -> gui/vdu/lisp_load

### (vdu-load vdu lines ox oy cx cy)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :vtable -> gui/vdu/vtable

