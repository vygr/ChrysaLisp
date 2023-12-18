# vdu

## view

## Lisp Bindings

### (vdu-configure vdu)

### (create-vdu)

### (vdu-load vdu lines ox oy cx cy)

## VP methods

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

### :vtable -> gui/vdu/vtable

