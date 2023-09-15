# texture

## obj

### :create -> gui/texture/create

### :deinit -> gui/texture/deinit

```code
inputs
:r0 = texture object (ptr)
outputs
:r0 = texture object (ptr)
trashes
:r1-:r14
```

### :get_metrics -> gui/texture/get_metrics

```code
inputs
:r0 = texture object (ptr)
outputs
:r0 = texture object (ptr)
:r1 = texture handle (ulong)
:r2 = width (pixels)
:r3 = height (pixels)
trashes
:r1-:r3
```

### :init -> gui/texture/init

```code
inputs
:r0 = texture object (ptr)
:r1 = vtable (pptr)
:r2 = texture handle (ulong)
:r3 = texture width (pixels)
:r4 = texture height (pixels)
outputs
:r0 = texture object (ptr)
:r1 = 0 if error, else ok
trashes
:r1
```

### :vtable -> gui/texture/vtable

