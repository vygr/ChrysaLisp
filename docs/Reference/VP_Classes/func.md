# func

## obj

### :create -> class/func/create

### :init -> class/num/init

```code
inputs
:r0 = num object (ptr)
:r1 = vtable (pptr)
:r2 = initial value (long)
outputs
:r0 = num object (ptr)
:r1 = 0 if error, else ok
trashes
:r1
```

### :print -> class/func/print

```code
inputs
:r0 = func object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = func object (ptr)
trashes
:r1-:r14
```

### :type -> class/func/type

### :vtable -> class/func/vtable

