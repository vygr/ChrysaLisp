# :plist

## :list

## Lisp Bindings

### (pfind list key) -> :nil | val

### (pinsert list key val) -> list

## VP methods

### :create -> class/plist/create

### :pfind -> class/plist/find

```code
inputs
:r0 = plist object (ptr)
:r1 = key object (ptr)
outputs
:r0 = plist object (ptr)
:r1 = 0 if not found, else iter (pptr)
trashes
:r1-:r10
```

### :pinsert -> class/plist/insert

```code
inputs
:r0 = plist object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = plist object (ptr)
:r1 = iterator (pptr)
trashes
:r1-:r14, :f0-:f15
```

### :vtable -> class/plist/vtable

