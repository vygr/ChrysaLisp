# :plist

## :list

## Lisp Bindings

### (perase plist key) -> plist

### (pfind plist key) -> :nil | val

### (pfindi plist key) -> :nil | idx

### (pinsert plist key val) -> plist

### (plist [key val] ...) -> plist

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
:r7 = iter_begin (pptr)
:r8 = iter_end (pptr)
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

### :type -> class/plist/type

### :vcreate -> class/plist/create

### :vtable -> class/plist/vtable

