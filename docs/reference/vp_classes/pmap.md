# :pmap

## :pset

## Lisp Bindings

### (perase props key) -> props

### (pfind props key) -> :nil | val

### (pinsert props key [val]) -> props

### (pmap [key val] ...) -> pmap

## VP methods

### :create -> class/pmap/create

### :pfind -> class/pmap/find

```code
inputs
:r0 = pmap object (ptr)
:r1 = key object (ptr)
outputs
:r0 = pmap object (ptr)
:r1 = 0 if not found, else iter (pptr)
:r7 = iter_begin (pptr)
:r8 = iter_end (pptr)
trashes
:r1-:r10
```

### :pinsert -> class/pmap/insert

```code
inputs
:r0 = pmap object (ptr)
:r1 = key str object (ptr)
:r2 = value object (ptr)
outputs
:r0 = pmap object (ptr)
:r1 = iterator (pptr)
trashes
:r1-:r14, :f0-:f15
```

### :type -> class/pmap/type

### :vcreate -> class/pmap/create

### :vtable -> class/pmap/vtable

