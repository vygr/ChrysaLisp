# :pset

## :list

## Lisp Bindings

### (pfind props key) -> :nil | val

### (pfindi props key) -> :nil | idx

### (pset [key] ...) -> pset

## VP methods

### :create -> class/pset/create

### :pfind -> class/pset/find

```code
inputs
:r0 = pset object (ptr)
:r1 = key object (ptr)
outputs
:r0 = pset object (ptr)
:r1 = 0 if not found, else iter (pptr)
:r7 = iter_begin (pptr)
:r8 = iter_end (pptr)
trashes
:r1-:r10
```

### :pinsert -> class/pset/insert

```code
inputs
:r0 = pset object (ptr)
:r1 = key str object (ptr)
outputs
:r0 = pset object (ptr)
:r1 = iterator (pptr)
trashes
:r1-:r12, :f0-:f15
```

### :type -> class/pset/type

### :vcreate -> class/pset/create

### :vtable -> class/pset/vtable

