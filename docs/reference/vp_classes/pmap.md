# :pmap

```image
docs/reference/vp_classes/pmap.cwb
```

## :pset

## Lisp Bindings

### (perase props key [key] ...) -> props

### (pfind props key) -> :nil | val

### (pinsert props key [val] ...) -> props

### (pmap [key val] ...) -> pmap

## VP methods

### :create -> class/pmap/create

```code
outputs
:r0 = 0 if error, else pmap object (ptr)
trashes
:r0-:r2, :f0-:f15
```

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

```code
inputs
:r0 = pmap object (ptr)
outputs
:r0 = pmap object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vcreate -> class/pmap/create

```code
outputs
:r0 = 0 if error, else pmap object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :vtable -> class/pmap/vtable

