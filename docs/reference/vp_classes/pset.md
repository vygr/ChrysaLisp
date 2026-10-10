# :pset

```image
docs/reference/vp_classes/pset.cwb
```

## :list

## Lisp Bindings

### (pfindi props key) -> :nil | idx

### (pset [key] ...) -> pset

## VP methods

### :create -> class/pset/create

```code
outputs
:r0 = 0 if error, else pset object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :eql -> class/obj/eql

```code
inputs
:r0 = obj object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = obj object (ptr)
:r1 = 0 if same, else not
trashes
:r1
```

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

```code
inputs
:r0 = pset object (ptr)
outputs
:r0 = pset object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vcreate -> class/pset/create

```code
outputs
:r0 = 0 if error, else pset object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :vtable -> class/pset/vtable

