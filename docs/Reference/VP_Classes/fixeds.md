# fixeds

## nums

### :create -> class/fixeds/create

### :div -> class/fixeds/div

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source1 fixeds object, can be same (ptr)
:r2 = source2 fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7
```

### :dot -> class/fixeds/dot

```code
inputs
:r0 = fixeds object (ptr)
:r1 = fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
:r1 = dot product (fixed)
trashes
:r1-:r6
```

### :floor -> class/fixeds/floor

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
trashes
:r1-:r4
```

### :frac -> class/fixeds/frac

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
trashes
:r1-:r5
```

### :lisp_floor -> class/fixeds/lisp_floor

### (fixeds-floor fixeds [fixeds])

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

### :lisp_frac -> class/fixeds/lisp_frac

### (fixeds-frac fixeds [fixeds])

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

### :mod -> class/fixeds/mod

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source1 fixeds object, can be same (ptr)
:r2 = source2 fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r8
```

### :mul -> class/fixeds/mul

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source1 fixeds object, can be same (ptr)
:r2 = source2 fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :scale -> class/fixeds/scale

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source fixeds object, can be same (ptr)
:r2 = scale (fixed)
outputs
:r0 = fixeds object (ptr)
trashes
:r1-:r5
```

### :vcreate -> class/fixeds/create

### :velement -> class/fixed/create

### :vtable -> class/fixeds/vtable

