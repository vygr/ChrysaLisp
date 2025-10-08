# fixeds

## nums

## Lisp Bindings

### (fixeds-floor fixeds [fixeds]) -> fixeds

### (fixeds-frac fixeds [fixeds]) -> fixeds

## VP methods

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

### :type -> class/fixeds/type

### :vcreate -> class/fixeds/create

### :velement -> class/fixed/create

### :vtable -> class/fixeds/vtable

