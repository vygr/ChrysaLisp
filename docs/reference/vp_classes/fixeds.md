# :fixeds

```image
docs/reference/vp_classes/fixeds.cwb
```

## :nums

## Lisp Bindings

### (fixeds-ceil fixeds [fixeds]) -> fixeds

### (fixeds-floor fixeds [fixeds]) -> fixeds

### (fixeds-frac fixeds [fixeds]) -> fixeds

## VP methods

### :ceil -> class/fixeds/ceil

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
trashes
:r1-:r6
```

### :create -> class/fixeds/create

```code
outputs
:r0 = 0 if error, else fixeds object (ptr)
trashes
:r0-:r2, :f0-:f15
```

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
:r1-:r8
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
:r1-:r7
```

### :floor -> class/fixeds/floor

```code
inputs
:r0 = fixeds object (ptr)
:r1 = source fixeds object, can be same (ptr)
outputs
:r0 = fixeds object (ptr)
trashes
:r1-:r5
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
:r1-:r9
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
:r1-:r7
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
:r1, :r3-:r6
```

### :type -> class/fixeds/type

```code
inputs
:r0 = fixeds object (ptr)
outputs
:r0 = fixeds object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vcreate -> class/fixeds/create

```code
outputs
:r0 = 0 if error, else fixeds object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :velement -> class/fixed/create

```code
inputs
:r0 = initial value (fixed)
outputs
:r0 = 0 if error, else fixed object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :vtable -> class/fixeds/vtable

