# :fixeds

## :nums

## Lisp Bindings

### (fixeds-ceil fixeds [fixeds]) -> fixeds

### (fixeds-floor fixeds [fixeds]) -> fixeds

### (fixeds-frac fixeds [fixeds]) -> fixeds

### (mat3x3-mul ma mb [out]) -> fixeds

### (mat3x3-v3-mul ma v [out]) -> fixeds

### (mat4x4-mul ma mb [out]) -> fixeds

### (mat4x4-v4-mul ma v [out]) -> fixeds

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
:r1-:r6
```

### :mat3x3_mul -> class/fixeds/mat3x3_mul

```code
inputs
:r0 = output fixeds object (ptr)
:r1 = ma fixeds object (ptr)
:r2 = mb fixeds object (ptr)
outputs
:r0 = output fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r13
```

### :mat3x3_v3_mul -> class/fixeds/mat3x3_v3_mul

```code
inputs
:r0 = output fixeds object (ptr)
:r1 = matrix ma fixeds object (ptr)
:r2 = vector v fixeds object (ptr)
outputs
:r0 = output fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r13
```

### :mat4x4_mul -> class/fixeds/mat4x4_mul

```code
inputs
:r0 = output fixeds object (ptr)
:r1 = ma fixeds object (ptr)
:r2 = mb fixeds object (ptr)
outputs
:r0 = output fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r13
```

### :mat4x4_v4_mul -> class/fixeds/mat4x4_v4_mul

```code
inputs
:r0 = output fixeds object (ptr)
:r1 = matrix ma fixeds object (ptr)
:r2 = vector v fixeds object (ptr)
outputs
:r0 = output fixeds object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r13
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
:r1-:r6
```

### :vcreate -> class/fixeds/create

### :velement -> class/fixed/create

### :vtable -> class/fixeds/vtable

