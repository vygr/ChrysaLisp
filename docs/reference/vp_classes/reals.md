# :reals

```image
docs/reference/vp_classes/reals.cwb
```

## :fixeds

## Lisp Bindings

### (mat4x4-mul reals reals [reals]) -> reals

### (mat4x4-vec3-mul reals reals [reals]) -> reals

### (mat4x4-vec4-mul reals reals [reals]) -> reals

### (reals-quant reals tol [reals]) -> reals

## VP methods

### :abs -> class/reals/abs

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r4, :d0
```

### :add -> class/reals/add

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :ceil -> class/reals/ceil

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :create -> class/reals/create

```code
outputs
:r0 = 0 if error, else reals object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :div -> class/reals/div

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d2
```

### :dot -> class/reals/dot

```code
inputs
:r0 = reals object (ptr)
:r1 = reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = dot product (real)
trashes
:r1-:r4, :d0-:d2
```

### :floor -> class/reals/floor

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :frac -> class/reals/frac

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :d0-:d3
```

### :mat4x4_mul -> class/reals/mat4x4_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = ma reals object (ptr)
:r2 = mb reals object (ptr)
outputs
:r0 = output reals object (ptr)
trashes
:r1-:r3, :d0-:d12
```

### :mat4x4_v3_mul -> class/reals/mat4x4_v3_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = matrix ma reals object (ptr)
:r2 = vector reals object (ptr)
outputs
:r0 = output reals object (ptr)
trashes
:r1-:r4, :d0-:d14
```

### :mat4x4_v4_mul -> class/reals/mat4x4_v4_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = matrix ma reals object (ptr)
:r2 = vector reals object (ptr)
outputs
:r0 = output reals object (ptr)
trashes
:r1-:r4, :d0-:d15
```

### :max -> class/reals/max

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :min -> class/reals/min

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :mod -> class/reals/mod

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6, :d0-:d3
```

### :mul -> class/reals/mul

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :quant -> class/reals/quant

```code
inputs
:r0 = reals object (dst)
:r1 = source reals object (src)
:r2 = tolerance real (real)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r4, :d0-:d4
```

### :scale -> class/reals/scale

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
:r2 = scale (real)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :sub -> class/reals/sub

```code
inputs
:r0 = reals object (ptr)
:r1 = source1 reals object, can be same (ptr)
:r2 = source2 reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :sum -> class/reals/sum

```code
inputs
:r0 = reals object (ptr)
outputs
:r0 = reals object (ptr)
:r1 = sum (real)
trashes
:r1-:r3, :d0-:d1
```

### :type -> class/reals/type

```code
inputs
:r0 = reals object (ptr)
outputs
:r0 = reals object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/reals/create

```code
outputs
:r0 = 0 if error, else reals object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :velement -> class/real/create

```code
inputs
:r0 = initial value (real)
outputs
:r0 = 0 if error, else real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/reals/vtable

