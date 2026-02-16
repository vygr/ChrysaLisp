# :reals

## :fixeds

## Lisp Bindings

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
:r1-:r4, :f0
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
:r1-:r5, :f0-:f1
```

### :ceil -> class/reals/ceil

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :f0-:f2
```

### :create -> class/reals/create

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
:r1-:r5, :f0-:f2
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
:r1-:r4, :f0-:f1
```

### :floor -> class/reals/floor

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :f0-:f2
```

### :frac -> class/reals/frac

```code
inputs
:r0 = reals object (ptr)
:r1 = source reals object, can be same (ptr)
outputs
:r0 = reals object (ptr)
trashes
:r1-:r5, :f0-:f3
```

### :mat3x3_mul -> class/reals/mat3x3_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = ma reals object (ptr)
:r2 = mb reals object (ptr)
outputs
:r0 = output reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r10, :f0-:f15
```

### :mat3x3_v3_mul -> class/reals/mat3x3_v3_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = matrix ma reals object (ptr)
:r2 = vector v reals object (ptr)
outputs
:r0 = output reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r10, :f0-:f7
```

### :mat4x4_mul -> class/reals/mat4x4_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = ma reals object (ptr)
:r2 = mb reals object (ptr)
outputs
:r0 = output reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r10, :f0-:f15
```

### :mat4x4_v4_mul -> class/reals/mat4x4_v4_mul

```code
inputs
:r0 = output reals object (ptr)
:r1 = matrix ma reals object (ptr)
:r2 = vector v reals object (ptr)
outputs
:r0 = output reals object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r10, :f0-:f7
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
:r1-:r5, :f0-:f1
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
:r1-:r5, :f0-:f1
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
:r1-:r6, :f0-:f3
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
:r1-:r5, :f0-:f1
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
:r1-:r5, :f0-:f4
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
:r1-:r4, :f0-:f1
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
:r1-:r5, :f0-:f1
```

### :sum -> class/reals/sum

```code
inputs
:r0 = reals object (ptr)
outputs
:r0 = reals object (ptr)
:r1 = sum (real)
trashes
:r1-:r2, :f0-:f1
```

### :vcreate -> class/reals/create

### :velement -> class/real/create

### :vtable -> class/reals/vtable

