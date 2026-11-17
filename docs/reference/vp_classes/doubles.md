# :doubles

```image
docs/reference/vp_classes/doubles.cwb
```

## :fixeds

## Lisp Bindings

### (mat4x4-mul doubles doubles [doubles]) -> doubles

### (mat4x4-vec3-mul doubles doubles [doubles]) -> doubles

### (mat4x4-vec4-mul doubles doubles [doubles]) -> doubles

### (doubles-quant doubles tol [doubles]) -> doubles

## VP methods

### :abs -> class/doubles/abs

```code
inputs
:r0 = doubles object (ptr)
:r1 = source doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r4, :d0
```

### :add -> class/doubles/add

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :add_scaled -> class/doubles/add_scaled

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
:r3 = scale (double)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r5, :d0-:d2
info
each of the first plus each of the second times the scale, in one
pass, a multiply then an add, so rounded twice as by scale then add
```

### :ceil -> class/doubles/ceil

```code
inputs
:r0 = doubles object (ptr)
:r1 = source doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :create -> class/doubles/create

```code
outputs
:r0 = 0 if error, else doubles object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :div -> class/doubles/div

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d2
```

### :dot -> class/doubles/dot

```code
inputs
:r0 = doubles object (ptr)
:r1 = doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = dot product (double)
trashes
:r1-:r4, :d0-:d5
info
four running sums over blocks of four, the rest into the first, then
the four added: no add waits on the one before it, so a long vector
goes as fast as it can be loaded
```

### :floor -> class/doubles/floor

```code
inputs
:r0 = doubles object (ptr)
:r1 = source doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :frac -> class/doubles/frac

```code
inputs
:r0 = doubles object (ptr)
:r1 = source doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r5, :d0-:d3
```

### :mat4x4_mul -> class/doubles/mat4x4_mul

```code
inputs
:r0 = output doubles object (ptr)
:r1 = ma doubles object (ptr)
:r2 = mb doubles object (ptr)
outputs
:r0 = output doubles object (ptr)
trashes
:r1-:r3, :d0-:d12
```

### :mat4x4_v3_mul -> class/doubles/mat4x4_v3_mul

```code
inputs
:r0 = output doubles object (ptr)
:r1 = matrix ma doubles object (ptr)
:r2 = vector doubles object (ptr)
outputs
:r0 = output doubles object (ptr)
trashes
:r1-:r4, :d0-:d14
```

### :mat4x4_v4_mul -> class/doubles/mat4x4_v4_mul

```code
inputs
:r0 = output doubles object (ptr)
:r1 = matrix ma doubles object (ptr)
:r2 = vector doubles object (ptr)
outputs
:r0 = output doubles object (ptr)
trashes
:r1-:r4, :d0-:d15
```

### :max -> class/doubles/max

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :min -> class/doubles/min

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :mod -> class/doubles/mod

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6, :d0-:d3
```

### :mul -> class/doubles/mul

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :quant -> class/doubles/quant

```code
inputs
:r0 = doubles object (dst)
:r1 = source doubles object (src)
:r2 = tolerance double (double)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r4, :d0-:d4
```

### :scale -> class/doubles/scale

```code
inputs
:r0 = doubles object (ptr)
:r1 = source doubles object, can be same (ptr)
:r2 = scale (double)
outputs
:r0 = doubles object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :sub -> class/doubles/sub

```code
inputs
:r0 = doubles object (ptr)
:r1 = source1 doubles object, can be same (ptr)
:r2 = source2 doubles object, can be same (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :sum -> class/doubles/sum

```code
inputs
:r0 = doubles object (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = sum (double)
trashes
:r1-:r3, :d0-:d4
info
four running sums over blocks of four, the rest into the first, then
the four added: no add waits on the one before it, so a long vector
goes as fast as it can be loaded
```

### :type -> class/doubles/type

```code
inputs
:r0 = doubles object (ptr)
outputs
:r0 = doubles object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/doubles/create

```code
outputs
:r0 = 0 if error, else doubles object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :velement -> class/double/create

```code
inputs
:r0 = initial value (double)
outputs
:r0 = 0 if error, else double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/doubles/vtable

