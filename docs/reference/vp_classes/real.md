# :real

```image
docs/reference/vp_classes/real.cwb
```

## :fixed

## Lisp Bindings

### (quant real tol) -> real

## VP methods

### :abs -> class/real/abs

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :add -> class/real/add

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :ceil -> class/real/ceil

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :cos -> class/real/cos

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :create -> class/real/create

```code
inputs
:r0 = initial value (real)
outputs
:r0 = 0 if error, else real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :div -> class/real/div

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = 0 if error, else result real object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :floor -> class/real/floor

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :frac -> class/real/frac

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :ge -> class/real/ge

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :gt -> class/real/gt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :le -> class/real/le

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :lt -> class/real/lt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :max -> class/real/max

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = result real object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :min -> class/real/min

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = result real object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :mod -> class/real/mod

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = 0 if error, else result real object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :mul -> class/real/mul

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :neg -> class/real/neg

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :pow -> class/real/pow

```code
inputs
:r0 = real object (ptr)
:r1 = power real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
info
0 if the real is not greater than 0, :sys_math :r_pow
```

### :print -> class/real/print

```code
inputs
:r0 = real object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = real object (ptr)
trashes
:r1-:r5, :d0-:d15
```

### :quant -> class/real/quant

```code
inputs
:r0 = real object (ptr)
:r1 = tolerance real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :random -> class/real/random

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r4, :r14, :d0-:d15
```

### :recip -> class/real/recip

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sign -> class/real/sign

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sin -> class/real/sin

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sqrt -> class/real/sqrt

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sub -> class/real/sub

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :type -> class/real/type

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/real/create

```code
inputs
:r0 = initial value (real)
outputs
:r0 = 0 if error, else real object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/real/vtable

