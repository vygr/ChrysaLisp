# :float

```image
docs/reference/vp_classes/float.cwb
```

## :fixed

## VP methods

### :abs -> class/float/abs

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :add -> class/float/add

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :ceil -> class/float/ceil

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :cos -> class/float/cos

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :create -> class/float/create

```code
inputs
:r0 = initial value (the 32 bits of a float)
outputs
:r0 = 0 if error, else float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :div -> class/float/div

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r0 = 0 if error, else result float object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :exp -> class/float/exp

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :floor -> class/float/floor

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :frac -> class/float/frac

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :ge -> class/float/ge

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :gt -> class/float/gt

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :le -> class/float/le

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :log -> class/float/log

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :lt -> class/float/lt

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :max -> class/float/max

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = result float object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :min -> class/float/min

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r1 = result float object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :mod -> class/float/mod

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r0 = 0 if error, else result float object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :mul -> class/float/mul

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :neg -> class/float/neg

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :pow -> class/float/pow

```code
inputs
:r0 = float object (ptr)
:r1 = power float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
info
0 if the float is not greater than 0, :sys_math :d_pow
```

### :print -> class/float/print

```code
inputs
:r0 = float object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = float object (ptr)
trashes
:r1-:r5, :d0-:d15
```

### :random -> class/float/random

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r4, :r14, :d0-:d15
```

### :recip -> class/float/recip

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sign -> class/float/sign

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sin -> class/float/sin

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sqrt -> class/float/sqrt

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sub -> class/float/sub

```code
inputs
:r0 = float object (ptr)
:r1 = list of float objects (ptr)
outputs
:r0 = result float object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :type -> class/float/type

```code
inputs
:r0 = float object (ptr)
outputs
:r0 = float object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/float/create

```code
inputs
:r0 = initial value (the 32 bits of a float)
outputs
:r0 = 0 if error, else float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/float/vtable

