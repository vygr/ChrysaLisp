# :double

```image
docs/reference/vp_classes/double.cwb
```

## :fixed

## Lisp Bindings

### (acos double) -> double

### (asin double) -> double

### (atan double) -> double

### (atan2 y x) -> double

### (exp double) -> double

### (log double) -> double

### (quant double tol) -> double

### (tanh double) -> double

## VP methods

### :abs -> class/double/abs

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :acos -> class/double/acos

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :add -> class/double/add

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :asin -> class/double/asin

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :atan -> class/double/atan

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :atan2 -> class/double/atan2

```code
inputs
:r0 = y double object (ptr)
:r1 = x double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
info
the angle of the point x y, -pi to pi, :sys_math :d_atan2
```

### :ceil -> class/double/ceil

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :cos -> class/double/cos

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :create -> class/double/create

```code
inputs
:r0 = initial value (double)
outputs
:r0 = 0 if error, else double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :div -> class/double/div

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r0 = 0 if error, else result double object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :eq -> class/double/eq

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :exp -> class/double/exp

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :floor -> class/double/floor

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :frac -> class/double/frac

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :ge -> class/double/ge

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :gt -> class/double/gt

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :le -> class/double/le

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :log -> class/double/log

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :lt -> class/double/lt

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :d0-:d1
```

### :max -> class/double/max

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = result double object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :min -> class/double/min

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = result double object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :mod -> class/double/mod

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r0 = 0 if error, else result double object (ptr)
trashes
:r0-:r5, :r14, :d0-:d15
```

### :mul -> class/double/mul

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :ne -> class/double/ne

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r5, :d0-:d1
```

### :neg -> class/double/neg

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :pow -> class/double/pow

```code
inputs
:r0 = double object (ptr)
:r1 = power double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
info
0 if the double is not greater than 0, :sys_math :d_pow
```

### :print -> class/double/print

```code
inputs
:r0 = double object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = double object (ptr)
trashes
:r1-:r5, :d0-:d15
```

### :quant -> class/double/quant

```code
inputs
:r0 = double object (ptr)
:r1 = tolerance double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :random -> class/double/random

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r4, :r14, :d0-:d15
```

### :recip -> class/double/recip

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sign -> class/double/sign

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sin -> class/double/sin

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sqrt -> class/double/sqrt

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :sub -> class/double/sub

```code
inputs
:r0 = double object (ptr)
:r1 = list of double objects (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r4, :r14, :d0-:d15
```

### :tanh -> class/double/tanh

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = result double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :type -> class/double/type

```code
inputs
:r0 = double object (ptr)
outputs
:r0 = double object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/double/create

```code
inputs
:r0 = initial value (double)
outputs
:r0 = 0 if error, else double object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/double/vtable

