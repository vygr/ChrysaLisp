# :real

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
:r0-:r2, :r14, :f0-:f15
```

### :add -> class/real/add

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r1-:r14, :f0-:f1
```

### :ceil -> class/real/ceil

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :cos -> class/real/cos

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :create -> class/real/create

### :div -> class/real/div

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = 0 if error, else result real object (ptr)
trashes
:r1-:r14, :f0-:f3
```

### :floor -> class/real/floor

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :frac -> class/real/frac

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :ge -> class/real/ge

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :f0-:f1
```

### :gt -> class/real/gt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :f0-:f1
```

### :le -> class/real/le

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :f0-:f1
```

### :lt -> class/real/lt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = 0, -1 (int)
trashes
:r1-:r3, :f0-:f1
```

### :max -> class/real/max

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = result real object (ptr)
trashes
:r1-:r4, :f0-:f1
```

### :min -> class/real/min

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r1 = result real object (ptr)
trashes
:r1-:r4, :f0-:f1
```

### :mod -> class/real/mod

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = 0 if error, else result real object (ptr)
trashes
:r1-:r14, :f0-:f3
```

### :mul -> class/real/mul

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r1-:r14, :f0-:f1
```

### :neg -> class/real/neg

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :print -> class/real/print

```code
inputs
:r0 = real object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = real object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :quant -> class/real/quant

```code
inputs
:r0 = real object (ptr)
:r1 = tolerance real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :random -> class/real/random

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r4, :r14, :f0-:f15
```

### :recip -> class/real/recip

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :sign -> class/real/sign

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :sin -> class/real/sin

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :sqrt -> class/real/sqrt

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r0-:r2, :r14, :f0-:f15
```

### :sub -> class/real/sub

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = result real object (ptr)
trashes
:r1-:r14, :f0-:f1
```

### :vcreate -> class/real/create

### :vtable -> class/real/vtable

