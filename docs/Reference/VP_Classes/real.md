# real

## fixed

## VP methods

### :abs -> class/real/abs

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :add -> class/real/add

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :cos -> class/real/cos

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :create -> class/real/create

### :div -> class/real/div

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0 if error, else result real object (ptr)
trashes
:r1-:r14
```

### :floor -> class/real/floor

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :frac -> class/real/frac

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :ge -> class/real/ge

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :gt -> class/real/gt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :le -> class/real/le

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :lt -> class/real/lt

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0, -1 (int)
trashes
:r1-:r14
```

### :max -> class/real/max

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :min -> class/real/min

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :mod -> class/real/mod

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = 0 if error, else result real object (ptr)
trashes
:r1-:r14
```

### :mul -> class/real/mul

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :neg -> class/real/neg

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :print -> class/real/print

```code
inputs
:r0 = real object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = real object (ptr)
trashes
:r1-:r14
```

### :recip -> class/real/recip

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :sign -> class/real/sign

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :sin -> class/real/sin

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :sqrt -> class/real/sqrt

```code
inputs
:r0 = real object (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :sub -> class/real/sub

```code
inputs
:r0 = real object (ptr)
:r1 = list of real objects (ptr)
outputs
:r0 = real object (ptr)
:r1 = result real object (ptr)
trashes
:r1-:r14
```

### :vcreate -> class/real/create

### :vtable -> class/real/vtable

