# fixed

## num

### :cos -> class/fixed/cos

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :create -> class/fixed/create

### :div -> class/fixed/div

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = 0 if error, else result fixed object (ptr)
trashes
:r1-:r14
```

### :floor -> class/fixed/floor

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :frac -> class/fixed/frac

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :lisp_cos -> class/fixed/lisp_cos

### (cos angle)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_floor -> class/fixed/lisp_floor

### (floor num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_frac -> class/fixed/lisp_frac

### (frac num)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_recip -> class/fixed/lisp_recip

### (recip fixed)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_sin -> class/fixed/lisp_sin

### (sin angle)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :mod -> class/fixed/mod

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = 0 if error, else result fixed object (ptr)
trashes
:r1-:r14
```

### :mul -> class/fixed/mul

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :print -> class/fixed/print

```code
inputs
:r0 = fixed object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = fixed object (ptr)
trashes
:r1-:r14
```

### :recip -> class/fixed/recip

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :sign -> class/fixed/sign

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = 0 if error, else result fixed object (ptr)
trashes
:r1-:r14
```

### :sin -> class/fixed/sin

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :sqrt -> class/fixed/sqrt

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = fixed object (ptr)
:r1 = result fixed object (ptr)
trashes
:r1-:r14
```

### :vcreate -> class/fixed/create

### :vtable -> class/fixed/vtable

