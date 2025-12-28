# :fixed

## :num

## Lisp Bindings

### (ceil fixed) -> fixed

### (cos fixed) -> fixed

### (floor fixed) -> fixed

### (frac fixed) -> fixed

### (recip fixed) -> fixed

### (sin fixed) -> fixed

## VP methods

### :ceil -> class/fixed/ceil

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14
```

### :cos -> class/fixed/cos

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14, :f0-:f3
```

### :create -> class/fixed/create

### :div -> class/fixed/div

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = 0 if error, else result fixed object (ptr)
trashes
:r1-:r14
```

### :floor -> class/fixed/floor

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14
```

### :frac -> class/fixed/frac

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14
```

### :mod -> class/fixed/mod

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = 0 if error, else result fixed object (ptr)
trashes
:r1-:r14
```

### :mul -> class/fixed/mul

```code
inputs
:r0 = fixed object (ptr)
:r1 = list of fixed objects (ptr)
outputs
:r0 = result fixed object (ptr)
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
:r0 = result fixed object (ptr)
trashes
:r0-:r14
```

### :sign -> class/fixed/sign

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14
```

### :sin -> class/fixed/sin

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14, :f0-:f3
```

### :sqrt -> class/fixed/sqrt

```code
inputs
:r0 = fixed object (ptr)
outputs
:r0 = result fixed object (ptr)
trashes
:r0-:r14, :f0-f1
```

### :vcreate -> class/fixed/create

### :vtable -> class/fixed/vtable

