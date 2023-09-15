# nums

## array

### :abs -> class/nums/abs

```code
inputs
:r0 = nums object (ptr)
:r1 = source nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
trashes
:r1-:r4
```

### :add -> class/nums/add

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :create -> class/nums/create

### :div -> class/nums/div

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7
```

### :dot -> class/nums/dot

```code
inputs
:r0 = nums object (ptr)
:r1 = nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = dot product (long)
trashes
:r1-:r6
```

### :eql -> class/nums/eql

```code
inputs
:r0 = nums object (ptr)
:r1 = nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if eql, else not (long)
trashes
:r1-:r6
```

### :hash -> class/nums/hash

```code
inputs
:r0 = nums object (ptr)
outputs
:r0 = nums object (ptr)
:r1 = hash code (long)
trashes
:r1-:r4
```

### :lisp_abs -> class/nums/lisp_abs

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

### :lisp_add -> class/nums/lisp_add

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

### :lisp_div -> class/nums/lisp_div

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

### :lisp_dot -> class/nums/lisp_dot

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

### :lisp_max -> class/nums/lisp_max

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

### :lisp_min -> class/nums/lisp_min

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

### :lisp_mod -> class/nums/lisp_mod

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

### :lisp_mul -> class/nums/lisp_mul

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

### :lisp_scale -> class/nums/lisp_scale

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

### :lisp_sub -> class/nums/lisp_sub

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

### :lisp_sum -> class/nums/lisp_sum

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

### :lisp_vecop1 -> class/nums/lisp_vecop1

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
:r2 = nums method (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = 0 if error, else value object (ptr)
trashes
:r1-:r14
```

### :lisp_vecop2 -> class/nums/lisp_vecop2

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
:r2 = nums method (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = 0 if error, else value object (ptr)
trashes
:r1-:r14
```

### :max -> class/nums/max

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :min -> class/nums/min

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :mod -> class/nums/mod

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r7
```

### :mul -> class/nums/mul

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :scale -> class/nums/scale

```code
inputs
:r0 = nums object (ptr)
:r1 = source nums object, can be same (ptr)
:r2 = scale (int)
outputs
:r0 = nums object (ptr)
trashes
:r1-:r5
```

### :sub -> class/nums/sub

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :sum -> class/nums/sum

```code
inputs
:r0 = nums object (ptr)
outputs
:r0 = nums object (ptr)
:r1 = sum (long)
trashes
:r1-:r4
```

### :vcreate -> class/nums/create

### :vtable -> class/nums/vtable

