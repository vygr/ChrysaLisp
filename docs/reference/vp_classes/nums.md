# nums

## array

## Lisp Bindings

### (nums-abs nums [nums])

### (nums-add nums nums [nums])

### (nums-div nums nums [nums])

### (nums-dot nums nums)

### (nums-max nums nums [nums])

### (nums-min nums nums [nums])

### (nums-mod nums nums [nums])

### (nums-mul nums nums [nums])

### (nums-scale nums scale [nums])

### (nums-sub nums nums [nums])

### (nums-sum nums)

## VP methods

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

### :type -> class/nums/type

### :vcreate -> class/nums/create

### :vtable -> class/nums/vtable

