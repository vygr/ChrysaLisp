# :nums

```image
docs/reference/vp_classes/nums.cwb
```

## :array

## Lisp Bindings

### (nums-abs nums [nums]) -> nums

### (nums-add nums nums [nums]) -> nums

### (nums-add-scaled nums nums scale [nums]) -> nums

### (nums-convert src dst) -> dst

### (nums-div nums nums [nums]) -> nums

### (nums-dot nums nums) -> num

### (nums-max nums nums [nums]) -> nums

### (nums-min nums nums [nums]) -> nums

### (nums-mod nums nums [nums]) -> nums

### (nums-mul nums nums [nums]) -> nums

### (nums-scale nums scale [nums]) -> nums

### (nums-sub nums nums [nums]) -> nums

### (nums-sum nums) -> num

## VP methods

### :abs -> class/nums/abs

```code
inputs
:r0 = nums object (ptr)
:r1 = source nums object, can be same (ptr)
outputs
:r0 = nums object (ptr)
trashes
:r1-:r5
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
:r1-:r7
```

### :add_scaled -> class/nums/add_scaled

```code
inputs
:r0 = nums object (ptr)
:r1 = source1 nums object, can be same (ptr)
:r2 = source2 nums object, can be same (ptr)
:r3 = scale (int)
outputs
:r0 = nums object (ptr)
trashes
:r1-:r2, :r4-:r8
info
each of the first plus each of the second times the scale, in one pass
```

### :convert -> class/nums/convert

```code
inputs
:r0 = nums object (ptr)
:r1 = source nums object, of any kind (ptr)
:r3 = 1 to read a raw source signed, else 0 (uint)
outputs
:r0 = nums object (ptr)
trashes
:r2-:r6, :d0-:d15
info
made the source's length, each element of it converted
```

### :create -> class/nums/create

```code
outputs
:r0 = 0 if error, else nums object (ptr)
trashes
:r0-:r2, :d0-:d15
```

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
:r1, :r3-:r8
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
:r1-:r7
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

### :kind -> class/nums/kind

```code
inputs
:r0 = nums object, or a bytes, shorts or ints (ptr)
outputs
:r0 = nums object (ptr)
:r1 = 0 nums, 1 fixeds, 2 doubles, 3 floats, 4 bytes, 6 shorts,
8 ints (uint)
trashes
:r1-:r2
info
the kind of vector, a path as fixeds
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
:r1-:r7
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
:r1-:r7
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
:r1, :r3-:r8
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
:r1-:r7
```

### :narrow -> class/nums/narrow

```code
inputs
:r0 = bytes, shorts or ints object (ptr)
:r1 = source nums object, of any kind of number (ptr)
outputs
:r0 = bytes, shorts or ints object (ptr)
trashes
:r1-:r7, :d0-:d15
info
made the source's length, each element an integer as n2i makes it,
its low bits
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
:r1, :r3-:r6
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
:r1-:r7
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

```code
inputs
:r0 = nums object (ptr)
outputs
:r0 = nums object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/nums/create

```code
outputs
:r0 = 0 if error, else nums object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :vtable -> class/nums/vtable

