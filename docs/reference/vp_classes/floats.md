# :floats

```image
docs/reference/vp_classes/floats.cwb
```

## :doubles

## Lisp Bindings

### (floats [float ...]) -> floats

## VP methods

### :abs -> class/floats/abs

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r4, :d0
```

### :add -> class/floats/add

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :add_scaled -> class/floats/add_scaled

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
:r3 = scale (the 32 bits of a float)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r5, :d0-:d2
info
each of the first plus each of the second times the scale, in one pass
```

### :append -> class/floats/append

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
```

### :cat -> class/floats/cat

```code
inputs
:r0 = floats object (ptr)
:r1 = list of floats objects (ptr)
outputs
:r0 = new floats object (ptr)
trashes
:r0-:r9, :d0-:d15
```

### :ceil -> class/floats/ceil

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :convert -> class/floats/convert

```code
inputs
:r0 = floats object (ptr)
:r1 = source nums object, of any kind (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r2-:r6, :d0-:d15
info
made the source's length, each element of it converted
```

### :create -> class/floats/create

```code
outputs
:r0 = 0 if error, else floats object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :div -> class/floats/div

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d2
```

### :dot -> class/floats/dot

```code
inputs
:r0 = floats object (ptr)
:r1 = floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = dot product (the 32 bits of a float)
trashes
:r1-:r4, :d0-:d5
info
four running sums, as doubles, over blocks of four, the rest into the
first, then the four added and rounded to the nearest float
```

### :eql -> class/floats/eql

```code
inputs
:r0 = floats object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r5
```

### :erase -> class/floats/erase

```code
inputs
:r0 = floats object (ptr)
:r1 = element iterator (pint)
outputs
:r0 = floats object (ptr)
:r1 = element iterator (pint)
trashes
:r2-:r3
```

### :erase2 -> class/floats/erase2

```code
inputs
:r0 = floats object (ptr)
:r1 = element iterator (pint)
outputs
:r0 = floats object (ptr)
:r1 = element iterator (pint)
trashes
:r2-:r3
```

### :fill -> class/floats/fill

```code
inputs
:r0 = floats object (ptr)
:r1 = length (uint)
:r2 = element (the 32 bits of a float)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r4, :d0-:d15
info
the floats made length long, every element the one given
```

### :find -> class/floats/find

```code
inputs
:r0 = floats object (ptr)
:r1 = element (the 32 bits of a float)
:r2 = start index (uint)
outputs
:r0 = floats object (ptr)
:r1 = element (the 32 bits of a float)
:r2 = -1, else index (int)
trashes
:r2-:r4
```

### :floor -> class/floats/floor

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r5, :d0-:d2
```

### :frac -> class/floats/frac

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r5, :d0-:d3
```

### :hash -> class/floats/hash

```code
inputs
:r0 = floats object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = hash code (long)
trashes
:r1-:r4
```

### :max -> class/floats/max

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :min -> class/floats/min

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :mod -> class/floats/mod

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6, :d0-:d3
```

### :mul -> class/floats/mul

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :print -> class/floats/print

```code
inputs
:r0 = floats object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r5, :r14, :d0-:d15
```

### :push -> class/floats/push

```code
inputs
:r0 = floats object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r3, :r6-:r9, :r11-:r13, :d0-:d15
info
each of the args after the first onto the end, raw as push has them
for the other arrays: a number the low 32 bits of its value, a floats
its elements, any other array or a str its bytes, 4 to an element
```

### :ref_back -> class/floats/ref_back

```code
inputs
:r0 = floats object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = float object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
```

### :ref_elem -> class/floats/ref_elem

```code
inputs
:r0 = floats object (ptr)
:r1 = element index (uint)
outputs
:r0 = floats object (ptr)
:r1 = float object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
```

### :rfind -> class/floats/rfind

```code
inputs
:r0 = floats object (ptr)
:r1 = element (the 32 bits of a float)
:r2 = start index (uint)
outputs
:r0 = floats object (ptr)
:r1 = element (the 32 bits of a float)
:r2 = 0, else index (int)
trashes
:r2-:r4
```

### :scale -> class/floats/scale

```code
inputs
:r0 = floats object (ptr)
:r1 = source floats object, can be same (ptr)
:r2 = scale (the 32 bits of a float)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r4, :d0-:d1
```

### :set_cap -> class/floats/set_cap

```code
inputs
:r0 = floats object (ptr)
:r1 = capacity (uint)
outputs
:r0 = floats object (ptr)
trashes
:r1-:r2, :d0-:d15
info
a capacity of floats, two to a slot of the array
```

### :set_elem -> class/floats/set_elem

```code
inputs
:r0 = floats object (ptr)
:r1 = element (the 32 bits of a float)
:r2 = element index (uint)
outputs
:r0 = floats object (ptr)
trashes
:r2-:r3
```

### :slice -> class/floats/slice

```code
inputs
:r0 = floats object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice floats object (ptr)
trashes
:r0-:r2, :r6-:r8, :d0-:d15
```

### :splice -> class/floats/splice

```code
inputs
:r0 = src1 floats object (ptr)
:r1 = src2 floats object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = new floats object (ptr)
trashes
:r0-:r11, :d0-:d15
```

### :sub -> class/floats/sub

```code
inputs
:r0 = floats object (ptr)
:r1 = source1 floats object, can be same (ptr)
:r2 = source2 floats object, can be same (ptr)
outputs
:r0 = floats object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :d0-:d1
```

### :sum -> class/floats/sum

```code
inputs
:r0 = floats object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = sum (the 32 bits of a float)
trashes
:r1-:r3, :d0-:d4
info
four running sums, as doubles, over blocks of four, the rest into the
first, then the four added and rounded to the nearest float
```

### :type -> class/floats/type

```code
inputs
:r0 = floats object (ptr)
outputs
:r0 = floats object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/floats/create

```code
outputs
:r0 = 0 if error, else floats object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :velement -> class/float/create

```code
inputs
:r0 = initial value (the 32 bits of a float)
outputs
:r0 = 0 if error, else float object (ptr)
trashes
:r0-:r2, :r14, :d0-:d15
```

### :vtable -> class/floats/vtable

