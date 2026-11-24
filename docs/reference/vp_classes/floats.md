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

### :append -> class/ints/append

```code
the append of :array at 4 bytes
```

### :cat -> class/ints/cat

```code
the cat of :array at 4 bytes
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
:r3 = 1 to read a raw source signed, else 0 (uint)
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

### :eql -> class/ints/eql

```code
the eql of :array at 4 bytes
```

### :erase -> class/ints/erase

```code
the erase of :array at 4 bytes
```

### :erase2 -> class/ints/erase2

```code
the erase2 of :array at 4 bytes
```

### :fill -> class/ints/fill

```code
the fill of :array at 4 bytes
```

### :find -> class/ints/find

```code
the find of :array at 4 bytes
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

### :hash -> class/ints/hash

```code
the hash of :array at 4 bytes
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

### :print -> class/ints/print

```code
the print of :array at 4 bytes
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

### :ref_back -> class/ints/ref_back

```code
the ref_back of :array at 4 bytes
```

### :ref_elem -> class/ints/ref_elem

```code
the ref_elem of :array at 4 bytes
```

### :rfind -> class/ints/rfind

```code
the rfind of :array at 4 bytes
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

### :set_cap -> class/ints/set_cap

```code
the set_cap of :array at 4 bytes
```

### :set_elem -> class/ints/set_elem

```code
the set_elem of :array at 4 bytes
```

### :slice -> class/ints/slice

```code
the slice of :array at 4 bytes
```

### :splice -> class/ints/splice

```code
the splice of :array at 4 bytes
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

