# :ints

```image
docs/reference/vp_classes/ints.cwb
```

## :array

## Lisp Bindings

### (ints [num ...]) -> ints

## VP methods

### :append -> class/ints/append

```code
inputs
:r0 = ints object (ptr)
:r1 = source array object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = ints object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the append of :array at 4 bytes
```

### :cat -> class/ints/cat

```code
inputs
:r0 = ints object (ptr)
:r1 = list of array objects (ptr)
outputs
:r0 = new ints object (ptr)
trashes
:r0-:r9, :d0-:d15
info
the cat of :array at 4 bytes
```

### :create -> class/ints/create

```code
outputs
:r0 = 0 if error, else ints object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :eql -> class/ints/eql

```code
inputs
:r0 = ints object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = ints object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r5
info
the eql of :array at 4 bytes
```

### :erase -> class/ints/erase

```code
inputs
:r0 = ints object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = ints object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase of :array at 4 bytes
```

### :erase2 -> class/ints/erase2

```code
inputs
:r0 = ints object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = ints object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase2 of :array at 4 bytes
```

### :fill -> class/ints/fill

```code
inputs
:r0 = ints object (ptr)
:r1 = length (uint)
:r2 = element (long)
outputs
:r0 = ints object (ptr)
trashes
:r1-:r4, :d0-:d15
info
the fill of :array at 4 bytes
```

### :find -> class/ints/find

```code
inputs
:r0 = ints object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = ints object (ptr)
:r1 = element (long)
:r2 = -1, else index (int)
trashes
:r2-:r4
info
the find of :array at 4 bytes
```

### :get_raw -> class/ints/get_raw

```code
inputs
:r0 = ints object (ptr)
outputs
:r0 = ints object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
trashes
:r1-:r3
info
the get_raw of :array at 4 bytes
```

### :hash -> class/ints/hash

```code
inputs
:r0 = ints object (ptr)
outputs
:r0 = ints object (ptr)
:r1 = hash code (long)
trashes
:r1-:r4
info
the hash of :array at 4 bytes
```

### :print -> class/ints/print

```code
inputs
:r0 = ints object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = ints object (ptr)
trashes
:r1-:r5, :r14, :d0-:d15
info
the print of :array at 4 bytes
```

### :push_raw -> class/ints/push_raw

```code
inputs
:r0 = ints object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
outputs
:r0 = ints object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the push_raw of :array at 4 bytes
```

### :ref_back -> class/ints/ref_back

```code
inputs
:r0 = ints object (ptr)
outputs
:r0 = ints object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_back of :array at 4 bytes
```

### :ref_elem -> class/ints/ref_elem

```code
inputs
:r0 = ints object (ptr)
:r1 = element index (uint)
outputs
:r0 = ints object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_elem of :array at 4 bytes
```

### :rfind -> class/ints/rfind

```code
inputs
:r0 = ints object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = ints object (ptr)
:r1 = element (long)
:r2 = 0, else index (int)
trashes
:r2-:r4
info
the rfind of :array at 4 bytes
```

### :set_cap -> class/ints/set_cap

```code
inputs
:r0 = ints object (ptr)
:r1 = capacity (uint)
outputs
:r0 = ints object (ptr)
trashes
:r1-:r2, :d0-:d15
info
the set_cap of :array at 4 bytes
```

### :set_elem -> class/ints/set_elem

```code
inputs
:r0 = ints object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = ints object (ptr)
trashes
:r2-:r3
info
the set_elem of :array at 4 bytes
```

### :slice -> class/ints/slice

```code
inputs
:r0 = ints object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice ints object (ptr)
trashes
:r0-:r3, :r6-:r8, :d0-:d15
info
the slice of :array at 4 bytes
```

### :splice -> class/ints/splice

```code
inputs
:r0 = src1 ints object (ptr)
:r1 = src2 array object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = new ints object (ptr)
trashes
:r0-:r11, :d0-:d15
info
the splice of :array at 4 bytes
```

### :type -> class/ints/type

```code
inputs
:r0 = ints object (ptr)
outputs
:r0 = ints object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/ints/create

```code
outputs
:r0 = 0 if error, else ints object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :vtable -> class/ints/vtable

