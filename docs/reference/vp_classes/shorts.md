# :shorts

```image
docs/reference/vp_classes/shorts.cwb
```

## :array

## Lisp Bindings

### (shorts [num ...]) -> shorts

## VP methods

### :append -> class/shorts/append

```code
inputs
:r0 = shorts object (ptr)
:r1 = source array object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = shorts object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the append of :array at 2 bytes
```

### :cat -> class/shorts/cat

```code
inputs
:r0 = shorts object (ptr)
:r1 = list of array objects (ptr)
outputs
:r0 = new shorts object (ptr)
trashes
:r0-:r9, :d0-:d15
info
the cat of :array at 2 bytes
```

### :create -> class/shorts/create

```code
outputs
:r0 = 0 if error, else shorts object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :eql -> class/shorts/eql

```code
inputs
:r0 = shorts object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = shorts object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r5
info
the eql of :array at 2 bytes
```

### :erase -> class/shorts/erase

```code
inputs
:r0 = shorts object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = shorts object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase of :array at 2 bytes
```

### :erase2 -> class/shorts/erase2

```code
inputs
:r0 = shorts object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = shorts object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase2 of :array at 2 bytes
```

### :fill -> class/shorts/fill

```code
inputs
:r0 = shorts object (ptr)
:r1 = length (uint)
:r2 = element (long)
outputs
:r0 = shorts object (ptr)
trashes
:r1-:r4, :d0-:d15
info
the fill of :array at 2 bytes
```

### :find -> class/shorts/find

```code
inputs
:r0 = shorts object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = shorts object (ptr)
:r1 = element (long)
:r2 = -1, else index (int)
trashes
:r2-:r4
info
the find of :array at 2 bytes
```

### :get_raw -> class/shorts/get_raw

```code
inputs
:r0 = shorts object (ptr)
outputs
:r0 = shorts object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
trashes
:r1-:r3
info
the get_raw of :array at 2 bytes
```

### :hash -> class/shorts/hash

```code
inputs
:r0 = shorts object (ptr)
outputs
:r0 = shorts object (ptr)
:r1 = hash code (long)
trashes
:r1-:r4
info
the hash of :array at 2 bytes
```

### :print -> class/shorts/print

```code
inputs
:r0 = shorts object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = shorts object (ptr)
trashes
:r1-:r5, :r14, :d0-:d15
info
the print of :array at 2 bytes
```

### :push_raw -> class/shorts/push_raw

```code
inputs
:r0 = shorts object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
outputs
:r0 = shorts object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the push_raw of :array at 2 bytes
```

### :ref_back -> class/shorts/ref_back

```code
inputs
:r0 = shorts object (ptr)
outputs
:r0 = shorts object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_back of :array at 2 bytes
```

### :ref_elem -> class/shorts/ref_elem

```code
inputs
:r0 = shorts object (ptr)
:r1 = element index (uint)
outputs
:r0 = shorts object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_elem of :array at 2 bytes
```

### :rfind -> class/shorts/rfind

```code
inputs
:r0 = shorts object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = shorts object (ptr)
:r1 = element (long)
:r2 = 0, else index (int)
trashes
:r2-:r4
info
the rfind of :array at 2 bytes
```

### :set_cap -> class/shorts/set_cap

```code
inputs
:r0 = shorts object (ptr)
:r1 = capacity (uint)
outputs
:r0 = shorts object (ptr)
trashes
:r1-:r2, :d0-:d15
info
the set_cap of :array at 2 bytes
```

### :set_elem -> class/shorts/set_elem

```code
inputs
:r0 = shorts object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = shorts object (ptr)
trashes
:r2-:r3
info
the set_elem of :array at 2 bytes
```

### :slice -> class/shorts/slice

```code
inputs
:r0 = shorts object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice shorts object (ptr)
trashes
:r0-:r3, :r6-:r8, :d0-:d15
info
the slice of :array at 2 bytes
```

### :splice -> class/shorts/splice

```code
inputs
:r0 = src1 shorts object (ptr)
:r1 = src2 array object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = new shorts object (ptr)
trashes
:r0-:r11, :d0-:d15
info
the splice of :array at 2 bytes
```

### :type -> class/shorts/type

```code
inputs
:r0 = shorts object (ptr)
outputs
:r0 = shorts object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/shorts/create

```code
outputs
:r0 = 0 if error, else shorts object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :vtable -> class/shorts/vtable

