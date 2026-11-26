# :bytes

```image
docs/reference/vp_classes/bytes.cwb
```

## :array

## Lisp Bindings

### (bytes [num ...]) -> bytes

## VP methods

### :append -> class/bytes/append

```code
inputs
:r0 = bytes object (ptr)
:r1 = source array object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = bytes object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the append of :array at 1 byte
```

### :cat -> class/bytes/cat

```code
inputs
:r0 = bytes object (ptr)
:r1 = list of array objects (ptr)
outputs
:r0 = new bytes object (ptr)
trashes
:r0-:r9, :d0-:d15
info
the cat of :array at 1 byte
```

### :create -> class/bytes/create

```code
outputs
:r0 = 0 if error, else bytes object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :eql -> class/bytes/eql

```code
inputs
:r0 = bytes object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = bytes object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r5
info
the eql of :array at 1 byte
```

### :erase -> class/bytes/erase

```code
inputs
:r0 = bytes object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = bytes object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase of :array at 1 byte
```

### :erase2 -> class/bytes/erase2

```code
inputs
:r0 = bytes object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = bytes object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r3
info
the erase2 of :array at 1 byte
```

### :fill -> class/bytes/fill

```code
inputs
:r0 = bytes object (ptr)
:r1 = length (uint)
:r2 = element (long)
outputs
:r0 = bytes object (ptr)
trashes
:r1-:r4, :d0-:d15
info
the fill of :array at 1 byte
```

### :find -> class/bytes/find

```code
inputs
:r0 = bytes object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = bytes object (ptr)
:r1 = element (long)
:r2 = -1, else index (int)
trashes
:r2-:r4
info
the find of :array at 1 byte
```

### :get_raw -> class/bytes/get_raw

```code
inputs
:r0 = bytes object (ptr)
outputs
:r0 = bytes object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
trashes
:r1-:r3
info
the get_raw of :array at 1 byte
```

### :hash -> class/bytes/hash

```code
inputs
:r0 = bytes object (ptr)
outputs
:r0 = bytes object (ptr)
:r1 = hash code (long)
trashes
:r1-:r4
info
the hash of :array at 1 byte
```

### :print -> class/bytes/print

```code
inputs
:r0 = bytes object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = bytes object (ptr)
trashes
:r1-:r5, :r14, :d0-:d15
info
the print of :array at 1 byte
```

### :push_raw -> class/bytes/push_raw

```code
inputs
:r0 = bytes object (ptr)
:r1 = raw data (pubyte)
:r2 = length (elements)
:r3 = element size (bytes)
outputs
:r0 = bytes object (ptr)
trashes
:r1-:r3, :r6-:r9, :d0-:d15
info
the push_raw of :array at 1 byte
```

### :ref_back -> class/bytes/ref_back

```code
inputs
:r0 = bytes object (ptr)
outputs
:r0 = bytes object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_back of :array at 1 byte
```

### :ref_elem -> class/bytes/ref_elem

```code
inputs
:r0 = bytes object (ptr)
:r1 = element index (uint)
outputs
:r0 = bytes object (ptr)
:r1 = num object (ptr)
trashes
:r1-:r3, :r14, :d0-:d15
info
the ref_elem of :array at 1 byte
```

### :rfind -> class/bytes/rfind

```code
inputs
:r0 = bytes object (ptr)
:r1 = element (long)
:r2 = start index (uint)
outputs
:r0 = bytes object (ptr)
:r1 = element (long)
:r2 = 0, else index (int)
trashes
:r2-:r4
info
the rfind of :array at 1 byte
```

### :set_cap -> class/bytes/set_cap

```code
inputs
:r0 = bytes object (ptr)
:r1 = capacity (uint)
outputs
:r0 = bytes object (ptr)
trashes
:r1-:r2, :d0-:d15
info
the set_cap of :array at 1 byte
```

### :set_elem -> class/bytes/set_elem

```code
inputs
:r0 = bytes object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = bytes object (ptr)
trashes
:r2-:r3
info
the set_elem of :array at 1 byte
```

### :slice -> class/bytes/slice

```code
inputs
:r0 = bytes object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice bytes object (ptr)
trashes
:r0-:r3, :r6-:r8, :d0-:d15
info
the slice of :array at 1 byte
```

### :splice -> class/bytes/splice

```code
inputs
:r0 = src1 bytes object (ptr)
:r1 = src2 array object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = new bytes object (ptr)
trashes
:r0-:r11, :d0-:d15
info
the splice of :array at 1 byte
```

### :type -> class/bytes/type

```code
inputs
:r0 = bytes object (ptr)
outputs
:r0 = bytes object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vcreate -> class/bytes/create

```code
outputs
:r0 = 0 if error, else bytes object (ptr)
trashes
:r0-:r2, :d0-:d15
```

### :vtable -> class/bytes/vtable

