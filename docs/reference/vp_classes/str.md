# str

## seq

## Lisp Bindings

### (bfind char cls) -> :nil | idx

### (bskip cls str idx) -> idx

### (bskipn cls str idx) -> idx

### (char num [width]) -> str

### (cmp str str) -> + | 0 | -

### (code str [width idx]) -> num

### (str-alloc size) -> str

### (id-decode str) -> str

### (id-encode str) -> str

### (expand str tab_width) -> str

### (load path) -> str

### (rbskip cls str idx) -> idx

### (rbskipn cls str idx) -> idx

### (save str path) -> str

### (str form) -> str

### (str-to-num str) -> num

## VP methods

### :append -> class/str/append

```code
inputs
:r0 = str object (ptr)
:r1 = str object (ptr)
outputs
:r0 = 0 if error, else new str object (ptr)
trashes
:r1-:r6
```

### :bfind -> class/str/bfind

```code
inputs
:r0 = str object (ptr)
:r1 = search char (uint)
outputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = -1, else position (int)
trashes
:r2-:r6
```

### :cat -> class/str/cat

```code
inputs
:r0 = str object (ptr)
:r1 = list of str objects (ptr)
outputs
:r0 = 0 if error, else new str object (ptr)
trashes
:r1-:r6
```

### :compare -> class/str/compare

```code
inputs
:r0 = str object (ptr)
:r1 = str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = 0 if same, else -, +
trashes
:r1-:r7
```

### :create_from_buffer -> class/str/create_from_buffer

```code
inputs
:r0 = buffer (pubyte)
:r1 = buffer length (uint)
outputs
:r0 = 0 if error, else str object (ptr)
trashes
:r0-:r6
```

### :create_from_cstr -> class/str/create_from_cstr

```code
inputs
:r0 = c string (pubyte)
outputs
:r0 = 0 if error, else str object (ptr)
trashes
:r0-:r6
```

### :create_from_file -> class/str/create_from_file

```code
inputs
:r0 = file name c string (pubyte)
outputs
:r0 = 0 if error, else str object (ptr)
trashes
:r0-:r6
```

### :create_from_long -> class/str/create_from_long

```code
inputs
:r0 = number (long)
:r1 = base, - for unsigned, (long)
outputs
:r0 = 0 if error, else str object (ptr)
trashes
:r0-:r6
```

### :find -> class/str/find

```code
inputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = start index (uint)
outputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = -1, else position (int)
trashes
:r2-:r5
```

### :get_length -> class/str/get_length

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = string length (bytes)
trashes
:r1
```

### :hash -> class/str/hash

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = hash code (ulong)
trashes
:r1-:r4
```

### :hash_pstr -> class/str/hash_pstr

```code
inputs
:r3 = start (pubyte)
:r4 = end (pubyte)
outputs
:r1 = hash code (ulong)
trashes
:r1-:r4
```

### :init -> class/str/init

```code
inputs
:r0 = str object (ptr)
:r1 = vtable (pptr)
:r2 = 0 else, buffer (pubyte)
:r3 = buffer length (uint)
outputs
:r0 = str object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :init1 -> class/str/init1

```code
inputs
:r0 = str object (ptr)
:r1 = vtable (pptr)
:r2 = str object (ptr)
:r3 = str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :init2 -> class/str/init2

```code
inputs
:r0 = str object (ptr)
:r1 = vtable (pptr)
:r2 = file name c string (pubyte)
:r3 = file length (uint)
outputs
:r0 = str object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :init3 -> class/str/init3

```code
inputs
:r0 = str object (ptr)
:r1 = vtable (pptr)
:r2 = list of str objects (ptr)
outputs
:r0 = str object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :print -> class/str/print

```code
inputs
:r0 = str object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = str object (ptr)
trashes
:r1-:r14
```

### :ref_elem -> class/str/ref_elem

```code
inputs
:r0 = str object (ptr)
:r1 = char index (uint)
outputs
:r0 = str object (ptr)
:r1 = char str object (ptr)
trashes
:r1-:r7
```

### :rfind -> class/str/rfind

```code
inputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = start index (uint)
outputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = 0, else position (int)
trashes
:r2-:r4
```

### :rslice -> class/str/rslice

```code
inputs
:r0 = str object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = str object (ptr)
:r1 = string slice object (ptr)
trashes
:r1-:r7
```

### :same -> class/str/same

```code
inputs
:r0 = str object (ptr)
:r1 = str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = 0 if same
trashes
:r1-:r6
```

### :same_pstr -> class/str/same_pstr

```code
inputs
:r1 = start1 (pubyte)
:r2 = end1 (pubyte)
:r3 = start2 (pubyte)
outputs
:r4 = 0 if match, else not
trashes
:r1-:r5
```

### :slice -> class/str/slice

```code
inputs
:r0 = str object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = str object (ptr)
:r1 = string slice object (ptr)
trashes
:r1-:r7
```

### :split -> class/str/split

```code
inputs
:r0 = str object (ptr)
:r1 = split char (uint)
outputs
:r0 = str object (ptr)
:r1 = list of str objects (ptr)
trashes
:r1-:r14
```

### :starts_with -> class/str/starts_with

```code
inputs
:r0 = str prefix object (ptr)
:r1 = str object (ptr)
outputs
:r0 = str prefix object (ptr)
:r1 = 0 if match, else not
trashes
:r1-:r6
```

### :type -> class/str/type

### :vtable -> class/str/vtable

