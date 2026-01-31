# :str

## :seq

## Lisp Bindings

### (bfind char cls) -> :nil | idx

### (bskip cls str idx) -> idx

### (bskipn cls str idx) -> idx

### (char num [width]) -> str

### (cmp str str) -> + | 0 | -

### (code str [width idx]) -> num

### (compress str tab_width) -> str

### (str-alloc size) -> str

### (hex-decode str) -> str

### (hex-encode str) -> str

### (ends-with str str) -> :nil | :t

### (expand str tab_width) -> str

### (load path) -> str

### (rbskip cls str idx) -> idx

### (rbskipn cls str idx) -> idx

### (save str path) -> str

### (split str [cls]) -> strs

### (starts-with str str) -> :nil | :t

### (str form) -> str

### (str-to-num str) -> num

### (unescape str) -> str

## VP methods

### :bfind -> class/str/bfind

```code
inputs
:r0 = str cls object (ptr)
:r1 = search char (uint)
outputs
:r0 = str cls object (ptr)
:r1 = search char (uint)
:r2 = -1, else position (int)
trashes
:r2-:r6
```

### :bskip -> class/str/bskip

```code
inputs
:r0 = str object (ptr)
:r1 = str cls object (ptr)
:r2 = start index (uint)
outputs
:r0 = str object (ptr)
:r1 = str cls object (ptr)
:r2 = end index (uint)
trashes
:r2-:r10
```

### :bskipn -> class/str/bskipn

```code
inputs
:r0 = str object (ptr)
:r1 = str cls object (ptr)
:r2 = start index (uint)
outputs
:r0 = str object (ptr)
:r1 = str cls object (ptr)
:r2 = end index (uint)
trashes
:r2-:r10
```

### :cat -> class/str/cat

```code
inputs
:r0 = str object (ptr)
:r1 = list of str objects (ptr)
outputs
:r0 = new str object (ptr)
trashes
:r0-:r8
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

### :decode -> class/str/decode

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = decoded str object (ptr)
trashes
:r1-:r8
```

### :encode -> class/str/encode

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = encoded str object (ptr)
trashes
:r1-:r8
```

### :ends_with -> class/str/ends_with

```code
inputs
:r0 = str postfix object (ptr)
:r1 = str object (ptr)
outputs
:r0 = str prefix object (ptr)
:r1 = 0 if match, else not
trashes
:r1-:r5
```

### :eql -> class/str/eql

```code
inputs
:r0 = str object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = str object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r6
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
:r1 = hash code (long)
trashes
:r1-:r4
```

### :hash_pstr -> class/str/hash_pstr

```code
inputs
:r3 = start (pubyte)
:r4 = end (pubyte)
outputs
:r1 = hash code (long)
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
:r0 = array object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = slice array object (ptr)
trashes
:r0-:r6
```

### :splice -> class/str/splice

```code
inputs
:r0 = src1 str object (ptr)
:r1 = src2 str object (ptr)
:r2 = nums object (ptr)
outputs
:r0 = new str object (ptr)
trashes
:r0-:r11
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

### :unescape -> class/str/unescape

```code
inputs
:r0 = str object (ptr)
outputs
:r0 = str object (ptr)
trashes
:r0-:r8
```

### :vtable -> class/str/vtable

