# str

## seq

## Lisp Bindings

### (bfind char str)

### (char num [width])

### (cmp str str)

### (code str [width index])

### (str-alloc size)

### (expand str tab_width)

### (load path)

### (save str path)

### (split str chars)

### (str-to-num str)

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

### :ref_element -> class/str/ref_element

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
outputs
:r0 = str object (ptr)
:r1 = search char (uint)
:r2 = -1, else position (int)
trashes
:r2-:r4
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
:r1 = split chars str object (ptr)
outputs
:r0 = str object (ptr)
:r1 = list of str objects (ptr)
trashes
:r1-:r14
```

### :split_char -> class/str/split_char

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
:r1 = 0 if match
trashes
:r1-:r6
```

### :type -> class/str/type

### :vtable -> class/str/vtable

