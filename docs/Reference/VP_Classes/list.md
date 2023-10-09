# list

## array

### :append -> class/list/append

```code
inputs
:r0 = list object (ptr)
:r1 = source list object (ptr)
:r2 = element start index (uint)
:r3 = element end index (uint)
outputs
:r0 = list object (ptr)
trashes
:r1-:r9
```

### :clear -> class/list/clear

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14
```

### :create -> class/list/create

### :deep_copy -> class/list/deep_copy

```code
inputs
:r0 = list object (ptr)
:r1 = stack array object (ptr)
outputs
:r0 = list object (ptr)
:r1 = copy list object (ptr)
trashes
:r1-:r14
```

### :deinit -> class/list/deinit

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14
```

### :erase -> class/list/erase

```code
inputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r14
```

### :erase2 -> class/list/erase2

```code
inputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
outputs
:r0 = list object (ptr)
:r1 = element iterator (pptr)
trashes
:r2-:r14
```

### :find -> class/list/find

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = -1, else index (int)
trashes
:r2-:r14
```

### :lisp_copy -> class/list/lisp_copy

### (copy form)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_list -> class/list/lisp_list

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_match -> class/list/lisp_match

### (lmatch? list list)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_merge -> class/list/lisp_merge

### (merge-obj dlist slist) -> dlist

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_part -> class/list/lisp_part

### (pivot lambda list start end)

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :print -> class/list/print

```code
inputs
:r0 = list object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14
```

### :ref_back -> class/list/ref_back

```code
inputs
:r0 = list object (ptr)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
trashes
:r1-:r2
```

### :ref_element -> class/list/ref_element

```code
inputs
:r0 = list object (ptr)
:r1 = element index (uint)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
trashes
:r1-:r2
```

### :rfind -> class/list/rfind

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
outputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = -1, else index (int)
trashes
:r2-:r14
```

### :set_element -> class/list/set_element

```code
inputs
:r0 = list object (ptr)
:r1 = element object (ptr)
:r2 = element index (uint)
outputs
:r0 = list object (ptr)
trashes
:r1-:r14
```

### :slice -> class/list/slice

```code
inputs
:r0 = list object (ptr)
:r1 = element start index (uint)
:r2 = element end index (uint)
outputs
:r0 = list object (ptr)
:r1 = slice list object (ptr)
trashes
:r1-:r8
```

### :type -> class/list/type

### :vcreate -> class/list/create

### :vtable -> class/list/vtable

