# seq

## obj

### :cat -> class/obj/null

### :find -> class/obj/null

### :get_length -> class/obj/null

### :lisp_cat -> class/seq/lisp_cat

### (cat seq ...)

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

### :lisp_each -> class/seq/lisp_each

### (each! start end lambda (seq ...))

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

### :lisp_elem -> class/seq/lisp_elem

### (elem-get index seq)

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

### :lisp_find -> class/seq/lisp_find

### (find elem seq)

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

### :lisp_length -> class/seq/lisp_length

### (length seq)

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

### :lisp_rfind -> class/seq/lisp_rfind

### (find-rev elem seq)

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

### :lisp_slice -> class/seq/lisp_slice

### (slice start end seq)

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

### :lisp_some -> class/seq/lisp_some

### (some! start end mode lambda (seq ...))

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

### :ref_element -> class/obj/null

### :rfind -> class/obj/null

### :slice -> class/obj/null

### :type -> class/seq/type

### :vtable -> class/seq/vtable

