# seq

## obj

### :cat -> class/obj/null

### :find -> class/obj/null

### :get_length -> class/obj/null

### :lisp_cat -> class/seq/lisp_cat

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(cat seq ...)
```

### :lisp_each -> class/seq/lisp_each

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(each! start end lambda (seq ...))
```

### :lisp_elem -> class/seq/lisp_elem

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(elem-get index seq)
```

### :lisp_find -> class/seq/lisp_find

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(find elem seq)
```

### :lisp_length -> class/seq/lisp_length

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(length seq)
```

### :lisp_rfind -> class/seq/lisp_rfind

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(find-rev elem seq)
```

### :lisp_slice -> class/seq/lisp_slice

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(slice start end seq)
```

### :lisp_some -> class/seq/lisp_some

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(some! start end mode lambda (seq ...))
```

### :ref_element -> class/obj/null

### :rfind -> class/obj/null

### :slice -> class/obj/null

### :type -> class/seq/type

### :vtable -> class/seq/vtable

