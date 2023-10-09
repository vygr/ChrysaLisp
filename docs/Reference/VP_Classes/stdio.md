# stdio

## obj

### :create -> class/stdio/create

### :deinit -> class/stdio/deinit

```code
inputs
:r0 = stdio object (ptr)
outputs
:r0 = stdio object (ptr)
trashes
:r1-:r14
```

### :init -> class/stdio/init

```code
inputs
:r0 = stdio object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = stdio object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :lisp_create -> class/stdio/lisp_create

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
;(create-stdio)
```

### :vtable -> class/stdio/vtable

