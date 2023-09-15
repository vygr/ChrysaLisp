# dim

## obj

### :create -> class/dim/create

### :deinit -> class/dim/deinit

```code
inputs
:r0 = dim object (ptr)
outputs
:r0 = dim object (ptr)
trashes
:r1-:r14
```

### :init -> class/dim/init

```code
inputs
:r0 = dim object (ptr)
:r1 = vtable (pptr)
:r2 = dims nums object (ptr)
:r3 = elements array object (ptr)
outputs
:r0 = error object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :lisp_dim -> class/dim/lisp_dim

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

### :lisp_get -> class/dim/lisp_get

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

### :lisp_set -> class/dim/lisp_set

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

### :vtable -> class/dim/vtable

