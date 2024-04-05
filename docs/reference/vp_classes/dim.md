# dim

## obj

## Lisp Bindings

### (dim nums array)

### (dim-get dim nums)

### (dim-set dim nums form)

## VP methods

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

### :vtable -> class/dim/vtable

