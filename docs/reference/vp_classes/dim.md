# :dim

## :obj

## Lisp Bindings

### (dim nums array) -> dim

### (dim-get dim nums) -> elem

### (dim-set dim nums elem) -> array

## VP methods

### :create -> class/dim/create

### :deinit -> class/dim/deinit

```code
inputs
:r0 = dim object (ptr)
outputs
:r0 = dim object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :init -> class/dim/init

```code
inputs
:r0 = dim object (ptr)
:r1 = vtable (pptr)
:r2 = dims nums object (ptr)
:r3 = elements array object (ptr)
outputs
:r0 = dim object (ptr)
:r1 = 0 if error, else ok
trashes
:r1
```

### :type -> class/dim/type

### :vtable -> class/dim/vtable

