# stdio

## obj

## Lisp Bindings

### (create-stdio) -> stdio

## VP methods

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

### :vtable -> class/stdio/vtable

