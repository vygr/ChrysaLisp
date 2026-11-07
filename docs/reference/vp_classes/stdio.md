# :stdio

```image
docs/reference/vp_classes/stdio.cwb
```

## :obj

## Lisp Bindings

### (create-stdio) -> stdio

## VP methods

### :create -> class/stdio/create

```code
inputs
none
outputs
:r0 = 0 if error, else stdio object (ptr)
trashes
:r0-:r6, :r14, :d0-:d15
```

### :deinit -> class/stdio/deinit

```code
inputs
:r0 = stdio object (ptr)
outputs
:r0 = stdio object (ptr)
trashes
:r1-:r9, :d0-:d15
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
:r1-:r6, :r14, :d0-:d15
```

### :type -> class/stdio/type

```code
inputs
:r0 = stdio object (ptr)
outputs
:r0 = stdio object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vtable -> class/stdio/vtable

