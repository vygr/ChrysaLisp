# sstream

## stream

### :claim_string -> class/sstream/claim_string

```code
inputs
:r0 = sstream object (ptr)
outputs
:r0 = sstream object (ptr)
:r1 = str object (ptr)
trashes
:r1-:r2
```

### :create -> class/sstream/create

### :flush -> class/sstream/flush

```code
inputs
:r0 = sstream object (ptr)
outputs
:r0 = sstream object (ptr)
trashes
:r1-:r14
```

### :init -> class/sstream/init

```code
inputs
:r0 = sstream object (ptr)
:r1 = vtable (pptr)
:r2 = str object (ptr)
outputs
:r0 = sstream object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5
```

### :ref_string -> class/sstream/ref_string

```code
inputs
:r0 = sstream object (ptr)
outputs
:r0 = sstream object (ptr)
:r1 = str object (ptr)
trashes
:r1-:r2
```

### :vtable -> class/sstream/vtable

### :write_next -> class/sstream/write_next

```code
inputs
:r0 = sstream object (ptr)
outputs
:r0 = sstream object (ptr)
trashes
:r1-:r14
```

