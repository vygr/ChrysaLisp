# mstream

## stream

## VP methods

### :create -> class/mstream/create

### :deinit -> class/mstream/deinit

```code
inputs
:r0 = mstream object (ptr)
outputs
:r0 = mstream object (ptr)
trashes
:r1-:r14
```

### :flush -> class/mstream/flush

```code
inputs
:r0 = mstream object (ptr)
outputs
:r0 = mstream object (ptr)
trashes
:r1-:r14
```

### :init -> class/mstream/init

```code
inputs
:r0 = mstream object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = mstream object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :read_next -> class/mstream/read_next

```code
inputs
:r0 = mstream object (ptr)
outputs
:r0 = mstream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r14
```

### :seek -> class/mstream/seek

```code
inputs
:r0 = mstream object (ptr)
:r1 = offset (long)
:r2 = pos (uint)
outputs
:r0 = mstream object (ptr)
:r1 = -1 for error, else file position
trashes
:r1-:r14
```

### :vtable -> class/mstream/vtable

### :write_next -> class/mstream/write_next

```code
inputs
:r0 = sstream object (ptr)
outputs
:r0 = sstream object (ptr)
trashes
:r1-:r14
```

