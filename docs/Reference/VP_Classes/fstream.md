# fstream

## stream

### :create -> class/fstream/create

### :deinit -> class/fstream/deinit

```code
inputs
:r0 = fstream object (ptr)
outputs
:r0 = fstream object (ptr)
trashes
:r1-:r14
```

### :flush -> class/fstream/flush

```code
inputs
:r0 = fstream object (ptr)
outputs
:r0 = fstream object (ptr)
trashes
:r1-:r14
```

### :init -> class/fstream/init

```code
inputs
:r0 = fstream object (ptr)
:r1 = vtable (pptr)
:r2 = c string filename (pubyte)
:r3 = open mode (uint)
outputs
:r0 = fstream object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :read_next -> class/fstream/read_next

```code
inputs
:r0 = fstream object (ptr)
outputs
:r0 = fstream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r14
```

### :seek -> class/fstream/seek

```code
inputs
:r0 = fstream object (ptr)
:r1 = offset (long)
:r2 = pos (uint)
outputs
:r0 = fstream object (ptr)
:r1 = -1 for error, else file position
trashes
:r1-:r14
```

### :vtable -> class/fstream/vtable

### :write_next -> class/fstream/write_next

```code
inputs
:r0 = fstream object (ptr)
outputs
:r0 = fstream object (ptr)
trashes
:r1-:r14
```

