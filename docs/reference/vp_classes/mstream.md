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
:r1-:r9
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

### :itop -> class/mstream/itop

```code
inputs
:r0 = mstream object (ptr)
:r1 = file position (uint)
outputs
:r0 = mstream object (ptr)
:r1 = file position (uint)
:r2 = 0, else bufp (pubyte)
:r3 = 0, else bufe (pubyte)
:r4 = 0, else chunk str object (ptr)
:r5 = 0, else chunk index (uint)
trashes
:r1-:r10
```

### :ptoi -> class/mstream/ptoi

```code
inputs
:r0 = mstream object (ptr)
:r1 = bufp (pubyte)
outputs
:r0 = mstream object (ptr)
:r1 = bufp (pubyte)
:r2 = 0, else chunk str object (ptr)
:r3 = 0, else file position (uint)
:r4 = 0, else file size (uint)
trashes
:r1-:r9
```

### :read_next -> class/mstream/read_next

```code
inputs
:r0 = mstream object (ptr)
outputs
:r0 = mstream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r6
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
:r1-:r11
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

