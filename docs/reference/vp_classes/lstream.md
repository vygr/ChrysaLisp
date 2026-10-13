# :lstream

```image
docs/reference/vp_classes/lstream.cwb
```

## :stream

## Lisp Bindings

### (lisp-stream fill [seek write]) -> stream

## VP methods

### :create -> class/lstream/create

```code
inputs
:r0 = fill function object (ptr)
:r1 = seek function object, 0 if none (ptr)
:r2 = lisp object (ptr)
:r3 = write function object, 0 if none (ptr)
outputs
:r0 = 0 if error, else lstream object (ptr)
trashes
:r0-:r5, :r14, :f0-:f15
```

### :deinit -> class/lstream/deinit

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
trashes
:r1-:r9, :f0-:f15
```

### :fill -> class/lstream/fill

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r14, :f0-:f15
```

### :flush -> class/lstream/flush

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
trashes
:r1-:r9, :f0-:f15
```

### :init -> class/lstream/init

```code
inputs
:r0 = lstream object (ptr)
:r1 = vtable (pptr)
:r2 = fill function object (ptr)
:r3 = seek function object, 0 if none (ptr)
:r4 = lisp object (ptr)
:r5 = write function object, 0 if none (ptr)
outputs
:r0 = lstream object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :f0-:f15
```

### :put -> class/lstream/put

```code
inputs
:r0 = lstream object (ptr)
:r1 = 0 if the buffer is full, else it is a flush (long)
outputs
:r0 = lstream object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :read_next -> class/lstream/read_next

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r8, :r14, :f0-:f15
```

### :seek -> class/lstream/seek

```code
inputs
:r0 = lstream object (ptr)
:r1 = offset (long)
:r2 = whence (uint)
outputs
:r0 = lstream object (ptr)
:r1 = -1 for error, else file position
trashes
:r1-:r11, :r14, :f0-:f15
```

### :seek_to -> class/lstream/seek_to

```code
inputs
:r0 = lstream object (ptr)
:r1 = offset (long)
:r2 = whence (uint)
outputs
:r0 = lstream object (ptr)
:r1 = -1 for error, else file position
trashes
:r1-:r14, :f0-:f15
```

### :type -> class/lstream/type

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :f0-:f15
```

### :vtable -> class/lstream/vtable

### :write_next -> class/lstream/write_next

```code
inputs
:r0 = lstream object (ptr)
outputs
:r0 = lstream object (ptr)
trashes
:r1-:r9, :f0-:f15
```

