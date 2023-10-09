# stream

## obj

## Lisp Bindings

### (stream-avail stream)

### (file-stream path [mode])

### (io-stream io)

### (read-avail stream)

### (read-char stream [width])

### (read-line stream)

### (stream-seek stream offset pos)

### (string-stream str)

### (write stream str)

### (stream-flush stream)

### (write-char stream list|num [width])

## VP methods

### :available -> class/stream/available

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
:r1 = available space (bytes)
trashes
:r1-:r2
```

### :create -> class/stream/create

### :deinit -> class/stream/deinit

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :flush -> class/stream/flush

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :init -> class/stream/init

```code
inputs
:r0 = stream object (ptr)
:r1 = vtable (pptr)
:r2 = buffer object, 0 if none (ptr)
:r3 = buffer data, 0 if none (ptr)
:r4 = buffer start (pubyte)
:r5 = buffer length (uint)
outputs
:r0 = stream object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5
```

### :read -> class/stream/read

```code
inputs
:r0 = stream object (ptr)
:r1 = buffer (pubyte)
:r2 = buffer length (uint)
outputs
:r0 = stream object (ptr)
:r1 = -1 for EOF, else bytes read (int)
trashes
:r1-:r14
```

### :read_bits -> class/stream/read_bits

```code
inputs
:r0 = stream object (ptr)
:r1 = num bits (uint)
:r2 = bit pool (ulong)
:r3 = bit pool size (uint)
outputs
:r0 = stream object (ptr)
:r1 = -1 if eof, else data (long)
:r2 = bit pool (ulong)
:r3 = bit pool size (uint)
trashes
:r1-:r14
```

### :read_char -> class/stream/read_char

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
:r1 = -1 for EOF, else char read (int)
trashes
:r1-:r14
```

### :read_line -> class/stream/read_line

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
:r1 = 0 for EOF, else str object (ptr)
trashes
:r1-:r14
```

### :read_next -> class/stream/read_next

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r14
```

### :seek -> class/stream/seek

```code
inputs
:r0 = stream object (ptr)
:r1 = offset (long)
:r2 = pos (uint)
outputs
:r0 = stream object (ptr)
:r1 = -1 for error, else file position
trashes
:r1-:r14
```

### :skip -> class/stream/skip

```code
inputs
:r0 = stream object (ptr)
:r1 = char to skip (uint)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :skip_not -> class/stream/skip_not

```code
inputs
:r0 = stream object (ptr)
:r1 = char to not skip (uint)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :vtable -> class/stream/vtable

### :write -> class/stream/write

```code
inputs
:r0 = stream object (ptr)
:r1 = buffer (pubyte)
:r2 = buffer length (uint)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :write_bits -> class/stream/write_bits

```code
inputs
:r0 = stream object (ptr)
:r1 = data (uint)
:r2 = num bits (uint)
:r3 = bit pool (ulong)
:r4 = bit pool size (uint)
outputs
:r0 = stream object (ptr)
:r1 = bit pool (ulong)
:r2 = bit pool size (uint)
trashes
:r1-:r14
```

### :write_char -> class/stream/write_char

```code
inputs
:r0 = stream object (ptr)
:r1 = char (uint)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :write_cstr -> class/stream/write_cstr

```code
inputs
:r0 = stream object (ptr)
:r1 = buffer (pubyte)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

### :write_next -> class/stream/flush

```code
inputs
:r0 = stream object (ptr)
outputs
:r0 = stream object (ptr)
trashes
:r1-:r14
```

