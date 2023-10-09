# sys_pii

## Lisp Bindings

### (pii-dirlist path)

### (pii-fstat path)

### (pii-read-char fd)

### (pii-remove path)

### (pii-time)

### (pii-write-char fd char)

## VP methods

### :clear_icache -> sys/pii/clear_icache

```code
inputs
:r0 = address (pubyte)
:r1 = length (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :close -> sys/pii/close

```code
inputs
:r0 = fd (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :close_shared -> sys/pii/close_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = handle (long)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :dirlist -> sys/pii/dirlist

```code
inputs
:r0 = c string pathname (pubyte)
:r1 = buffer pointer (ptr)
:r2 = buffer length (ulong)
outputs
:r0 = buffer length (ulong)
trashes
:r0
```

### :exit -> sys/pii/exit

```code
inputs
:r0 = code (long)
```

### :mmap -> sys/pii/mmap

```code
inputs
:r0 = len (ulong)
:r1 = fd (ulong)
:r2 = mode (ulong)
outputs
:r0 = buffer (ptr)
trashes
:r0
```

### :mprotect -> sys/pii/mprotect

```code
inputs
:r0 = buffer (ptr)
:r1 = len (ulong)
:r2 = prot (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :munmap -> sys/pii/munmap

```code
inputs
:r0 = buffer (ptr)
:r1 = len (ulong)
:r2 = mode (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :open -> sys/pii/open

```code
inputs
:r0 = c string filename (pubyte)
:r1 = mode (ulong)
outputs
:r0 = fd (ulong)
trashes
:r0
```

### :open_shared -> sys/pii/open_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = length (ulong)
outputs
:r0 = handle (long)
trashes
:r0
```

### :rand -> sys/pii/rand

```code
inputs
:r0 = data buffer pointer (pubyte)
:r1 = length (uint)
trashes
:r0
```

### :read -> sys/pii/read

```code
inputs
:r0 = fd (ulong)
:r1 = buffer (ptr)
:r2 = len (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :read_char -> sys/pii/read_char

```code
inputs
:r0 = fd (ulong)
outputs
:r0 = char (ulong)
trashes
:r0
```

### :remove -> sys/pii/remove

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :seek -> sys/pii/seek

```code
inputs
:r0 = fd (ulong)
:r1 = offset (long)
:r2 = pos (ulong)
outputs
:r0 = -1 if error, else file position (ulong)
trashes
:r0
```

### :stat -> sys/pii/stat

```code
inputs
:r0 = c string filename (pubyte)
:r1 = stat buf (ptr)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :time -> sys/pii/time

```code
outputs
:r0 = time in usec (ulong)
trashes
:r0
```

### :unlink -> sys/pii/unlink

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :usb_running -> sys/pii/usb_running

```code
inputs
:r0 = link buffer (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :usb_start -> sys/pii/usb_start

```code
inputs
:r0 = link buffer (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :usb_stop -> sys/pii/usb_stop

```code
inputs
:r0 = link buffer (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :write -> sys/pii/write

```code
inputs
:r0 = fd (ulong)
:r1 = buffer (pubyte)
:r2 = len (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :write_char -> sys/pii/write_char

```code
inputs
:r0 = fd (ulong)
:r1 = char (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :write_num -> sys/pii/write_num

```code
inputs
:r0 = fd (ulong)
:r1 = number (ulong)
:r2 = base (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :write_str -> sys/pii/write_str

```code
inputs
:r0 = fd (ulong)
:r1 = c string (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

