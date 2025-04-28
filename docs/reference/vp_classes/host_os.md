# host_os

## Lisp Bindings

### (pii-dirlist path)

### (pii-fstat path)

### (pii-read-char fd)

### (pii-remove path)

### (pii-time)

### (pii-write-char fd char)

## VP methods

### :clear_icache -> :nil

### :close -> :nil

### :close_shared -> :nil

### :dirlist -> :nil

### :exit -> :nil

### :gettime -> :nil

### :mmap -> :nil

### :mprotect -> :nil

### :munmap -> :nil

### :open -> :nil

### :open_shared -> :nil

### :pii_clear_icache -> sys/pii/clear_icache

```code
inputs
:r0 = address (pubyte)
:r1 = length (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_close -> sys/pii/close

```code
inputs
:r0 = fd (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_close_shared -> sys/pii/close_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = handle (long)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_dirlist -> sys/pii/dirlist

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

### :pii_exit -> sys/pii/exit

```code
inputs
:r0 = code (long)
```

### :pii_flush -> sys/pii/flush

### :pii_mmap -> sys/pii/mmap

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

### :pii_mprotect -> sys/pii/mprotect

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

### :pii_munmap -> sys/pii/munmap

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

### :pii_open -> sys/pii/open

```code
inputs
:r0 = c string filename (pubyte)
:r1 = mode (ulong)
outputs
:r0 = fd (ulong)
trashes
:r0
```

### :pii_open_shared -> sys/pii/open_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = length (ulong)
outputs
:r0 = handle (long)
trashes
:r0
```

### :pii_rand -> sys/pii/rand

```code
inputs
:r0 = data buffer pointer (pubyte)
:r1 = length (uint)
trashes
:r0
```

### :pii_read -> sys/pii/read

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

### :pii_read_char -> sys/pii/read_char

```code
inputs
:r0 = fd (ulong)
outputs
:r0 = char (ulong)
trashes
:r0
```

### :pii_remove -> sys/pii/remove

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_seek -> sys/pii/seek

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

### :pii_stat -> sys/pii/stat

```code
inputs
:r0 = c string filename (pubyte)
:r1 = stat buf (ptr)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_time -> sys/pii/time

```code
outputs
:r0 = time in usec (ulong)
trashes
:r0
```

### :pii_unlink -> sys/pii/unlink

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_write -> sys/pii/write

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

### :pii_write_char -> sys/pii/write_char

```code
inputs
:r0 = fd (ulong)
:r1 = char (ulong)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :pii_write_num -> sys/pii/write_num

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

### :pii_write_str -> sys/pii/write_str

```code
inputs
:r0 = fd (ulong)
:r1 = c string (pubyte)
outputs
:r0 = error code (ulong)
trashes
:r0
```

### :rand -> :nil

### :read -> :nil

### :remove -> :nil

### :seek -> :nil

### :sleep -> :nil

### :stat -> :nil

### :unlink -> :nil

### :write -> :nil

