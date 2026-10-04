# :host_os

## Lisp Bindings

### (pii-alive pid) -> :nil | :t

### (pii-cpus) -> num

### (pii-dirlist path) -> info

### (pii-exit)

### (pii-fstat path) -> info

### (pii-memory) -> bytes

### (pii-pid) -> pid

### (pii-read-char fd) -> char

### (pii-remove path)

### (pii-spawn args) -> pid | -1

### (pii-time) -> ns

### (pii-write-char fd char) -> char

## VP methods

### :alive -> :nil

### :clear_icache -> :nil

### :close -> :nil

### :close_shared -> :nil

### :cpus -> :nil

### :dirlist -> :nil

### :exit -> :nil

### :gettime -> :nil

### :memory -> :nil

### :mmap -> :nil

### :mprotect -> :nil

### :munmap -> :nil

### :open -> :nil

### :open_shared -> :nil

### :pid -> :nil

### :pii_alive -> sys/pii/alive

```code
inputs
:r0 = process id (long)
outputs
:r0 = 1 if running, else 0 (long)
trashes
:r0, :f0-:f15
info
is this process running ?
```

### :pii_clear_icache -> sys/pii/clear_icache

```code
inputs
:r0 = address (pubyte)
:r1 = length (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_close -> sys/pii/close

```code
inputs
:r0 = fd (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_close_shared -> sys/pii/close_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = handle (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_cpus -> sys/pii/cpus

```code
outputs
:r0 = number of processors (long)
trashes
:r0, :f0-:f15
info
the number of processors this machine has online
```

### :pii_dirlist -> sys/pii/dirlist

```code
inputs
:r0 = c string pathname (pubyte)
:r1 = buffer pointer (ptr)
:r2 = buffer length (long)
outputs
:r0 = buffer length (long)
trashes
:r0, :f0-:f15
```

### :pii_exit -> sys/pii/exit

```code
inputs
:r0 = code (long)
trashes
:r0, :f0-:f15
```

### :pii_flush -> sys/pii/flush

### :pii_memory -> sys/pii/memory

```code
outputs
:r0 = physical memory in bytes (long)
trashes
:r0, :f0-:f15
info
the physical memory this machine has
```

### :pii_mmap -> sys/pii/mmap

```code
inputs
:r0 = len (long)
:r1 = fd (long)
:r2 = mode (long)
outputs
:r0 = buffer (ptr)
trashes
:r0, :f0-:f15
```

### :pii_mprotect -> sys/pii/mprotect

```code
inputs
:r0 = buffer (ptr)
:r1 = len (long)
:r2 = prot (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_munmap -> sys/pii/munmap

```code
inputs
:r0 = buffer (ptr)
:r1 = len (long)
:r2 = mode (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_open -> sys/pii/open

```code
inputs
:r0 = c string filename (pubyte)
:r1 = mode (long)
outputs
:r0 = fd (long)
trashes
:r0, :f0-:f15
```

### :pii_open_shared -> sys/pii/open_shared

```code
inputs
:r0 = c string filename (pubyte)
:r1 = length (long)
outputs
:r0 = handle (long)
trashes
:r0, :f0-:f15
```

### :pii_pid -> sys/pii/pid

```code
outputs
:r0 = process id (long)
trashes
:r0, :f0-:f15
info
the process id of this node
```

### :pii_rand -> sys/pii/rand

```code
inputs
:r0 = data buffer pointer (pubyte)
:r1 = length (uint)
outputs
:r0 = data buffer pointer (pubyte)
trashes
:r0, :f0-:f15
```

### :pii_read -> sys/pii/read

```code
inputs
:r0 = fd (long)
:r1 = buffer (ptr)
:r2 = len (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_read_char -> sys/pii/read_char

```code
inputs
:r0 = fd (long)
outputs
:r0 = char (long), 0 if no char, -1 if EOF
trashes
:r0, :f0-:f15
```

### :pii_remove -> sys/pii/remove

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_seek -> sys/pii/seek

```code
inputs
:r0 = fd (long)
:r1 = offset (long)
:r2 = pos (long)
outputs
:r0 = -1 if error, else file position (long)
trashes
:r0, :f0-:f15
```

### :pii_spawn -> sys/pii/spawn

```code
inputs
:r0 = c string args (pubyte)
outputs
:r0 = process id, -1 if error (long)
trashes
:r0, :f0-:f15
info
start another node, this host and this boot image, with these args
```

### :pii_stat -> sys/pii/stat

```code
inputs
:r0 = c string filename (pubyte)
:r1 = stat buf (ptr)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_sysid -> sys/pii/sysid

```code
inputs
:r0 = data buffer pointer (pubyte)
:r1 = length (uint)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_time -> sys/pii/time

```code
outputs
:r0 = time in usec (long)
trashes
:r0, :f0-:f15
```

### :pii_unlink -> sys/pii/unlink

```code
inputs
:r0 = c string filename (pubyte)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_write -> sys/pii/write

```code
inputs
:r0 = fd (long)
:r1 = buffer (pubyte)
:r2 = len (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_write_char -> sys/pii/write_char

```code
inputs
:r0 = fd (long)
:r1 = char (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_write_num -> sys/pii/write_num

```code
inputs
:r0 = fd (long)
:r1 = number (long)
:r2 = base (long)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :pii_write_str -> sys/pii/write_str

```code
inputs
:r0 = fd (long)
:r1 = c string (pubyte)
outputs
:r0 = error code (long)
trashes
:r0, :f0-:f15
```

### :rand -> :nil

### :read -> :nil

### :remove -> :nil

### :seek -> :nil

### :sleep -> :nil

### :spawn -> :nil

### :stat -> :nil

### :sysid -> :nil

### :unlink -> :nil

### :write -> :nil

