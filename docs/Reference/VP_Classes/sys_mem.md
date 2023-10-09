# sys_mem

## VP methods

### :alloc -> sys/mem/alloc

```code
inputs
:r0 = minimum amount (bytes)
outputs
:r0 = 0 if failed, else address (ptr)
:r1 = 0 if failed, else size given (bytes)
trashes
:r0-:r2
```

### :avail -> sys/mem/avail

```code
outputs
:r0 = available on free lists (uint)
trashes
:r0-:r3
```

### :calloc -> sys/mem/calloc

```code
inputs
:r0 = minimum amount (bytes)
outputs
:r0 = 0 if failed, else address (ptr)
:r1 = 0 if failed, else size given (bytes)
trashes
:r0-:r2
```

### :collect -> sys/mem/collect

```code
trashes
:r0-:r14
info
free all unused blocks
```

### :copy -> sys/mem/copy

```code
inputs
:r0 = source address (ptr)
:r1 = destination address (ptr)
:r2 = length (bytes)
outputs
:r0 = source address end (ptr)
:r1 = destination address end (ptr)
trashes
:r0-:r3
```

### :fill -> sys/mem/fill

```code
inputs
:r0 = address (ptr)
:r1 = length (bytes)
:r2 = fill pattern (ulong)
outputs
:r0 = address end (ptr)
trashes
:r0-:r3
```

### :free -> sys/mem/free

```code
inputs
:r0 = address (ptr)
trashes
:r0-:r2
```

### :realloc -> sys/mem/realloc

```code
inputs
:r0 = block address (ptr)
:r1 = block size (bytes)
:r2 = new block min size (bytes)
outputs
:r0 = new block address (ptr)
:r1 = new block size (bytes)
trashes
:r0-:r5
```

### :recalloc -> sys/mem/recalloc

```code
inputs
:r0 = block address (ptr)
:r1 = block size (bytes)
:r2 = new block min size (bytes)
outputs
:r0 = new block address (ptr)
:r1 = new block size (bytes)
trashes
:r0-:r7
```

### :statics_deinit -> sys/mem/statics_deinit

```code
info
deinit mem statics
```

### :statics_init -> sys/mem/statics_init

```code
info
init mem statics
```

