# sys_heap

### :alloc -> sys/heap/alloc

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
:r1 = cell (ptr)
trashes
:r1-:r2
```

### :collect -> sys/heap/collect

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
trashes
:r1-:r8
```

### :deinit -> sys/heap/deinit

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
trashes
:r1-:r6
```

### :free -> sys/heap/free

```code
inputs
:r0 = heap (ptr)
:r1 = cell (ptr)
outputs
:r0 = heap (ptr)
:r1 = cell (ptr)
trashes
:r2
```

### :init -> sys/heap/init

```code
inputs
:r0 = heap (ptr)
:r1 = cell size (bytes)
:r2 = num cells (uint)
outputs
:r0 = heap (ptr)
:r1 = cell size (bytes)
trashes
:r1-:r2
```

