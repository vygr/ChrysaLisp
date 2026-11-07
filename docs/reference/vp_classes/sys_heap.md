# :sys_heap

```image
docs/reference/vp_classes/sys_heap.cwb
```

## VP methods

### :alloc -> sys/heap/alloc

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
:r1 = cell (ptr)
trashes
:r1-:r2, :d0-:d15
```

### :collect -> sys/heap/collect

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
trashes
:r1-:r8, :d0-:d15
```

### :deinit -> sys/heap/deinit

```code
inputs
:r0 = heap (ptr)
outputs
:r0 = heap (ptr)
trashes
:r1-:r6, :d0-:d15
```

### :double_free -> sys/heap/double_free

```code
trashes
none
info
a cell being freed is already free, report and dump
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
trashes
:r1-:r2
```

### :overrun -> sys/heap/overrun

```code
trashes
none
info
a cell being freed has had its guard word written on, report and dump
```

