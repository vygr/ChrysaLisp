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
info
a cell back on the free list, for a function that does not free in
line, see (hp-freecell): those of a validate build, here are its
checks. A normal build's is the free alone, none of its own functions
call it, and its boot image holds it for a validate function to be
bound to: a normal system's cells have no guard word, and are not
filled, so its free must not look for either.
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

