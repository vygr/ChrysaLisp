# :sys_kernel

```image
docs/reference/vp_classes/sys_kernel.cwb
```

## Lisp Bindings

### (kernel-stats [flush]) -> (task_count mem_total mem_avail max_stack idle_time mem_inuse)

### (system-id) -> nodeid

## VP methods

### :id -> sys/kernel/id

```code
outputs
:r0-:r1 = node id (node_id)
trashes
:r0-:r1
```

### :kernel -> sys/kernel/kernel

```code
inputs
:r0 = argv pointer (pptr)
info
loader is already initialized when we get here !
trashes
:r0-:r14, :f0-:f15
```

### :ping -> sys/kernel/ping

```code
started by kernel at boot
trashes
none
```

### :system_id -> sys/kernel/system_id

```code
outputs
:r0-:r1 = system id (node_id)
trashes
:r0-:r1
```

