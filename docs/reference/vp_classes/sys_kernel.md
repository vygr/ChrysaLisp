# sys_kernel

## Lisp Bindings

### (kernel-stats) -> (task_count mem_used mem_avail max_stack)

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
```

### :ping -> sys/kernel/ping

```code
started by kernel at boot
```

