# sys_kernel

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

### :lisp_stats -> sys/kernel/lisp_stats

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(kernel-stats)
```

### :ping -> sys/kernel/ping

```code
started by kernel at boot
```

