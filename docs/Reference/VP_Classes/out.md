# out

## stream

## Lisp Bindings

### (out-stream mbox)

## VP methods

### :create -> class/out/create

### :deinit -> class/out/deinit

```code
inputs
:r0 = out object (ptr)
outputs
:r0 = out object (ptr)
trashes
:r1-:r14
```

### :flush -> class/out/flush

```code
inputs
:r0 = out object (ptr)
outputs
:r0 = out object (ptr)
trashes
:r1-:r14
```

### :init -> class/out/init

```code
inputs
:r0 = out object (ptr)
:r1 = vtable (pptr)
:r2-:r4 = target ID (net_id)
outputs
:r0 = out object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :set_state -> class/out/set_state

```code
inputs
:r0 = out object (ptr)
:r1 = state (uint)
outputs
:r0 = out object (ptr)
trashes
:r1-:r14
```

### :vtable -> class/out/vtable

### :wait_acks -> class/out/wait_acks

```code
inputs
:r0 = out object (ptr)
:r1 = msg ack num (uint)
outputs
:r0 = out object (ptr)
trashes
:r1-:r14
```

### :write_next -> class/out/write_next

```code
inputs
:r0 = out object (ptr)
outputs
:r0 = out object (ptr)
trashes
:r1-:r14
```

