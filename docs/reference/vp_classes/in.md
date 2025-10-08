# in

## stream

## Lisp Bindings

### (in-stream) -> in_stream

### (in-next-msg in_stream) -> msg

## VP methods

### :create -> class/in/create

### :deinit -> class/in/deinit

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
trashes
:r1-:r14
```

### :init -> class/in/init

```code
inputs
:r0 = in object (ptr)
:r1 = vtable (pptr)
:r2 = 0, else mailbox id (uint)
outputs
:r0 = in object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :next_msg -> class/in/next_msg

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
trashes
:r1-:r14
```

### :read_next -> class/in/read_next

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r14
```

### :vtable -> class/in/vtable

