# :in

```image
docs/reference/vp_classes/in.cwb
```

## :stream

## Lisp Bindings

### (in-stream) -> in_stream

### (in-next-msg in_stream) -> msg

## VP methods

### :create -> class/in/create

```code
inputs
:r0 = 0, else mailbox id (uint)
outputs
:r0 = 0 if error, else in object (ptr)
trashes
:r0-:r5, :r14, :f0-:f15
```

### :deinit -> class/in/deinit

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
trashes
:r1-:r14, :f0-:f15
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
:r1-:r5, :f0-:f15
```

### :next_msg -> class/in/next_msg

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
trashes
:r1-:r8, :f0-:f15
```

### :read_next -> class/in/read_next

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
:r1 = -1 for EOF, else more data
trashes
:r1-:r8, :f0-:f15
```

### :type -> class/in/type

```code
inputs
:r0 = in object (ptr)
outputs
:r0 = in object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vtable -> class/in/vtable

