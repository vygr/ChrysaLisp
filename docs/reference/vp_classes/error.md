# :error

```image
docs/reference/vp_classes/error.cwb
```

## :obj

## VP methods

### :create -> class/error/create

```code
inputs
:r0 = description c string (pubyte)
:r1 = 0, else error msg index (uint)
:r2 = error payload object (ptr)
:r3 = script string (ptr)
:r4 = stream string (ptr)
:r5 = stream line number (uint)
:r6 = stack frame (ptr)
outputs
:r0 = 0 if error, else error object (ptr)
trashes
:r0-:r8, :r14, :f0-:f15
```

### :deinit -> class/error/deinit

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :get_msg -> class/error/get_msg

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = error c string (pubyte)
trashes
:r1-:r5
```

### :init -> class/error/init

```code
inputs
:r0 = error object (ptr)
:r1 = vtable (pptr)
:r2 = description c string (pubyte)
:r3 = 0, else error msg index (uint)
:r4 = error payload object (ptr)
:r5 = script string (ptr)
:r6 = stream string (ptr)
:r7 = stream line number (uint)
:r8 = stack frame (ptr)
outputs
:r0 = error object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r5, :f0-:f15
```

### :print -> class/error/print

```code
inputs
:r0 = error object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = error object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :type -> class/error/type

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vtable -> class/error/vtable

