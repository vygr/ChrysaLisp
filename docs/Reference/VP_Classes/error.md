# error

## obj

## VP methods

### :create -> class/error/create

### :deinit -> class/error/deinit

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
trashes
:r1-:r14
```

### :get_description -> class/error/get_description

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = str object (ptr)
trashes
:r1
```

### :get_file -> class/error/get_file

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = str object (ptr)
trashes
:r1
```

### :get_frame -> class/error/get_frame

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = error payload object (ptr)
trashes
:r1
```

### :get_line -> class/error/get_line

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = line number (uint)
trashes
:r1
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

### :get_object -> class/error/get_object

```code
inputs
:r0 = error object (ptr)
outputs
:r0 = error object (ptr)
:r1 = error payload object (ptr)
trashes
:r1
```

### :init -> class/error/init

```code
inputs
:r0 = error object (ptr)
:r1 = vtable (pptr)
:r2 = description c string (pubyte)
:r3 = 0, else error msg index (uint)
:r4 = error payload object (ptr)
:r5 = filename c string (pubyte)
:r6 = line number (uint)
:r7 = stack frame (ptr)
outputs
:r0 = error object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :print -> class/error/print

```code
inputs
:r0 = error object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = error object (ptr)
trashes
:r1-:r14
```

### :vtable -> class/error/vtable

