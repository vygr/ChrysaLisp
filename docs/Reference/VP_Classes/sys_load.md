# sys_load

### :bind -> sys/load/bind

```code
input
:r0 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r1-:r7
```

### :find -> sys/load/find

```code
inputs
:r0 = code pointer (ptr)
outputs
:r0 = 0, else function header pointer (ptr)
:r1 = function header offset (uint)
trashes
:r0-:r2
```

### :init -> sys/load/init

```code
inputs
system argv
host OS function table
host GUI function table
info
register inputs are dependant on the platform ABI
they are extracted via (abi-arg 0)-(abi-arg 2).
we need to keep the statics function on the front
of the tail block, even though it dosn't get used
because the boot image has relative link references
to the 'sys/statics/statics' string in its header !
```

### :lisp_path -> sys/load/lisp_path

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :load -> sys/load/load

```code
input
:r0 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r1-:r7
```

