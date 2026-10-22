# :sys_load

```image
docs/reference/vp_classes/sys_load.cwb
```

## Lisp Bindings

### (load-path) -> path

## VP methods

### :bind -> sys/load/bind

```code
input
:r0 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r0-:r7, :f0-:f15
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
:r0 = argv of the host (pptr)
:r1 = the host's function tables, os gui audio net (pptr)
outputs
:r0 return code
trashes
:r0-:r14, :f0-:f15
info
called by the host, so its args are where the host's ABI puts them,
(vp-entry-abi) has them to :r0 :r1 as a host call would.
we need to keep the statics function on the front
of the tail block, even though it dosn't get used
because the boot image has relative link references
to the 'sys/statics/statics' string in its header !
```

### :load -> sys/load/load

```code
input
:r0 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r0-:r7, :f0-:f15
```

