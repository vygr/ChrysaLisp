# :sys_load

```image
docs/reference/vp_classes/sys_load.cwb
```

## Lisp Bindings

### (load-path) -> path

### (task-server [folder])

## VP methods

### :bind -> sys/load/bind

```code
input
:r0 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r0-:r7, :f0-:f15
info
for the task's function server first, if it has one, then the node's
own. One that fails to bind for a server leaves nothing of it: what it
put on the server's list is taken off again
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
info
of the node's own functions, then of each server's
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
info
a function of the server of the bind if its folder has it, else of
the node. The links of one of the node's are bound to the node's
alone, so none of the node's own can call a server's
```

### :search -> sys/load/search

```code
input
:r6 = c string function path name (pubyte)
output
:r0 = 0 else, function entry pointer (ptr)
trashes
:r0-:r5, :r7
info
a function loaded already, of the server of the bind, then of the node
```

### :server -> sys/load/server

```code
inputs
:r0 = c string, the server's folder under the load path (pubyte)
outputs
:r0 = function server (ptr)
trashes
:r0-:r3, :r5-:r7, :f0-:f15
info
the node's server of that folder, made if it has none
```

### :sweep -> sys/load/sweep

```code
trashes
:r0-:r2, :r5-:r13, :f0-:f15
info
a server no task of the node has goes, its blocks and itself. Marked
by the tasks there are, not counted, so a task that went any way at
all, its server is let go of
```

