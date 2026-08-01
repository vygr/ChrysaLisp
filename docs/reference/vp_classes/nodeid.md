# :nodeid

## :str

## VP methods

### :create -> class/nodeid/create

```code
inputs
:r0 = buffer (pubyte)
outputs
:r0 = 0 if error, else nodeid object (ptr)
trashes
:r0-:r5, :f0-:f15
```

### :eql -> class/nodeid/eql

```code
inputs
:r0 = node1 object (ptr)
:r1 = node2 object (ptr)
outputs
:r0 = node1 object (ptr)
:r1 = 0 if same, else not
trashes
:r1-:r4
```

### :hash -> class/nodeid/hash

```code
inputs
:r0 = nodeid object (ptr)
outputs
:r0 = nodeid object (ptr)
:r1 = hash code (long)
trashes
:r1-:r2
```

### :type -> class/nodeid/type

### :vtable -> class/nodeid/vtable

