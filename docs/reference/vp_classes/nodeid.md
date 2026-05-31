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

### :vtable -> class/nodeid/vtable

