# nodeid

## str

## VP methods

### :create -> class/nodeid/create

```code
inputs
:r0 = buffer (pubyte)
outputs
:r0 = 0 if error, else nodeid object (ptr)
trashes
:r1-:r6
```

### :hash -> class/nodeid/hash

```code
inputs
:r0 = nodeid object (ptr)
outputs
:r0 = nodeid object (ptr)
:r1 = hash code (ulong)
trashes
:r1-:r2
```

### :vtable -> class/nodeid/vtable

