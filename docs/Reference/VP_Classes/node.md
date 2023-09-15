# node

## str

### :create -> class/node/create

```code
inputs
:r0 = buffer (pubyte)
:r1 = buffer length (uint)
outputs
:r0 = 0 if error, else node object (ptr)
trashes
:r1-:r6
```

### :hash -> class/node/hash

```code
inputs
:r0 = node object (ptr)
outputs
:r0 = node object (ptr)
:r1 = hash code (ulong)
trashes
:r1-:r2
```

### :vtable -> class/node/vtable

