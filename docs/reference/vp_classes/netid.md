# netid

## str

## VP methods

### :create -> class/netid/create

```code
outputs
:r0 = 0 if error, else netid object (ptr)
trashes
:r1-:r6
```

### :deinit -> class/netid/deinit

```code
inputs
:r0 = netid object (ptr)
outputs
:r0 = netid object (ptr)
trashes
:r1-:r14
```

### :init -> class/netid/init

```code
inputs
:r0 = netid object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = netid object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r6
```

### :vtable -> class/netid/vtable

