# :netid

```image
docs/reference/vp_classes/netid.cwb
```

## :str

## VP methods

### :create -> class/netid/create

```code
inputs
none
outputs
:r0 = 0 if error, else netid object (ptr)
trashes
:r0-:r6, :f0-:f15
```

### :deinit -> class/netid/deinit

```code
inputs
:r0 = netid object (ptr)
outputs
:r0 = netid object (ptr)
trashes
:r1-:r5
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
:r1-:r6, :f0-:f15
```

### :type -> class/netid/type

```code
inputs
:r0 = netid object (ptr)
outputs
:r0 = netid object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vtable -> class/netid/vtable

