# :entry

```image
docs/reference/vp_classes/entry.cwb
```

## :str

## VP methods

### :create -> class/entry/create

```code
inputs
:r0 = service key str object (ptr)
outputs
:r0 = entry object (ptr)
trashes
:r0-:r5, :d0-:d15
info
an entry of the key's characters. Not the key itself, the service map
holds that, so it would never be let go of
```

### :deinit -> class/entry/deinit

```code
inputs
:r0 = entry object (ptr)
outputs
:r0 = entry object (ptr)
trashes
:r1-:r14, :d0-:d15
info
the service forgotten, if it is still there
```

### :type -> class/entry/type

```code
inputs
:r0 = entry object (ptr)
outputs
:r0 = entry object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vtable -> class/entry/vtable

