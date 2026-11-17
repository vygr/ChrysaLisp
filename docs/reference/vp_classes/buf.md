# :buf

```image
docs/reference/vp_classes/buf.cwb
```

## :obj

## Lisp Bindings

### (buf size [key]) -> :nil | buffer

### (buf-get buf offset type) -> val

### (buf-set buf offset type val) -> buf

## VP methods

### :check -> class/buf/check

```code
inputs
:r0 = passed through (ptr)
:r1 = limit, how many bytes there are (ulong)
:r2 = offset (long)
:r3 = type (long)
outputs
:r0 = as it was (ptr)
:r1 = 0 if ok, 1 if it is no type, 2 if it is not all inside the limit,
or not at the alignment of its type
trashes
:r1, :r4-:r5
info
a field of a type, not a struct, is at its natural alignment,
docs/vm/vp_vm.md: what it is in starts on a word, so its offset is a
whole number of its size
```

### :create -> class/buf/create

```code
inputs
:r0 = size in bytes (ulong)
:r1 = key, 0 to make a block under a new key (long)
outputs
:r0 = 0 if error, else buf object (ptr)
trashes
:r0-:r3, :r8-:r14, :d0-:d15
```

### :deinit -> class/buf/deinit

```code
inputs
:r0 = buf object (ptr)
outputs
:r0 = buf object (ptr)
trashes
:r1-:r2, :d0-:d15
info
the block unmapped, and its key let go of if this made it
```

### :get -> class/buf/get

```code
inputs
:r0 = passed through (ptr)
:r1 = address (pubyte)
:r2 = type, not 0 (long)
outputs
:r0 = as it was (ptr)
:r1 = value object (ptr)
trashes
:r1-:r6, :r14, :d0-:d15
info
:r0 is passed through, the lisp object of a caller that jumps here
for the end of its binding
```

### :init -> class/buf/init

```code
inputs
:r0 = buf object (ptr)
:r1 = vtable (pptr)
:r2 = size in bytes (ulong)
:r3 = key, 0 to make a block under a new key (long)
outputs
:r0 = buf object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r2, :r8-:r13, :d0-:d15
info
the block of a key mapped on this node, or with a key of 0 a new one,
zero, under a key at random that no other block has. An error if there
is no block of that key of at least that size on this machine, or the
host has no shared memory
```

### :set -> class/buf/set

```code
inputs
:r0 = passed through (ptr)
:r1 = address (pubyte)
:r2 = type, not 0 (long)
:r3 = value object (ptr)
outputs
:r0 = as it was (ptr)
:r1 = 0 if set, 1 if a struct and the value has not that many bytes
trashes
:r1-:r7
info
a number its value. A struct, a type of more than 0, that many bytes
of a str, or of the elements of an array of numbers, a :nums, so a
vector goes in whole. Not of a list, a map or a set, their elements
are objects
```

### :type -> class/buf/type

```code
inputs
:r0 = buf object (ptr)
outputs
:r0 = buf object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r3, :d0-:d15
```

### :vtable -> class/buf/vtable

