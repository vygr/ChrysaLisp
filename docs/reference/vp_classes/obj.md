# obj

## Lisp Bindings

### (get-field obj field size|0)

### (hash obj)

### (obj-ref num)

### (set-field obj field size|0 val)

### (type-of obj)

### (weak-ref obj)

## VP methods

### :deinit -> class/obj/null

### :deref -> class/obj/deref

```code
inputs
:r0 = object (ptr)
trashes
:r0-:r14
```

### :deref_if -> class/obj/deref_if

```code
inputs
:r0 = 0, else object (ptr)
trashes
:r0-:r14
```

### :destroy -> class/obj/destroy

```code
inputs
:r0 = object (ptr)
trashes
:r0-:r14
```

### :hash -> class/obj/hash

```code
inputs
:r0 = object (ptr)
outputs
:r0 = object (ptr)
:r1 = hash code (ulong)
trashes
:r1-:r14
```

### :init -> class/obj/init

```code
inputs
:r0 = object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = object (ptr)
:r1 = 0 if error, else ok
trashes
:r1
```

### :inst_of -> class/obj/inst_of

```code
inputs
:r0 = object (ptr)
:r1 = vtable of tested type (ptr)
outputs
:r0 = object (ptr)
:r1 = 0 if not, else vtable of object (ptr)
trashes
:r1-:r2
```

### :null -> class/obj/null

### :print -> class/obj/print

```code
inputs
:r0 = object (ptr)
:r1 = stream object (ptr)
outputs
:r0 = object (ptr)
trashes
:r1-:r14
```

### :ref -> class/obj/ref

```code
inputs
:r0 = object (ptr)
outputs
:r0 = object (ptr)
trashes
:r1
```

### :ref_if -> class/obj/ref_if

```code
inputs
:r0 = 0, else object (ptr)
outputs
:r0 = 0, else object (ptr)
trashes
:r1
```

### :type -> class/obj/type

```code
inputs
:r0 = obj object (ptr)
outputs
:r0 = obj object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r14
```

### :vtable -> class/obj/vtable

