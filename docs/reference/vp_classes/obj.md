# :obj

```image
docs/reference/vp_classes/obj.cwb
```

## Lisp Bindings

### (eql obj obj) -> :nil | :t

### (obj-get obj offset type) -> val

### (hash obj) -> num

### (nql obj obj) -> :nil | :t

### (obj-ref num) -> obj

### (obj-set obj offset type val) -> obj

### (type-of obj) -> (... :obj)

### (weak-ref obj) -> num

## VP methods

### :dead -> class/obj/dead

### :deinit -> class/obj/null

### :deref -> class/obj/deref

```code
inputs
:r0 = object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :deref_if -> class/obj/deref_if

```code
inputs
:r0 = 0, else object (ptr)
trashes
:r0-:r2, :f0-:f15
```

### :destroy -> class/obj/destroy

```code
inputs
:r0 = object (ptr)
trashes
:r0-:r2, :f0-:f15
info
an object is destroyed from inside the destroying of what held it: a
list in a list in a list, and each is a call further down the task's
stack, which is not long. A list 250 deep was the end of its task.

The first destroy of a task, the one no other is going on outside
of, says so in the task, and stays till all is done. One inside it
goes as it always did if there is the room on the stack. If there is
little left, the object is not destroyed there: it is put by, a note
of it on a list the task has, and that is all.
The first destroy, when it has done its own, does each that was put
by, one after another and no deeper, till there are none. So all is
destroyed before the first destroy comes back, as it was, however
deep it went.

And it leaves every register but :r0 to :r2 as it was, whatever the
object is: they are put on the stack round its deinit, which may do
anything, and brought back. So a deref trashes what a free does and
no more, and what calls one need not go by what it happens to know
of the object. That is 96 bytes of stack for each one inside
another, so with the usual stack about the thirtieth deep is the
first to be put by.
```

### :eql -> class/obj/eql

```code
inputs
:r0 = obj object (ptr)
:r1 = obj object (ptr)
outputs
:r0 = obj object (ptr)
:r1 = 0 if same, else not
trashes
:r1
```

### :hash -> class/obj/hash

```code
inputs
:r0 = object (ptr)
outputs
:r0 = object (ptr)
:r1 = hash code (long)
trashes
:r1
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
:r1-:r5, :f0-:f15
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
:r1-:r3, :f0-:f15
```

### :vtable -> class/obj/vtable

