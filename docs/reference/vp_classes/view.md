# view

## hmap

## Lisp Bindings

### (view-add-front parent child)

### (view-add-back parent child)

### (view-add-dirty view x y w h)

### (view-add-opaque view x y w h)

### (view-children view)

### (view-clr-opaque view)

### (create-view)

### (view-find-id view id)

### (view-flatten view)

### (view-hide view)

### (view-hit-tree view x y)

### (view-set-flags view flags mask)

### (view-sub view)

### (view-sub-opaque view x y w h)

### (view-to-back view)

### (view-to-front view)

### (view-trans-dirty view rx ry)

## VP methods

### :add_back -> gui/view/add_back

```code
inputs
:r0 = view object (ptr)
:r1 = child view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r3
```

### :add_front -> gui/view/add_front

```code
inputs
:r0 = view object (ptr)
:r1 = child view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r3
```

### :backward_tree -> gui/view/backward_tree

```code
inputs
:r0 = view object (ptr)
:r1 = user data pointer
:r2 = down callback (ptr)
:r3 = up callback (ptr)
outputs
:r0 = view object (ptr)
trashes
...
callback api
inputs
:r0 = view object (ptr)
:r1 = user data pointer (ptr)
outputs
:r0 = view object (ptr)
:r1 = 0 if should not descend after down callback
trashes
...
```

### :create -> gui/view/create

### :deinit -> gui/view/deinit

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :draw -> class/view/draw

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :find_id -> gui/view/find_id

```code
inputs
:r0 = view object (ptr)
:r1 = target id (long)
outputs
:r0 = view object (ptr)
:r1 = 0 if not found, else view object (ptr)
trashes
:r1-:r3
```

### :forward_tree -> gui/view/forward_tree

```code
inputs
:r0 = view object (ptr)
:r1 = user data pointer
:r2 = down callback (ptr)
:r3 = up callback (ptr)
outputs
:r0 = view object (ptr)
trashes
...
callback api
inputs
:r0 = view object (ptr)
:r1 = user data pointer (ptr)
outputs
:r0 = view object (ptr)
:r1 = 0 if should not descend after down callback
trashes
...
```

### :forward_tree_callback -> class/obj/null

### :get_long_prop -> gui/view/get_long_prop

```code
inputs
:r0 = view object (ptr)
:r1 = static sym num (uint)
outputs
:r0 = view object (ptr)
:r1 = property value (long)
trashes
:r1-:r14
```

### :get_prop -> gui/view/get_prop

```code
inputs
:r0 = view object (ptr)
:r1 = static sym num (uint)
outputs
:r0 = view object (ptr)
:r1 = 0 else, property object (ptr)
trashes
:r1-:r14
```

### :hide -> gui/view/hide

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :hit -> gui/view/hit

```code
inputs
:r0 = view object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
outputs
:r0 = view object (ptr)
:r1 = 0 if not, else hit
trashes
:r1
```

### :hit_tree -> gui/view/hit_tree

```code
inputs
:r0 = view object (ptr)
:r7 = x (pixels)
:r8 = y (pixels)
outputs
:r0 = view object (ptr)
:r1 = 0 if not hit, else hit view object (ptr)
:r7 = x relative to hit (pixels)
:r8 = y relative to hit (pixels)
trashes
:r1-:r3
```

### :init -> gui/view/init

```code
inputs
:r0 = view object (ptr)
:r1 = vtable (pptr)
outputs
:r0 = view object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :ref_prop -> gui/view/ref_prop

```code
inputs
:r0 = view object (ptr)
:r1 = static sym num (uint)
outputs
:r0 = view object (ptr)
:r1 = 0 else, property object (ptr)
trashes
:r1-:r14
```

### :set_flags -> gui/view/set_flags

```code
inputs
:r0 = view object (ptr)
:r1 = flag values (ulong)
:r2 = flag mask (ulong)
outputs
:r0 = view object (ptr)
:r1 = new flag values (ulong)
trashes
:r1-:r4
```

### :sub -> gui/view/sub

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r2
```

### :to_back -> gui/view/to_back

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :to_front -> gui/view/to_front

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14
```

### :vtable -> gui/view/vtable

