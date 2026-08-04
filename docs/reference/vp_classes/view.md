# :view

## :hmap

## Lisp Bindings

### (view-add-front parent mth child)

### (view-add-back parent mth child)

### (view-add-dirty view mth x y w h)

### (view-add-opaque view mth x y w h)

### (view-children view mth)

### (view-clr-opaque view mth)

### (create-view)

### (view-find-id view mth id)

### (view-flatten view mth)

### (view-hide view mth)

### (view-hit-tree view mth x y)

### (view-set-flags view mth flags mask)

### (view-sub view mth)

### (view-sub-opaque view mth x y w h)

### (view-to-back view mth)

### (view-to-front view mth)

### (view-trans-dirty view mth rx ry)

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
:r1-:r14, :f0-:f15
callback api
in
:r0 = view object (ptr)
:r1 = user data pointer (ptr)
out
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
:r1-:r14, :f0-:f15
```

### :draw -> gui/view/draw

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
callback api
in
:r0 = view object (ptr)
:r1 = user data pointer (ptr)
out
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
:r1-:r9
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
:r1-:r9
```

### :hide -> gui/view/hide

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r5, :f0-:f15
```

### :ref_prop -> gui/view/ref_prop

```code
inputs
:r0 = view object (ptr)
:r1 = static sym offset (uint)
outputs
:r0 = view object (ptr)
:r1 = 0 else, property object (ptr)
trashes
:r1-:r9
```

### :set_flags -> gui/view/set_flags

```code
inputs
:r0 = view object (ptr)
:r1 = flag values (long)
:r2 = flag mask (long)
outputs
:r0 = view object (ptr)
:r1 = new flag values (long)
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
:r1-:r14, :f0-:f15
```

### :to_front -> gui/view/to_front

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
trashes
:r1-:r5
```

### :vtable -> gui/view/vtable

