# view

## hmap

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

### :find_owner -> gui/view/find_owner

```code
inputs
:r0 = view object (ptr)
outputs
:r0 = view object (ptr)
:r1-:r3 = 0, else mailbox ID of owner (net_id)
trashes
:r1-:r4
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

### :lisp_add -> gui/view/lisp_add

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-add-front parent child)
```

### :lisp_add_back -> gui/view/lisp_add_back

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-add-back parent child)
```

### :lisp_add_dirty -> gui/view/lisp_add_dirty

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-add-dirty view x y w h)
```

### :lisp_add_opaque -> gui/view/lisp_add_opaque

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-add-opaque view x y w h)
```

### :lisp_children -> gui/view/lisp_children

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-children view)
```

### :lisp_clr_opaque -> gui/view/lisp_clr_opaque

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-clr-opaque view)
```

### :lisp_create -> gui/view/lisp_create

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(create-view)
```

### :lisp_find_id -> gui/view/lisp_find_id

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-find-id view id)
```

### :lisp_hide -> gui/view/lisp_hide

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-hide view)
```

### :lisp_hit_tree -> gui/view/lisp_hit_tree

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-hit-tree view x y)
```

### :lisp_set_flags -> gui/view/lisp_set_flags

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-set-flags view flags mask)
```

### :lisp_sub -> gui/view/lisp_sub

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-sub view)
```

### :lisp_sub_opaque -> gui/view/lisp_sub_opaque

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-sub-opaque view x y w h)
```

### :lisp_to_back -> gui/view/lisp_to_back

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-to-back view)
```

### :lisp_to_front -> gui/view/lisp_to_front

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-to-front view)
```

### :lisp_trans_dirty -> gui/view/lisp_trans_dirty

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(view-trans-dirty view rx ry)
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

