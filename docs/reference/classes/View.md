# View

```code
(View) -> view

override the default 'this' env with a View component
```

### :add_back

```code
(. view :add_back child) -> view
```

### :add_child

```code
(. view :add_child child) -> view
```

### :add_dirty

```code
(. view :add_dirty x y width height) -> view
```

### :add_front

```code
(. view :add_front child) -> view
```

### :add_opaque

```code
(. view :add_opaque x y width height) -> view
```

### :change

```code
(. view :change x y width height [flag]) -> view
```

### :change_dirty

```code
(. view :change_dirty x y width height [flag]) -> view
```

### :children

```code
(. view :children) -> (child0 child1 ...)
```

### :clr_opaque

```code
(. view :clr_opaque) -> view
```

### :connect

```code
(. view :connect id) -> view
```

### :constrain

```code
(. view :constrain [flag]) -> view
```

### :constraint

```code
(. view :constraint) -> (width height)
```

### :constraints

```code
(. view :constraints) -> view
```

### :ctx_blit

```code
(. view :ctx_blit tid col x y width height) -> view
```

### :ctx_box

```code
(. view :ctx_box x y width height) -> view
```

### :ctx_filled_box

```code
(. view :ctx_filled_box x y width height) -> view
```

### :ctx_panel

```code
(. view :ctx_panel col flags depth x y width height) -> view
```

### :ctx_set_color

```code
(. view :ctx_set_color col) -> view
```

### :dirty

```code
(. view :dirty) -> view
```

### :dirty_all

```code
(. view :dirty_all) -> view
```

### :emit

```code
(. view :emit) -> view
```

### :find_id

```code
(. view :find_id target_id) -> :nil | target_view
```

### :find_owner

```code
(. view :find_owner) -> :nil | netid
```

### :flatten

```code
(. view :flatten) -> (child0 child1 ...)
```

### :get_bounds

```code
(. view :get_bounds) -> (x y width height)
```

### :get_constraint

```code
(. view :get_constraint) -> (width height)
```

### :get_flags

```code
(. view :get_flags) -> flags
```

### :get_id

```code
(. view :get_id) -> id
```

### :get_pos

```code
(. view :get_pos) -> (x y)
```

### :get_relative

```code
(. view :get_relative child) -> (x y w h)
```

### :get_size

```code
(. view :get_size) -> (width height)
```

### :hide

```code
(. view :hide) -> view
```

### :hit_tree

```code
(. view :hit_tree x y) -> (hit_view | :nil rx ry)
```

### :layout

```code
(. view :layout) -> view
```

### :pref_size

```code
(. view :pref_size) -> (width height)
```

### :set_bounds

```code
(. view :set_size x y width height) -> view
```

### :set_constraint

```code
(. view :set_constraint width height) -> view
```

### :set_flags

```code
(. view :set_flags value mask) -> view
```

### :set_owner

```code
(. view :set_owner netid) -> view
```

### :set_pos

```code
(. view :set_pos x y) -> view
```

### :set_size

```code
(. view :set_size width height) -> view
```

### :sub

```code
(. view :lisp_sub) -> view
```

### :sub_opaque

```code
(. view :sub_opaque x y width height) -> view
```

### :to_back

```code
(. view :to_back) -> view
```

### :to_front

```code
(. view :to_front) -> view
```

### :trans_dirty

```code
(. view :trans_dirty rx ry) -> view
```

