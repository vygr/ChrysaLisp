# View

```code
(View) -> view

overide the default 'this' env with a View component
```code

### :add_back

```code
(. view :add_back child) -> view
```code

### :add_child

```code
(. view :add_child child) -> view
```code

### :add_dirty

```code
(. view :add_dirty x y width height) -> view
```code

### :add_front

```code
(. view :add_front child) -> view
```code

### :add_opaque

```code
(. view :add_opaque x y width height) -> view
```code

### :change

```code
(. view :change x y width height [flag]) -> view
```code

### :change_dirty

```code
(. view :change_dirty x y width height) -> view
```code

### :children

```code
(. view :children) -> (child0 child1 ...)
```code

### :clr_opaque

```code
(. view :clr_opaque) -> view
```code

### :connect

```code
(. view :connect id) -> view
```code

### :ctx_blit

```code
(. view :ctx_blit tid col x y width height) -> view
```code

### :ctx_box

```code
(. view :ctx_box x y width height) -> view
```code

### :ctx_filled_box

```code
(. view :ctx_filled_box x y width height) -> view
```code

### :ctx_panel

```code
(. view :ctx_panel col flags depth x y width height) -> view
```code

### :ctx_set_color

```code
(. view :ctx_set_color col) -> view
```code

### :dirty

```code
(. view :dirty) -> view
```code

### :dirty_all

```code
(. view :dirty_all) -> view
```code

### :emit

```code
(. view :emit) -> view
```code

### :find_id

```code
(. view :find_id target_id) -> :nil | target_view
```code

### :find_owner

```code
(. view :find_owner) -> :nil | netid
```code

### :get_bounds

```code
(. view :get_bounds) -> (x y width height)
```code

### :get_flags

```code
(. view :get_flags) -> flags
```code

### :get_id

```code
(. view :get_id) -> id
```code

### :get_pos

```code
(. view :get_pos) -> (x y)
```code

### :get_size

```code
(. view :get_size) -> (width height)
```code

### :hide

```code
(. view :hide) -> view
```code

### :hit_tree

```code
(. view :hit_tree x y) -> (hit_view | :nil rx ry)
```code

### :layout

```code
(. view :layout) -> view
```code

### :pref_size

```code
(. view :pref_size) -> (width height)
```code

### :set_bounds

```code
(. view :set_size x y width height) -> view
```code

### :set_flags

```code
(. view :set_flags value mask) -> view
```code

### :set_pos

```code
(. view :set_pos x y) -> view
```code

### :set_size

```code
(. view :set_size width height) -> view
```code

### :sub

```code
(. view :lisp_sub) -> view
```code

### :sub_opaque

```code
(. view :sub_opaque x y width height) -> view
```code

### :to_back

```code
(. view :to_back) -> view
```code

### :to_front

```code
(. view :to_front) -> view
```code

### :trans_dirty

```code
(. view :trans_dirty rx ry) -> view
```code

