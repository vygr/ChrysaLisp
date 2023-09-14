# View

```code
(View) -> view
```code

### :add_child

```code
(. view :add_child child) -> view
```code

### :change

```code
(. view :change x y width height [flag]) -> view
```code

### :change_dirty

```code
(. view :change_dirty x y width height) -> view
```code

### :connect

```code
(. view :connect id) -> view
```code

### :ctx_panel

```code
(. view :ctx_panel col flags depth x y width height) -> view
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

### :set_pos

```code
(. view :set_pos x y) -> view
```code

### :set_size

```code
(. view :set_size width height) -> view
```code

