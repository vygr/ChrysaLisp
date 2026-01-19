# Edit

## View

```code
(Edit) -> edit
```

### :backspace

### :bottom

### :bottom_select

### :char_pos

```code
(. edit :char_pos event) -> (x y)
```

### :clip_cursor

### :comment

```code
(. edit :comment) -> edit
```

### :constraint

```code
(. edit :constraint) -> (width height)
```

### :delete

### :down

### :down_select

### :end

### :end_select

### :floor_selection

### :get_buffer

### :get_cursor

### :get_focus

```code
(. edit :get_focus) -> (x y x1 y1)
```

### :get_scroll

```code
(. edit :get_scroll) -> (x y)
```

### :get_select

```code
(. edit :get_select) -> text
```

### :get_selected_extent

```code
(. edit :get_selected_extent) -> csr
```

### :get_vdu_text

### :home

### :home_select

### :insert

### :invert

```code
(. edit :invert) -> edit
```

### :layout

```code
(. edit :layout) -> edit
```

### :left

### :left_bracket

```code
(. edit :left_bracket) -> edit
```

### :left_select

### :left_tab

```code
(. edit :left_tab) -> edit
```

### :max_size

```code
(. edit :max_size) -> (width height)
```

### :mouse_down

```code
(. edit :mouse_down event) -> edit
```

### :mouse_move

```code
(. edit :mouse_move event) -> edit
```

### :mouse_up

```code
(. vdu :mouse_up event) -> vdu
```

### :mouse_wheel

```code
(. edit :mouse_wheel event) -> edit
```

### :reflow

```code
(. edit :reflow) -> edit
```

### :right

### :right_bracket

```code
(. edit :right_bracket) -> edit
```

### :right_select

### :right_tab

```code
(. edit :right_tab) -> edit
```

### :select_all

### :select_block

```code
(. edit :select_block) -> edit

FIXME to separate regions
```

### :select_line

### :select_paragraph

### :select_word

### :set_buffer

### :set_cursor

### :set_focus

```code
(. edit :set_focus x y x1 y1) -> edit
```

### :set_found_color

```code
(. edit :set_found_color argb) -> edit
```

### :set_ink_color

```code
(. edit :set_ink_color argb) -> edit
```

### :set_region_color

```code
(. edit :set_region_color argb) -> edit
```

### :set_scroll

```code
(. edit :set_scroll x y) -> edit
```

### :set_select_color

```code
(. edit :set_select_color argb) -> edit
```

### :sort

```code
(. edit :sort) -> edit
```

### :split

```code
(. edit :split) -> edit
```

### :tab

```code
(. edit :tab) -> edit
```

### :to_lower

```code
(. edit :to_lower) -> edit
```

### :to_upper

```code
(. edit :to_upper) -> edit
```

### :top

### :top_select

### :underlay

```code
(. edit :underlay) -> edit

create the masks for selections, brackets and cursors
```

### :unique

```code
(. edit :unique) -> edit
```

### :up

### :up_select

