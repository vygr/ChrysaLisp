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

### :get_vdu_text

### :home

### :home_select

### :insert

### :invert

### :layout

```code
(. edit :layout) -> edit
```

### :left

### :left_bracket

### :left_select

### :left_tab

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

### :right

### :right_bracket

### :right_select

### :right_tab

### :select_all

### :select_block

### :select_form

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

### :split

### :tab

### :to_lower

### :to_upper

### :top

### :top_select

### :trim

### :underlay

```code
(. edit :underlay) -> edit

create the masks for selections, brackets and cursors
```

### :unique

### :up

### :up_select

