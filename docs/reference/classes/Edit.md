# Edit

## View

```code
(Edit) -> edit
```

### :add_cursor

### :add_found_cursors

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

### :copy

### :cut

### :delete

### :down

### :down_select

### :end

### :end_select

### :find

### :floor_selection

### :get_buffer

### :get_buffer_found

### :get_cursor

### :get_focus

### :get_primary_text

### :get_scroll

```code
(. edit :get_scroll) -> (x y)
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

### :left_bracket_select

### :left_select

### :left_tab

### :left_white_space

### :left_white_space_select

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

### :next_found_cursor

### :paste

### :prev_found_cursor

### :primary_cursor

### :redo

### :reflow

### :rewind

### :right

### :right_bracket

### :right_bracket_select

### :right_select

### :right_tab

### :right_white_space

### :right_white_space_select

### :select_all

### :select_block

### :select_form

### :select_line

### :select_paragraph

### :select_word

### :set_buffer

### :set_cursor

### :set_focus

### :set_found_color

```code
(. edit :set_found_color argb) -> edit
```

### :set_found_cursors

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

### :undo

### :unique

### :up

### :up_select

