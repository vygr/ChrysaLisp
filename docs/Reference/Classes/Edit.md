# Edit

## View

```code
(Edit) -> edit
```

### :backspace

```code
(. edit :backspace) -> edit
```

### :break

```code
(. edit :break) -> edit
```

### :char_pos

```code
(. edit :char_pos event) -> (x y)
```

### :clear_selection

```code
(. edit :clear_selection) -> edit
```

### :comment

```code
(. edit :comment) -> edit
```

### :delete

```code
(. edit :delete) -> edit
```

### :down

```code
(. edit :down) -> edit
```

### :down_select

```code
(. edit :down_select) -> edit
```

### :end

```code
(. edit :end) -> edit
```

### :end_select

```code
(. edit :end_select) -> edit
```

### :get_anchor

```code
(. edit :get_anchor) -> (x y)
```

### :get_buffer

```code
(. edit :get_buffer) -> text_buffer
```

### :get_cursor

```code
(. edit :get_cursor) -> (x y)
```

### :get_find

```code
(. edit :get_find) -> (x y x1 y1)
```

### :get_scroll

```code
(. edit :get_scroll) -> (x y)
```

### :get_vdu_text

```code
(. edit :get_vdu_text) -> vdu_text
```

### :get_vdu_underlay

```code
(. edit :get_vdu_underlay) -> vdu_underlay
```

### :home

```code
(. edit :home) -> edit
```

### :home_select

```code
(. edit :home_select) -> edit
```

### :insert

```code
(. edit :insert string) -> edit
```

### :invert

```code
(. edit :invert) -> edit
```

### :layout

```code
(. edit :layout) -> edit
```

### :left

```code
(. edit :left) -> edit
```

### :left_bracket

```code
(. edit :left_bracket) -> edit
```

### :left_select

```code
(. edit :left_select) -> edit
```

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

### :mouse_wheel

```code
(. edit :mouse_wheel event) -> edit
```

### :pref_size

```code
(. edit :pref_size) -> (width height)
```

### :reflow

```code
(. edit :reflow) -> edit
```

### :right

```code
(. edit :right) -> edit
```

### :right_bracket

```code
(. edit :right_bracket) -> edit
```

### :right_select

```code
(. edit :right_select) -> edit
```

### :right_tab

```code
(. edit :right_tab) -> edit
```

### :select_all

```code
(. edit :select_all) -> edit
```

### :select_block

```code
(. edit :select_block) -> edit
```

### :select_line

```code
(. edit :select_line) -> edit
```

### :select_paragraph

```code
(. edit :select_paragraph) -> edit
```

### :select_word

```code
(. edit :select_word) -> edit
```

### :set_anchor

```code
(. edit :set_anchor x y) -> edit
```

### :set_buffer

```code
(. edit :set_buffer text_buffer) -> this
```

### :set_cursor

```code
(. edit :set_cursor x y) -> edit
```

### :set_find

```code
(. edit :set_find x y x1 y1) -> edit
```

### :set_scroll

```code
(. edit :set_scroll x y) -> edit
```

### :set_underlay_color

```code
(. edit :set_underlay_color argb) -> edit
```

### :set_underlay_find_color

```code
(. edit :set_underlay_find_color argb) -> edit
```

### :sort

```code
(. edit :sort) -> edit
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

### :underlay_brackets

```code
(. edit :underlay_brackets) -> edit

create the underlay for just bracket indicators
```

### :underlay_clear

```code
(. edit :underlay_clear) -> edit

create the underlay for clear
```

### :underlay_find

```code
(. edit :underlay_selection) -> edit
```

### :underlay_selection

```code
(. edit :underlay_selection) -> edit
```

### :unique

```code
(. edit :unique) -> edit
```

### :up

```code
(. edit :up) -> edit
```

### :up_select

```code
(. edit :up_select) -> edit
```

