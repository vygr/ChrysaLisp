# Edit

## View

```code
(Edit) -> edit
```code

### :backspace

```code
(. edit :backspace) -> edit
```code

### :break

```code
(. edit :break) -> edit
```code

### :char_pos

```code
(. edit :char_pos event) -> (x y)
```code

### :clear_selection

```code
(. edit :clear_selection) -> edit
```code

### :comment

```code
(. edit :comment) -> edit
```code

### :delete

```code
(. edit :delete) -> edit
```code

### :down

```code
(. edit :down) -> edit
```code

### :down_select

```code
(. edit :down_select) -> edit
```code

### :end

```code
(. edit :end) -> edit
```code

### :end_select

```code
(. edit :end_select) -> edit
```code

### :get_anchor

```code
(. edit :get_anchor) -> (x y)
```code

### :get_buffer

```code
(. edit :get_buffer) -> text_buffer
```code

### :get_cursor

```code
(. edit :get_cursor) -> (x y)
```code

### :get_scroll

```code
(. edit :get_scroll) -> (x y)
```code

### :get_vdu_text

```code
(. edit :get_vdu_text) -> vdu_text
```code

### :get_vdu_underlay

```code
(. edit :get_vdu_underlay) -> vdu_underlay
```code

### :home

```code
(. edit :home) -> edit
```code

### :home_select

```code
(. edit :home_select) -> edit
```code

### :insert

```code
(. edit :insert string) -> edit
```code

### :layout

```code
(. edit :layout) -> edit
```code

### :left

```code
(. edit :left) -> edit
```code

### :left_bracket

```code
(. edit :left_bracket) -> edit
```code

### :left_select

```code
(. edit :left_select) -> edit
```code

### :left_tab

```code
(. edit :left_tab) -> edit
```code

### :max_size

```code
(. edit :max_size) -> (width height)
```code

### :mouse_down

```code
(. edit :mouse_down event) -> edit
```code

### :mouse_move

```code
(. edit :mouse_move event) -> edit
```code

### :mouse_wheel

```code
(. edit :mouse_wheel event) -> edit
```code

### :ordered

```code
(. edit :ordered) -> edit
```code

### :ordered_unique

```code
(. edit :ordered_unique) -> edit
```code

### :pref_size

```code
(. edit :pref_size) -> (width height)
```code

### :reflow

```code
(. edit :reflow) -> edit
```code

### :right

```code
(. edit :right) -> edit
```code

### :right_bracket

```code
(. edit :right_bracket) -> edit
```code

### :right_select

```code
(. edit :right_select) -> edit
```code

### :right_tab

```code
(. edit :right_tab) -> edit
```code

### :select_all

```code
(. edit :select_all) -> edit
```code

### :select_block

```code
(. edit :select_block) -> edit
```code

### :select_line

```code
(. edit :select_line) -> edit
```code

### :select_paragraph

```code
(. edit :select_paragraph) -> edit
```code

### :select_word

```code
(. edit :select_word) -> edit
```code

### :set_anchor

```code
(. edit :set_anchor x y) -> this
```code

### :set_buffer

```code
(. edit :set_buffer text_buffer) -> this
```code

### :set_cursor

```code
(. edit :set_cursor x y) -> this
```code

### :set_scroll

```code
(. edit :set_scroll x y) -> this
```code

### :set_underlay_color

```code
(. edit :set_underlay_color argb) -> edit
```code

### :tab

```code
(. edit :tab) -> edit
```code

### :to_lower

```code
(. edit :to_lower) -> edit
```code

### :to_upper

```code
(. edit :to_upper) -> edit
```code

### :underlay_brackets

```code
(. edit :underlay_brackets) -> edit

create the underlay for just bracket indicators
```code

### :underlay_clear

```code
(. edit :underlay_clear) -> edit

create the underlay for clear
```code

### :underlay_selection

```code
(. edit :underlay_selection) -> edit
```code

### :up

```code
(. edit :up) -> edit
```code

### :up_select

```code
(. edit :up_select) -> edit
```code

