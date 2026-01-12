# Buffer

```code
(Buffer [mode syntax]) -> buffer
```

### :add_cursor

```code
(. buffer :set_cursor cx cy [ax ay]) -> buffer
```

### :add_icursor

```code
(. buffer :add_icursor ci [ai]) -> buffer
```

### :backspace

```code
(. buffer :backspace) -> buffer
```

### :bottom

```code
(. buffer :bottom) -> buffer
```

### :bottom_select

```code
(. buffer :bottom_select) -> buffer
```

### :clear_undo

```code
(. buffer :clear_undo) -> buffer
```

### :copy

```code
(. buffer :copy) -> text
```

### :cursor_to_index

```code
(. buffer :cursor_to_index csr) -> idx

clips to min/max of buffer
```

### :cut

```code
(. buffer :cut) -> text
```

### :delete

```code
(. buffer :delete) -> buffer
```

### :down

```code
(. buffer :down) -> buffer
```

### :down_select

```code
(. buffer :down_select) -> buffer
```

### :end

```code
(. buffer :end) -> buffer
```

### :end_select

```code
(. buffer :end_select) -> buffer
```

### :file_load

```code
(. buffer :file_load filepath) -> buffer
```

### :file_load_hex

```code
(. buffer :file_load_hex filepath [width]) -> buffer
```

### :file_save

```code
(. buffer :file_save filepath) -> buffer
```

### :find

```code
(. buffer :find pattern wmode rmode) -> buffer_found
```

### :get_buffer_found

### :get_buffer_lines

### :get_cursor

```code
(. buffer :get_cursor) -> (cx cy ax ay)
```

### :get_cursors

### :get_icursor

```code
(. buffer :get_icursor) -> (ci ai)
```

### :get_modified

### :get_selected

```code
(. buffer :get_selected) -> (((cx cy) (ax ay)) ...)
```

### :get_selected_unsorted

```code
(. buffer :get_selected_unsorted) -> ((cx cy ax ay) ...)
```

### :get_size

```code
(. buffer :get_size) -> (width height)
```

### :get_syntax_engine

### :get_tab_width

### :get_tcursors

### :get_text_line

```code
(. buffer :get_text_line y) -> line
```

### :get_wrap_width

### :home

```code
(. buffer :home) -> buffer
```

### :home_select

```code
(. buffer :home_select) -> buffer
```

### :icopy

```code
(. buffer :icopy si ei) -> str
```

### :idelete

```code
(. buffer :idelete si ei) -> buffer
```

### :iinsert

```code
(. buffer :iinsert si text) -> buffer
```

### :index_to_cursor

```code
(. buffer :index_to_cursor idx) -> (x y)

clips to min/max of buffer
```

### :insert

```code
(. buffer :insert text) -> buffer
```

### :left

```code
(. buffer :left) -> buffer
```

### :left_bracket

```code
(. buffer :left_bracket) -> (x y) | (:nil :nil)
```

### :left_select

```code
(. buffer :left_select) -> buffer
```

### :merge_cursors

```code
(. buffer :merge_cursors cursors) -> cursors
```

### :next_mark

```code
(. buffer :next_mark) -> mark
```

### :paste

```code
(. buffer :paste text) -> buffer
```

### :push_undo

```code
(. buffer :push_undo record ...) -> buffer
```

### :redo

```code
(. buffer :redo) -> buffer
```

### :right

```code
(. buffer :right) -> buffer
```

### :right_bracket

```code
(. buffer :right_bracket) -> (x y) | (:nil :nil)
```

### :right_select

```code
(. buffer :right_select) -> buffer
```

### :set_cursor

```code
(. buffer :set_cursor cx cy [ax ay]) -> buffer
```

### :set_cursors

### :set_icursor

```code
(. buffer :set_icursor ci [ai]) -> buffer
```

### :set_tcursors

### :top

```code
(. buffer :top) -> buffer
```

### :top_select

```code
(. buffer :top_select) -> buffer
```

### :undo

```code
(. buffer :undo) -> buffer
```

### :up

```code
(. buffer :up) -> buffer
```

### :up_select

```code
(. buffer :up_select) -> buffer
```

### :vdu_load

```code
(. buffer :vdu_load vdu scroll_x scroll_y [end_state]) -> buffer
```

