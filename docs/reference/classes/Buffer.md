# Buffer

```code
(Buffer [flags syntax]) -> buffer
```

### :add_cursor

```code
(. buffer :add_cursor cx cy [ax ay]) -> buffer
```

### :add_found_cursors

```code
(. buffer :add_found_cursors buffer_found) -> buffer
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

### :clip_cursor

```code
(. buffer :clip_cursor cx cy [ax ay sx]) -> (cx cy ax ay sx)

clips coordinates to valid buffer positions
```

### :copy

```code
(. buffer :copy) -> text
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

### :floor_selection

```code
(. buffer :floor_selection) -> buffer
```

### :get_buffer_found

### :get_buffer_lines

### :get_cursor

```code
(. buffer :get_cursor) -> (cx cy ax ay)

return last cursor position without sticky x
```

### :get_cursors

### :get_modified

### :get_selected

```code
(. buffer :get_selected) -> ((cx cy ax ay) ...)

return cursors sorted by position, without sx
```

### :get_selected_extent

```code
(. buffer :get_selected_extent) -> csr
```

### :get_selected_unsorted

```code
(. buffer :get_selected_unsorted) -> ((cx cy ax ay) ...)

return cursors in original order, without sx
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
(. buffer :icopy cx cy ax ay) -> str

copy text from (cx cy) to (ax ay)
```

### :idelete

```code
(. buffer :idelete cx cy ax ay) -> buffer

delete text from (cx cy) to (ax ay)
```

### :iinsert

```code
(. buffer :iinsert cx cy text) -> buffer

insert text at position (cx cy)
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

cursors format: ((cx cy ax ay sx) ...)
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

### :set_found_cursors

```code
(. buffer :set_found_cursors buffer_found) -> buffer
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

