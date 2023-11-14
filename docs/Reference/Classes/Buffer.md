# Buffer

```code
(Buffer [mode syntax]) -> buffer
```

### :backspace

```code
(. buffer :backspace &optional num) -> buffer
```

### :break

```code
(. buffer :break) -> buffer
```

### :clear_undo

```code
(. buffer :clear_undo) -> buffer
```

### :constrain

```code
(. buffer :constrain x y) -> (x y)
```

### :copy

```code
(. buffer :copy anchor_x anchor_y) -> string
```

### :cut

```code
(. buffer :cut anchor_x anchor_y) -> string
```

### :delete

```code
(. buffer :delete &optional num) -> buffer
```

### :down

```code
(. buffer :down) -> buffer
```

### :file_load

```code
(. buffer :file_load filepath) -> buffer
```

### :file_save

```code
(. buffer :file_save filepath) -> buffer
```

### :find

```code
(. buffer :find pattern wmode rmode) -> buffer_found
```

### :get_cursor

```code
(. buffer :get_cursor) -> (x y)
```

### :get_found

```code
(. buffer :get_found) -> found
```

### :get_modified

```code
(. buffer :get_modified) -> :t | :nil
```

### :get_size

```code
(. buffer :get_size) -> (width height)
```

### :get_syntax

```code
(. buffer :get_syntax) -> syntax
```

### :get_tab_width

```code
(. buffer :get_tab_width) -> tab_width
```

### :get_text_line

```code
(. buffer :get_text_line y) -> line
```

### :get_text_lines

```code
(. buffer :get_text_lines) -> lines
```

### :get_wrap_width

```code
(. buffer :get_wrap_width) -> wrap_width
```

### :insert

```code
(. buffer :insert string) -> buffer
```

### :left

```code
(. buffer :left) -> buffer
```

### :left_bracket

```code
(. buffer :left_bracket) -> (x y) | (:nil :nil)
```

### :next_mark

```code
(. buffer :next_mark) -> mark
```

### :paste

```code
(. buffer :paste string) -> buffer
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

### :set_cursor

```code
(. buffer :set_cursor x y) -> buffer
```

### :undo

```code
(. buffer :undo) -> buffer
```

### :up

```code
(. buffer :up) -> buffer
```

### :vdu_load

```code
(. buffer :vdu_load vdu scroll_x scroll_y &optional end_state) -> buffer
```

