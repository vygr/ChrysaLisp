# Buffer

```code
(Buffer [mode syntax]) -> buffer
```code

### :backspace

```code
(. buffer :backspace &optional num) -> buffer
```code

### :break

```code
(. buffer :break) -> buffer
```code

### :clear_undo

```code
(. buffer :clear_undo) -> buffer
```code

### :colorise

```code
(. buffer :colorise) -> buffer
```code

### :constrain

```code
(. buffer :constrain x y) -> (x y)
```code

### :copy

```code
(. buffer :copy anchor_x anchor_y) -> string
```code

### :cut

```code
(. buffer :cut anchor_x anchor_y) -> string
```code

### :delete

```code
(. buffer :delete &optional num) -> buffer
```code

### :down

```code
(. buffer :down) -> buffer
```code

### :file_load

```code
(. buffer :file_load filepath) -> buffer
```code

### :file_save

```code
(. buffer :file_save filepath) -> buffer
```code

### :find

```code
(. buffer :find pattern wmode rmode) -> buffer_found
```code

### :get_cursor

```code
(. buffer :get_cursor) -> (x y)
```code

### :get_modified

```code
(. buffer :get_modified) -> :t | :nil
```code

### :get_size

```code
(. buffer :get_size) -> (width height)
```code

### :get_syntax

```code
(. buffer :get_syntax) -> syntax
```code

### :get_tab_width

```code
(. buffer :get_tab_width) -> tab_width
```code

### :get_text_line

```code
(. buffer :get_text_line y) -> line
```code

### :get_text_lines

```code
(. buffer :get_text_lines) -> lines
```code

### :get_wrap_width

```code
(. buffer :get_wrap_width) -> wrap_width
```code

### :insert

```code
(. buffer :insert string) -> buffer
```code

### :left

```code
(. buffer :left) -> buffer
```code

### :left_bracket

```code
(. buffer :left_bracket) -> (x y) | (:nil :nil)
```code

### :next_mark

```code
(. buffer :next_mark) -> mark
```code

### :paste

```code
(. buffer :paste string) -> buffer
```code

### :push_undo

```code
(. buffer :push_undo record ...) -> buffer
```code

### :redo

```code
(. buffer :redo) -> buffer
```code

### :right

```code
(. buffer :right) -> buffer
```code

### :right_bracket

```code
(. buffer :right_bracket) -> (x y) | (:nil :nil)
```code

### :set_cursor

```code
(. buffer :set_cursor x y) -> buffer
```code

### :set_mode

```code
(. buffer :set_mode mode) -> buffer
```code

### :undo

```code
(. buffer :undo) -> buffer
```code

### :up

```code
(. buffer :up) -> buffer
```code

### :vdu_load

```code
(. buffer :vdu_load vdu scroll_x scroll_y) -> buffer
```code

