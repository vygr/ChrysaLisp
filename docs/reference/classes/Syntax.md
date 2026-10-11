# Syntax

```image
docs/reference/classes/Syntax.cwb
```

```code
(Syntax) -> syntax

default colours and state
```

### :colorise

```code
(. syntax :colorise str) -> array

assign colours
```

### :get_keywords

### :get_state

### :line_compress

```code
(. syntax :line_compress line tab_width) -> line
```

### :line_expand

```code
(. syntax :line_expand line tab_width) -> line
```

### :set_colors

```code
(. syntax :set_colors fmap) -> syntax
```

### :set_state

### :tokenize

```code
(. syntax :tokenize line) -> (token_list state_list)

a string that began on a line before can end with the first
character of this one, one that begins here not with its first
```

