# Ring

```code
(Ring capacity_req [init_seq]) -> ring

Creates a generic ring-buffer over a init_sequence (defaults to str).
```

### :get_pos

### :match

```code
(. ring :match offset match_len) -> ring
```

### :write

```code
(. ring :write init_seq) -> ring
```

### :write_stream

```code
(. ring :write_stream stream out_len) -> ring
```

