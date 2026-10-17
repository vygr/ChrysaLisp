# Scroll

```image
docs/reference/classes/Scroll.cwb
```

## View

```code
(Scroll flags) -> scroll
```

### :action

```code
(. scroll :action data) -> scroll

scrolled, what is in it moved and drawn again where it now is, its
lists of draws kept; the sliders are what changed
```

### :add_child

```code
(. scroll :add_child child) -> scroll
```

### :constraint

```code
(. scroll :constraint) -> (width height)
```

### :layout

```code
(. scroll :layout) -> scroll
```

### :mouse_wheel

```code
(. scroll :mouse_wheel event) -> scroll
```

### :visible

```code
(. scroll :visible descendant [flags]) -> scroll
```

