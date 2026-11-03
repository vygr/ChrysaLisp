# Scroll

```image
docs/reference/classes/Scroll.cwb
```

## View

```code
(Scroll flags) -> scroll

what it scrolls, its child, is the size it has, unless the scroll is
given how to size it, and how to be sized by it, (. scroll :fit)
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

and sized by it, if it is to be, (. scroll :fit)
```

### :constraint

```code
(. scroll :constraint) -> (width height)
```

### :fit

```code
(. scroll :fit) -> scroll

of a scroll given :child_fill, :fit_width or :fit_height, what it
scrolls is sized and laid out: its own size, and on a way of the
flags of :child_fill, at least the view less the slider. And the
scroll is sized by it, a percentage of its own size, :fit_width of
its width: the least the scroll may be on a way it has no slider,
what it would like, :want_width, on a way it has one. With :fit_min
the least on both: a scroll that is not what fills its flow is laid
out at its least, what it would like sizes only the window. Done as it is
laid out, and by its owner once what it scrolls has changed, before
the window is fitted to what it would like
```

### :layout

```code
(. scroll :layout) -> scroll
```

### :mouse_wheel

```code
(. scroll :mouse_wheel event) -> scroll
```

### :place

```code
(. scroll :place) -> scroll

the sliders and what it scrolls where they are now, nothing sized:
a scroll that is scrolled is only this
```

### :view_size

```code
(. scroll :view_size) -> (width height)

what of it shows what it scrolls, its size less its sliders
```

### :visible

```code
(. scroll :visible descendant [flags]) -> scroll
```

