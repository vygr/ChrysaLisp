# Window

```image
docs/reference/classes/Window.cwb
```

## View

```code
(Window) -> window
```

### :add_child

```code
(. window :add_child child) -> window
```

### :constraint

```code
(. window :constraint) -> (width height)
```

### :dispatch

```code
(. window :dispatch event) -> :t | :nil

standard action and keyboard event dispatching
```

### :drag_mode

```code
(. window :drag_mode rx ry) -> (drag_mode drag_offx drag_offy)
```

### :draw

```code
(. window :draw) -> window
```

### :event

```code
(. window :event event) -> window
```

### :layout

```code
(. window :layout) -> window
```

### :mouse_down

```code
(. window :mouse_down event) -> window
```

### :mouse_move

```code
(. window :mouse_move event) -> window
```

### :theme

```code
(. window :theme name) -> window

the theme of the desktop is now this one. Each symbol font this
task has is changed for the same size of the new theme's, here
and in every view of the window that holds it, then the window is
laid out again and drawn. A view has a font, not the name of one,
so it is by which font it is that one is found. What an app made
with another font, of its own, is left as it is
```

