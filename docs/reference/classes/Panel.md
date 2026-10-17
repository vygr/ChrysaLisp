# Panel

```image
docs/reference/classes/Panel.cwb
```

## Actor

```code
(Panel board x y) -> panel

about a point of the document
```

### :cell_at

```code
(. panel :cell_at px py) -> :nil | :bar | cell

what a point of its own is on: a cell, the cross among them, or
the bar
```

### :cell_items

```code
(. panel :cell_items cell state) -> shapes

what is drawn of a cell, in the panel's own space
```

### :close

```code
(. panel :close) -> panel
```

### :draw

```code
(. panel :draw canvas [m]) -> panel
```

### :draw_state

```code
(. panel :draw_state) -> state

what the drawing of each cell is to know, worked out once a draw
```

### :get_cells

```code
(. panel :get_cells) -> cells

made once, about the middle, in the space of the screen, the cross
at the right end of the bar the last
```

### :hit

```code
(. panel :hit x y) -> :t | :nil

by its outline, till it is shut
```

### :hot

```code
(. panel :hot cell) -> panel
```

### :layout

```code
(. panel :layout) -> (cells w h)

the cells from 0 0, and how wide and high they are together
```

### :leave

### :pointers

```code
(. panel :pointers stage events) -> panel

a pointer that comes up on the cell it went down on taps it. One
that went down on the bar, or between the cells, and is dragged
moves the panel
```

### :tap

```code
(. panel :tap cell event) -> panel

a cell, not the cross, was tapped by event, the device as it went down
```

### :tip

```code
(. panel :tip cell) -> :nil | str

the name of a cell, shown above the panel while a pointer is on it
```

### :to_own

```code
(. panel :to_own x y) -> (x y)

a point of the document, about the middle, at the size it is on the
screen, the right way up whichever way the board is turned
```

### :where

```code
(. panel :where what [val]) -> :nil | (x y)

the point of the document that the middle of a cell is at, for what
would tap it, (:where :close) for the cross; with no value the first
of that kind
```

### :zones

```code
(. panel :zones) -> ((name polygons) ...)

its cross, the bar it is dragged by, and all of it
```

