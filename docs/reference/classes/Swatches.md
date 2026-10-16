# Swatches

```image
docs/reference/classes/Swatches.cwb
```

## Actor

```code
(Swatches board x y key colors on_pick) -> swatches

about a point of the document, for the custom ink of key, :color for
the pen or :fill for the brush, colors the two of them now. on_pick
is called (on_pick swatches event what val), with :key the one it is
for
```

### :cell_at

```code
(. swatches :cell_at px py) -> :nil | cell
```

### :close

```code
(. swatches :close) -> swatches
```

### :draw

```code
(. swatches :draw canvas [m]) -> swatches
```

### :get_cells

```code
(. swatches :get_cells) -> cells

made once, about the middle, in the space of the screen
```

### :hit

```code
(. swatches :hit x y) -> :t | :nil
```

### :hot

```code
(. swatches :hot cell) -> swatches
```

### :leave

### :pointers

```code
(. swatches :pointers stage events) -> swatches

a pointer that comes up on the cell it went down on taps it. One
that went down between the cells and is dragged moves the menu
```

### :set_color

```code
(. swatches :set_color key color) -> swatches

the custom pen, key :color, or brush, :fill, is this now, it is marked
```

### :to_own

```code
(. swatches :to_own x y) -> (x y)

a point of the document, about the middle, at the size it is on the
screen, the right way up whichever way the board is turned
```

### :where

```code
(. swatches :where what [val]) -> :nil | (x y)

the point of the document that the middle of a cell is at, for what
would tap it: (:where :color 0xff000000), (:where :alpha 0x80),
(:where :close)
```

