# Swatches

```image
docs/reference/classes/Swatches.cwb
```

## Actor

```code
(Swatches board x y) -> swatches

about a point of the document
```

### :cell_at

```code
(. swatches :cell_at px py) -> :nil | :bar | cell
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
that went down on the bar, or between the cells, and is dragged
moves the menu
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
would tap it: (:where :ink 0xff000000), (:where :alpha 0x80),
(:where :none), (:where :close)
```

