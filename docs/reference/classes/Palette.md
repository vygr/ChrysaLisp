# Palette

## Actor

```code
(Palette board x y [kind id]) -> palette

about a point of the document, for the pointer of that kind and id,
default the mouse. It is not on the stage till it is put there,
(palette-open) does both. It starts to open at the time the board
was last told
```

### :build

```code
(. palette :build) -> palette

the wedges, each with what is drawn for it, made once
```

### :close

```code
(. palette :close) -> palette

it shuts, and is off the stage when it has, at a (:tick)
```

### :dismiss

```code
(. palette :dismiss event) -> :t | :nil

a pointer has gone down on the board, off this. It shuts if the
pointer is the one it belongs to: of its kind, and for a pen the
same pen. Did it
```

### :draw

```code
(. palette :draw canvas [m]) -> palette

on a canvas, by the matrix the board is seen by. Each ring is as
far open as it is, and turns as it opens
```

### :find_wedge

```code
(. palette :find_wedge what val) -> :nil | wedge
```

### :get_wedges

```code
(. palette :get_wedges) -> wedges
```

### :hit

```code
(. palette :hit x y) -> :t | :nil

all within its outer ring is on it, till it shuts
```

### :hot

```code
(. palette :hot wedge) -> palette

the wedge a pointer is on is drawn so
```

### :leave

### :pick

```code
(. palette :pick what [val]) -> palette

do what a tap on a wedge does, see (:where)
```

### :pointers

```code
(. palette :pointers stage events) -> palette

a pointer that comes up on a wedge picks it, wherever on the
palette it went down
```

### :ring_scale

```code
(. palette :ring_scale k) -> num

how far open a ring is, 0 for not at all, 1 for open, by the time
```

### :set_value

```code
(. palette :set_value key val) -> palette
```

### :tick

```code
(. palette :tick time) -> :t | :nil

the time is now that, microseconds. Is it still moving
```

### :to_own

```code
(. palette :to_own x y) -> (x y)

a point of the document, about the middle of the palette at the
size it is on the screen
```

### :value

```code
(. palette :value key) -> val

:mode, :color or :width, as the pointer it belongs to has it, a
pen that is bound to its own, or the board's
```

### :wedge_at

```code
(. palette :wedge_at px py) -> :nil | :hub | wedge

what a point of its own is on
```

### :where

```code
(. palette :where what [val]) -> :nil | (x y)

the point of the document that the middle of a wedge is at, for
what would tap it: (:where :tool :rect), (:where :color 0xffe03131),
(:where :width 8.0), (:where :action :undo), and (:where :hub)
```

