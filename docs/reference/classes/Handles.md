# Handles

## Actor

```code
(Handles board) -> handles

what is selected is sized and turned by these, in select mode. They
are in front of the surface, and a point is on them only where one is.

One thing selected has them in its own frame: the box of what it is,
moved, turned and sized as it is. So a box that has been turned has its
handles at its own corners, and is sized along its own sides, and
stays a box. Several things have the box round them all.

A line has none of that. It has a point at each end, and each is
dragged to where that end is to be.
```

### :box

```code
(. handles :box) -> :nil | (x y x1 y1)

the box round what is selected, :nil if nothing is, or the board
is not in select mode
```

### :corners

```code
(. handles :corners) -> :nil | (x y x y x y x y)

the four corners of the frame, where they are on the board
```

### :ends

```code
(. handles :ends) -> :nil | (item (x y x1 y1))

if what is selected is one line, it, and its two ends in its own space
```

### :frame

```code
(. handles :frame) -> :nil | (matrix box)

the box the handles are on, and the matrix that puts it where it
is. Of one thing, its own box and its own matrix. Of several, the
box round them and no change
```

### :hit

### :pointers

### :spot_at

```code
(. handles :spot_at x y) -> :nil | spot
```

### :spots

```code
(. handles :spots) -> ((kind x y fx fy) ...)

where each handle is on the board. kind is :size, :turn or :end.
fx and fy say which way a size handle pulls, -1 0 or 1 each way,
in the frame. fx of an end is which, 0 or 1
```

