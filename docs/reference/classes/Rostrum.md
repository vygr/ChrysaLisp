# Rostrum

```image
docs/reference/classes/Rostrum.cwb
```

## Figure

```code
(Rostrum width height [focus]) -> rostrum

a picture of that size, its top left at 0 0. focus is how far the camera
is from z 0, :nil for none, and things at z 0 are their own size
```

### :box

```code
(. rostrum :box figure) -> :nil | (x y x1 y1)

the box in the picture round all it is drawn with, what is under it
too. A tilted figure's is by the corners of its box
```

### :collide

```code
(. rostrum :collide order) -> rostrum

who has come to touch whom, who still does, and who no longer does,
and what they fire
```

### :draw

```code
(. rostrum :draw canvas [view]) -> rostrum

the figures on a canvas, by a 3x2 matrix from the picture to it, as
they were worked out by the last tick. By z, the furthest first, if
:ros_by_depth is set, else in the order of the tree
```

### :polygons

```code
(. rostrum :polygons figure) -> paths

all that it is drawn with in the picture, what is under it too
```

### :tick

```code
(. rostrum :tick dt) -> rostrum

dt seconds have gone
```

