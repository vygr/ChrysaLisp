# Rostrum

```image
docs/reference/classes/Rostrum.cwb
```

## Figure

```code
(Rostrum width height [focus seed]) -> rostrum

a picture of that size, its top left at 0 0. focus is how far the camera
is from z 0, :nil for none, and things at z 0 are their own size. seed
starts its own random numbers, (:random), so that a scene made again
with the same seed and gone on by the same steps is the same scene
```

### :advance

```code
(. rostrum :advance secs) -> rostrum

secs seconds of the wall clock have gone, times :ros_speed: as many
steps of +ros_step as that comes to are taken, what is left over
is owed to the next. A scene goes on by the same steps however fast
it is drawn, so it can be run again to any step
```

### :box

```code
(. rostrum :box figure [flat]) -> :nil | (x y x1 y1)

the box in the picture round all it is drawn with, what is under it
too, as the camera has it, or with flat as it is with no focus. A
tilted figure's is by the corners of its box
```

### :collide

```code
(. rostrum :collide order) -> rostrum

who has come to touch whom, who still does, and who no longer does,
and what they fire. Asked flat, with no focus: the focus is the
camera's, how the scene is seen, and what the scene does is the
same whichever, so a run sought to is the run that was seen
```

### :copy

```code
(. rostrum :copy) -> rostrum

all of it as it is now, made anew: each figure, each of its
components, and what the scene keeps on them, that refer to each
other as they did, so the copy goes on as this would have and this
is not changed by it. Kept every so often it is a place to seek
from. A weak link a component keeps, Tie's, is to what it was
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
(. rostrum :polygons figure [flat]) -> paths

all that it is drawn with in the picture, what is under it too, as
the camera has it, or with flat as it is with no focus
```

### :random

```code
(. rostrum :random n) -> 0 to n - 1

the rostrum's own random numbers, from its seed, so a scene that
takes all of its from here is the same each time it is run
```

### :tick

```code
(. rostrum :tick dt) -> rostrum

dt seconds have gone, one step
```

