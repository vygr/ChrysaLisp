# Actor

```image
docs/reference/classes/Actor.cwb
```

```code
(Actor) -> actor

something on a stage that pointers can be on. It keeps what it likes
for each pointer it has, by the pointer's id.

Where it can be touched is its zones: lists of polygons in its own
space, each named for what a pointer that lands there is to do, its
body, the band along an edge, a bar to drag it by, (:zones). Their
union is its outline, all of where it is, made once and kept, and
made again when its zones change, (:zones_changed), or what they
depend on, (:zones_key). A point is its if it is in that outline,
(:hit), and then which zone it is in says what it is to do, (:zone_at)
```

### :claim

```code
(. actor :claim) -> (key zones outline box)

its zones as paths, their union, and the box round that
```

### :dismiss

```code
(. actor :dismiss event) -> :t | :nil

a pointer has gone down on the surface. One that is only there
till then, a palette, goes. Did it
```

### :draw

```code
(. actor :draw canvas [m]) -> actor

on a canvas, over the document, by the matrix the board is seen
by, one that has anything to draw
```

### :drawing?

```code
(. actor :drawing?) -> :t | :nil

is any pointer that is down on it drawing something, a line that
is not yet done. One that has anything to draw with says
```

### :held

```code
(. actor :held) -> num

how many pointers it has something kept for, those that are down on it
```

### :hit

```code
(. actor :hit x y [event]) -> :t | :nil

is a point on it, the point is in the space of the stage, and is
the contact its own, by its own rules, with the event of the
contact as it lands: the stage asks, it tests nothing itself. By
its outline, a box round it first
```

### :leave

```code
(. actor :leave stage id) -> actor

a pointer that was over it, not down, is no longer
```

### :pointers

```code
(. actor :pointers stage events) -> actor

the events of the pointers it has, all of this batch
```

### :set_state

```code
(. actor :set_state id state) -> actor

what is kept for a pointer, :nil for nothing more
```

### :state

```code
(. actor :state id) -> :nil | state
```

### :tick

```code
(. actor :tick time) -> :t | :nil

the time is now that, microseconds, for one that moves by itself.
Is it still moving, and so to be drawn again
```

### :to_own

```code
(. actor :to_own x y) -> (x y)

a point of the stage in its own space
```

### :zone_at

```code
(. actor :zone_at lx ly) -> :nil | name

the first of its zones a point of its own space is in
```

### :zones

```code
(. actor :zones) -> ((name polygons) ...)

its zones in its own space, the first that has a point says what
the point is on. A polygon a path, or a list of x y
```

### :zones_changed

```code
(. actor :zones_changed) -> actor

its zones are not what they were, the outline is made again
```

### :zones_key

```code
(. actor :zones_key) -> key

what its zones depend on but its own make, how big it is seen say.
The outline is made again when this is not what it was
```

