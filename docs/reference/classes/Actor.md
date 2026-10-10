# Actor

```image
docs/reference/classes/Actor.cwb
```

```code
(Actor) -> actor

something on a stage that pointers can be on. It keeps what it likes
for each pointer it has, by the pointer's id
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

### :held

```code
(. actor :held) -> num

how many pointers it has something kept for, those that are down on it
```

### :hit

```code
(. actor :hit x y) -> :t | :nil

is a point on it, the point is in the space of the stage
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

