# Surface

```image
docs/reference/classes/Surface.cwb
```

## Actor

```code
(Surface board) -> surface

the canvas, the actor at the back, every point is on it: what no
other actor takes is its. A stroke is drawn, a shape dragged out, words
put down, a box dragged out round things, a tap is the board's menu if
it has one, and an eraser rubs out what it goes over. It knows nothing
of what is held, (Things). A finger that goes down here puts away
nobody's palette
```

### :begin

```code
(. surface :begin event) -> state

a pointer has gone down
```

### :drawing?

```code
(. surface :drawing?) -> :t | :nil

is any pointer drawing, or rubbing out: it has a tool that is not
the hand. A hand that holds a thing or drags a box out is not
```

### :end

```code
(. surface :end event state)

a pointer has come up
```

### :hit

### :pointers

```code
(. surface :pointers stage events) -> surface
```

### :rub_flush

```code
(. surface :rub_flush) -> :t | :nil

the bite of every way an eraser has gone since the last. Was
anything rubbed out
```

### :shape_d

```code
(. surface :shape_d state x y) -> d

the path of what a pointer is drawing, now it is at a point
```

### :step

```code
(. surface :step event state) -> state

a pointer that is down has moved, or come up
```

### :tap?

```code
(. surface :tap? state x y) -> :t | :nil

did a contact lift as a tap: soon, and near where it landed. A
board that is told no time, (:tick), goes by the distance alone
```

### :tick

```code
(. surface :tick time) -> :nil
```

