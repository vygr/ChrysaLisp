# Surface

## Actor

```code
(Surface board) -> surface

the actor at the back, every point is on it
```

### :begin

```code
(. surface :begin event) -> state

a pointer has gone down
```

### :end

```code
(. surface :end event state)

a pointer has come up
```

### :hit

### :holding

```code
(. surface :holding) -> :nil | id

a pointer that has hold of what is selected
```

### :pointers

```code
(. surface :pointers stage events) -> surface
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

### :things

```code
(. surface :things) -> ((item matrix) ...)

what is selected, each with where it is now, to be moved from there
```

### :two_hands

```code
(. surface :two_hands)

if two pointers have hold of what is selected, the two of them
move it, turn it and size it, by where they were when the second
took hold and where they are now. While they do, neither moves it
alone. When one lets go the other carries on from where things are
```

