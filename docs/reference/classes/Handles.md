# Handles

## Actor

```code
(Handles board) -> handles

the box round what is selected, with a square at each corner and the
middle of each side to size it by, and a round one above it to turn
it by. It is in front of the surface, and a point is on it only where
one of those is
```

### :box

```code
(. handles :box) -> :nil | (x y x1 y1)

the box round what is selected, :nil if nothing is, or the board
is not in select mode
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

where each handle is. kind is :size or :turn, fx and fy say which
way a size handle pulls, -1 0 or 1 each way
```

