# Instrument

## Actor

```code
(Instrument board x y) -> instrument

what every instrument is. One of a kind sets :length and its limits,
and has a :rebuild that sets what it is made of:
	:outline	a d, closed
	:edges		((kind ...) ...), (:line x0 y0 x1 y1) or (:arc cx cy r a0 a1)
	:parts		((what d) ...), what is :move :turn :size or :close,
				the first that a point is in is the one, so put the
				small ones first. A :turn can have the point it turns
				about after its d, x y, default the middle
	:marks		shapes, drawn over the body
```

### :begin

```code
(. instrument :begin stage event) -> state
```

### :built

```code
(. instrument :built) -> instrument

its length or what it is has changed, it is made again
```

### :draw

```code
(. instrument :draw canvas [m]) -> instrument

on a canvas, by the matrix the board is seen by
```

### :edge_d

```code
(. instrument :edge_d state x y) -> d

what a pen held to an edge has drawn, now it is at a point, in the
board's space. A line along a line. Along an arc, the arc from
where it went down, round the way it has gone, or with :mode :pie
a slice of that, or :circle all the way round
```

### :edge_near

```code
(. instrument :edge_near x y) -> :nil | (edge px py)

the edge a point, in its own space, is near enough to be held to,
and the point of the edge that is nearest
the nearest, and of two as near the first, so the order they are
given in says which has a corner where two meet
```

### :end

```code
(. instrument :end stage event state)
```

### :hit

```code
on it, or near enough one of its edges to draw along it
```

### :matrix

```code
(. instrument :matrix) -> matrix

from its own space to the board's
```

### :on_edge

```code
(. instrument :on_edge edge x y) -> (px py)

the point of an edge nearest a point, both in its own space
```

### :part_at

```code
(. instrument :part_at x y) -> :nil | (what shape pivot)

the part a point, in its own space, is in
```

### :pointers

### :rebuild

```code
(. instrument :rebuild) -> instrument
```

### :step

```code
(. instrument :step stage event state)
```

### :to_board

```code
(. instrument :to_board x y) -> (x y)
```

### :to_own

```code
(. instrument :to_own x y) -> (x y)
```

### :two_hands

```code
(. instrument :two_hands)

two pointers on its parts, not its edges, move and turn it together,
by where they were when the second went down and where they are
```

