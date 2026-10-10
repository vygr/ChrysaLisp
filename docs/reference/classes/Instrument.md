# Instrument

## Actor

```code
(Instrument board x y) -> instrument

what every instrument is. One of a kind sets :length, how big it is,
and its limits, and has a :rebuild that sets what it is made of, at
that size. :size is how much bigger all of it is seen, 1.0, and is
not what a hand changes:
	:outline	a d, closed
	:edges		((kind ...) ...), (:line x0 y0 x1 y1) or (:arc cx cy r a0 a1)
	:parts		((what d) ...), what is :move :turn :size :close, or
				:mode, which when tapped has it draw the next of an
				arc, a slice and a circle,
				the first that a point is in is the one, so put the
				small ones first. A :turn can have the point it turns
				about after its d, x y, default the middle. A part
				that ends with :quiet is not drawn darker, it is a
				part of the thing that is plain to see already
	:marks		shapes, drawn over the body
	:glyphs		((what x y [angle]) ...), where the sign of a part is
				drawn, so that it is seen what the part is for. A
				part other than :move is drawn too, a shade darker
	:readouts	((x y [turn]) ...), where the angle it is turned to
				is written, in degrees, the way a protractor counts,
				up from level. One with a turn is written turned by
				that, and says the angle that much on: a ruler seen
				from its other side
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

### :degrees

```code
(. instrument :degrees [turn]) -> str

the angle it is turned to, in degrees, to a tenth, counted the way
a protractor's numbers go, up from level, 0 to 360. With a turn,
that much on
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
a slice of that, or :circle all the way round. :fpie and :fcircle
are those filled, which is the shape's doing, (:begin)
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

### :extent

```code
(. instrument :extent) -> num

how big it is, the number that is made more or less when it is
sized: :length
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

### :readout

```code
(. instrument :readout) -> shapes

the angle, written at each of its :readouts, made again only when
what it says is not what it said
```

### :rebuild

```code
(. instrument :rebuild) -> instrument
```

### :restack

```code
(. instrument :restack stage front) -> instrument

to the front of the instruments on a stage, or to the back of
them. They stay in front of the surface and the handles, the two
at the back, and behind what else is there, a palette
```

### :set_extent

```code
(. instrument :set_extent extent) -> instrument

it is that big, or as near as it may be, and is made again at
that size. It is not seen bigger: the marks of a straight side are
as far apart as they were, and there are more of them, as a
longer ruler has. What goes round, the degrees of a protractor,
is the one thing that is spread out
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

two pointers on its parts, not its edges, whichever parts they are,
move it, turn it and size it together, by where they were when the
second went down and where they are.

The first two that came are the two. A third that comes down on it
while they have it does nothing, they have it. When one of the two
lifts, those that are left carry on from where things are, the
next two of them together
```

