# Figure

```image
docs/reference/classes/Figure.cwb
```

## Tree-node

```code
(Figure [name]) -> figure

a node of a tree of figures, Tree-node, and how it looks and moves
```

### :add

```code
(. figure :add comp ...) -> figure

components, run from the next time this figure has its turn
```

### :flat

```code
(. figure :flat) -> (fills strokes box)

the polygons it is drawn with, in its own space, and the box they
are all in, :nil if there is nothing. Worked out once and kept
```

### :kill

```code
(. figure :kill) -> figure

it goes at the end of this tick, and all under it
```

### :local

```code
(. figure :local) -> matrix

where it is in the space of its parent. Worked out once and kept
```

### :set_outline

```code
(. figure :set_outline outline) -> figure
```

### :set_place

```code
(. figure :set_place key val ...) -> figure

any of :fig_x :fig_y :fig_z :fig_rx :fig_ry :fig_rz :fig_sx :fig_sy
:fig_px :fig_py. The matrix kept is dropped, a (def) would not be seen
```

### :set_trim

```code
(. figure :set_trim trim) -> figure
```

### :set_width

```code
(. figure :set_width width) -> figure
```

### :update

```code
(. figure :update dt) -> figure

its components have their turn, dt seconds since the last. Those
that say they are over go, then what they fire is fired, so a
figure's list is not changed while it is being gone through
```

