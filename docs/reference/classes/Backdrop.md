# Backdrop

```image
docs/reference/classes/Backdrop.cwb
```

## View

```code
(Backdrop) -> backdrop

a view of one colour, :paper, with lines on it by a :style, :grid
:lines :axis or none, :spacing apart, in :ink.

That much is all most want. More, each only if it is given:

	:origin_x :origin_y	where a line of the grid, or the cross of
			the axis, is, in pixels of the view. With none a grid
			and an axis are about the middle, lines from the top.
			What shows something moved about says here by how far
	:rules	((spacing color) ...), more rulings than the one, each
			drawn over the last: a coarse one and finer between
			it, each as bright as its colour says. A spacing can
			have parts of a pixel, and a ruling too fine to see
			is left out. In place of :spacing and :ink
	:turned	:t and the lines of :lines go up and down
```

### :draw

```code
(. backdrop :draw) -> backdrop
```

