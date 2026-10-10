# Set Logic Of Polygons

`lib/math/polygon.inc` takes two sets of polygons and gives what is in
either, in both, in the first and not the second, or in just one of them.
It is what a true eraser needs, a shape with a bite taken out of it, and a
shape cut to the paper, and it is there for anything else that has two
shapes and wants a third.

```image
docs/diagrams/poly_ops.cwb
```

Each of those was made by the call under it and drawn as it came back.

## To use it

```vdu
(import "lib/math/polygon.inc")

(defq a (list (path 10.0 10.0 50.0 10.0 50.0 50.0 10.0 50.0))
	b (list (path 30.0 30.0 70.0 30.0 70.0 70.0 30.0 70.0)))

(poly-union a b)
(poly-intersect a b)
(poly-difference a b)
(poly-xor a b)
(poly-op :union a b)
```

* A set is a list of paths, as `(. canvas :fpoly)` takes one, and so is the
  answer. Either set may be the empty list.

* `(poly-op op a b [mode_a mode_b])` is all four, `op` one of `:union`,
  `:intersect`, `:difference` and `:xor`. The other four call it.

* A mode is the winding rule a set is filled by, as `:fpoly` has them: `0`,
  odd even, the default, or `1`, non zero. A stroke made by
  `(path-stroke-polylines)` goes over itself where a line turns sharply or
  crosses itself, and wants `1`.

* The answer fills the same by either rule. A hole is a ring of its own
  that goes round the other way to the ring it is a hole in.

* `(poly-union a (list) 1)` is a set made flat: a shape that went over
  itself, as one or more rings that do not.

* `(poly-area paths)` is the area of a set, a ring that goes round the
  other way taken off, so of an answer it is the area that is filled.

Nothing here needs the GUI. A command, or a task on a node with no
screen, can use it.

## It is fixed point

A path is fixed point, 48 bits and 16, as every path generator makes and
`:fpoly` fills, and all of this is done in it. A fixed number is taken as
the whole number it is held as, and every sum is a sum of whole numbers,
so nothing is rounded on the way. The one place a number is made that was
not given is where two edges cross, and that is made once, to the nearest
65536th of a pixel.

So an answer is not nearly right. A point of it is a point that was given,
or a crossing. Two squares side by side come back as one box of four
corners. A shape less itself is nothing, no slivers. The areas add up:
over polygons of random points that cross themselves and each other, the
area of `a`, and of `b`, was the area in either and the area in both,
to a hundredth of a pixel in ten thousand.

Where it stops: a point more than 16384 pixels from the others, the
products no longer fit in 64 bits.

## How it works

```image
docs/diagrams/poly_bands.cwb
```

It is the scan line of a fill, with bands in the place of lines.

1. Every y a point is at cuts the plane into bands. Within a band, the
   edges are put in order across its top. If each is no further left at
   the bottom than all before it, none cross. If some do, each y at which
   two cross cuts the band again. Now in every band each edge goes from
   its top to its bottom and crosses none.

2. In a band the edges are gone along from the left with two counts, how
   many times `a` is wound round here and how many times `b`. After each
   edge the op is asked: is this in the answer. Where it begins to be and
   where it stops are an edge each, and between them is a piece of the
   answer with four corners.

3. Edges of the two sets that lie along each other are worked out apart,
   and may differ by one in the last place. Edges that close at both ends
   of the band are taken as one edge, so a shared side is a side and not a
   sliver.

4. The left of a piece is a line going down, its right a line going up.
   Along the line between two bands, where one has a piece and the other
   has none is a line across. Every line begins where another ends, by
   how they were made, so they join into rings with nothing left over.

5. Lines that follow one another along the one edge are one line, and a
   point that is on the line between its neighbours is dropped. A ring has
   a point only where it turns.

Step 1 is why it is exact: within a band no edge crosses another, so
there is no case of two edges that nearly cross, or cross at a point, to
get wrong. Steps 1 to 3 are `(poly-band)` and `(poly-cuts)`, step 4
`(poly-lines)`, step 5 `(poly-rings)`.

## An eraser

Chris's boards knew the shape of what rubbed on them, a cloth, the side of
a hand, and took exactly that out of what was drawn:

> sample the shape at each contact point of the erase ... and then along
> it's stroke, use a convex hull to join all the stroke together ... that
> gave you a full swept erase polygon and THAT polygon was subtracted from
> all shapes on the canvas

```image
docs/diagrams/poly_sweep.cwb
```

* `(poly-hull paths) -> path`, the least path with no dent in it that
  holds every point of the paths.

* `(poly-sweep shape points) -> paths`, what a shape covers as it is moved
  along a line. `shape` is a path about `0 0` with no dent, `points` the
  line. The shape is put at each point, each two that follow are joined by
  their hull, and the hulls are made one.

* `(poly-difference thing (poly-sweep shape points) rule 1)` is the thing
  with that rubbed out of it.

The Whiteboard has this as a second way to rub out, `:rub_mode :bite` on a
board, [`whiteboard.md`](whiteboard.md). Its default eraser still cuts a line
where it is crossed and takes any other thing whole. What a bite does to a
shape is as Chris told it: the shape becomes a flat polygon, the change
is a step that undo takes back, and the shapes that are really touched are
found by their boxes and then by what they draw. Giving each shape to a
task of its own is not done, a bite is a millisecond or two.

## How long it takes

On one core of an Apple M4 Max:

| | points | all Lisp | with the native code |
|---|---|---|---|
| a box, an eraser stroke across it | 4 and 52 | 1ms | under 1ms |
| an ellipse, the same | 63 and 52 | 1ms | under 1ms |
| a word of 50 points written joined up, 2 wide | 206 and 52 | 5ms | under 1ms |
| one of 200 points | 808 and 52 | 25ms | 3ms |
| one of 800 points, 20 loops over each other | 3208 and 52 | 378ms | 12ms |
| a scribble of 100 points at random, one of 25 across it | 577 and 198 | 1,536ms | 16ms |
| one of 200, and 50 | 1104 and 432 | 10,392ms | 61ms |

The time is the bands by the edges in each. A long line that goes back
over itself has many of both, and a scribble at random is the worst there
is, every part of it crosses every other.

## The native code

The work of a band is three functions in `lib/math/lisp.vp`, 2,856 bytes
of ARM64 in the boot image:

* `(poly-band edges active y0 y1 mode out)`, the pieces of the answer in a
  band: it drops the edges that have ended, works out where each of the
  rest is at the top and the bottom, puts them in order, and goes along
  them. If two cross in the band it makes nothing and says so.

* `(poly-cuts edges active ya yb cuts)`, the ys at which the edges of a
  band cross.

* `(poly-lines out y0 y1 above below segs openl openr)`, the lines of the
  pieces: down the left of each, up its right, and across where the band
  above differs.

The edges are one array of whole numbers, ten to an edge, and so is all
that passes between Lisp and these, so nothing is made and thrown away for
each band. What is left in Lisp runs once for each y a point is at, and
then joins the lines into rings.

Each has the Lisp it was written from beside it in `polygon.inc`, with
`-ref` on its name. That is not used. It is there to say what the native
code does, and a test gives both the same bands at random, 800 of them
with edges that end, cross, and lie along each other, and holds every
number that comes back from one to what comes back from the other. It
runs on every machine, so a translator that got a division or a compare
wrong on one CPU would be seen.

How it got fast, in the order it was done:

1. All Lisp, a list for each edge, a sort for each band with a Lisp
   function to hold two edges together: 542ms for the word of 800.

2. The sort given whole numbers, each an x and a place packed in one, and
   no sort at all when the edges of a band come in order: 378ms.

3. `(poly-band)` made native: 265ms. Less than hoped. What was left in
   Lisp, taking out the edges that had ended and making two lines for each
   piece of each band, was as much work as what had been moved.

4. Ended edges dropped inside `(poly-band)`, and `(poly-lines)` native:
   12ms. And a side that goes on from where it ended in the band above is
   made longer and is not a new line, so what Lisp has to join into rings
   at the end is a few hundred lines and not a hundred thousand.

5. `(poly-cuts)` native, which only a band that edges cross in needs: the
   scribble of 200 from 10 seconds to 61ms.

## Tests

`tests/system/test_polygon.lisp`: the squares above, shared sides, a
shape with itself, a hole and what fills it, the two winding rules on a
star, 40 pairs of random polygons each tried at 40 points with all four
ops and their areas summed, a line with a bite out of it, the hull and
the sweep, and the native code held to its Lisp.
