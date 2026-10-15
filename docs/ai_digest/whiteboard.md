# The Whiteboard

A board to draw on, where what is drawn is shapes. This is how it is made, and
how it is driven, by a hand, a script or an AI. The page for someone who only
wants to use it is [`whiteboard.md`](../apps/whiteboard.md).

It was made again from the ground in October 2026. Chris Hinsley, who wrote
the whiteboards that schools have on the wall, asked for it and said what it
was to be: shapes that are edited as shapes, not polygons; many pointers at
once, pens, fingers and a mouse, each doing its own thing; instruments, a
ruler, a protractor, that a pen is run along and that leave true lines in the
document; a file that is text and loads as a picture; and all of it able to
be driven without a hand. The way the instruments work, edges that hold a pen
and regions that do things, is his from those boards. Nothing here is that
code.

## The parts

```vdu
lib/cwb/doc.inc         a document: layers, shapes, groups, the file
lib/cwb/pointer.inc     pointers, a stage of actors, bindings
lib/cwb/board.inc       a board: a document that is worked on
lib/cwb/tools.inc       the instruments
lib/cwb/palette.inc     a palette that opens on the board
lib/cwb/paper.inc       what is behind a document while it is worked on
lib/cwb/stripes.inc     a board drawn in stripes by the nodes
lib/image/cwb.inc       a .cwb as a picture
cmd/cwb.lisp            the command
apps/media/whiteboard/  the app
```

Each stands on the ones above it. Only the app needs a screen.

```image
docs/diagrams/wb_parts.cwb
```

## A document

*To change: the document is to have no layers, see [The board to come](#layers).*

A document is a size, a background, and layers. A layer is a name, flags, 1
for hidden and 2 for locked, and a list of items, the last on top. An item is
a shape or a group of items.

A shape is the `d` of an SVG `<path>`, as text, with a fill, a stroke and a
matrix:

```vdu
(cwb-shape "M 10 10 L 100 10 A 30 30 0 0 1 100 70 Z"
	:fill 0xffffd070 :stroke 0xff000000 :width 3 :join :round)
```

`(path-gen-paths)` in `gui/path/lisp.inc` reads the path: `M L H V C S Q T A Z`
and their lower case. The `A`, an arc of an ellipse from one point to another,
did not work before this, and needs the angle of a vector, which nothing gave.
`(path-angle x y)` and `(path-gen-earc ...)` are there now, in Lisp: a first
guess at the angle good to a part in 300, and two steps toward it with `sin`
and `cos`.

The matrix is six numbers, `a b tx c d ty`, and puts the shape where it is in
what holds it. To move, turn or size a shape is to change its matrix. The path
is as it was drawn, so a box turned 30 degrees is still a box, and can be
turned back. A group has a matrix too, and what is in it is in its space.

Words are a shape of kind `:text`, `(cwb-text "Start" x y)`, whose outline
comes from a font. `(cwb-text-mid)` puts their middle at a point.

Every item has an id, a number of its own in the document, kept in the file.
`(cwb-find doc id)`. It is how a script says which.

A shape is flattened to polygons the first time it is drawn, `(cwb-flat)`:
those of its fill, those of its stroke, and the box they are in. They are
kept with it till what it is drawn with changes, and are not saved. Its
matrix is not part of that, a shape that is moved is not flattened again.

## The file

`(cwb-save doc stream)` is `(tree-save)`, the `.tre` format a `.cwb` always
was, version 4. An item is its type and then what it has that is not what it
would have anyway, a key and a value:

```vdu
((:Emap 1)
	:version 4 :width 640 :height 420 :background 0 :style :grid :grid 32
	:next_id 3
	:layers
	((:list 1)
		((:list 3) "[Layer 1]" 0
			((:list 2)
				((:list 7) :shape :id 1 :d "[M 40 360 L 220 360]" :width 4.0)
				((:list 11) :shape :id 2 :kind :text :text "[Start]"
					:fill 4278190080 :stroke 0)))))
	:props :nil)
```

It is laid out a value to a line when the system writes it. It can be written
by hand, or by anything: a file with no ids, and only what matters said, loads,
and its items are given ids. A file of version 2 or 3, polygons, is made into
one of this as it loads.

`lib/image/cwb.inc` is the `.cwb` loader of a canvas. As a picture a document
is its shapes, at the size it says it is, on its background, which is nothing
unless it was given one.

## Pointers and the stage

*To change: universal input, buttons that are only buttons, see [The board to come](#universal-input).*

A pointer is anything that points at the board. An event of one is a list:

```vdu
(ptr-event id kind buttons x y [pressure time])
```

`kind` is `:mouse`, `:pen`, `:eraser` or `:touch`. `buttons` is those held, 1
left, 2 middle, 4 right, and 0 for a pointer that is only over the board or
has just come up. A finger has the left. There can be many pointers at once,
each with an id of its own.

A `Stage` has actors, from the back to the front. A batch of events, no more
than one for a pointer, is given to it, `(. stage :pointers events)`:

* A pointer belongs to the actor it went down on, till it comes up.

* One that is not down belongs to the frontmost actor it is over.

* Each actor is then given the events of its own pointers, all at once.

So two fingers on a thing arrive together and can be seen as two fingers, and
a pen on a ruler and a finger on a shape are two actors' business at the same
moment. An actor has `:hit`, is a point on me, `:pointers`, and `:leave`.

```image
docs/diagrams/wb_pointers.cwb
```

What a pointer does is looked up. `Bindings` are rules, each a kind, an id
and buttons to match, any of them `:nil` for any, and what a pointer that
matches is to be:

```vdu
(. bindings :bind :pen 9 :nil '(:tool :pen :mode :rect :color 0xffff0000 :width 8.0))
(. bindings :bind :mouse :nil +pev_right '(:tool :hand))
```

The first rule that matches is the one. `(bindings-default)` is how a board
starts: a pen draws, and rubs out with its other end or with the button on
its side held; the mouse draws with its left button and moves things with
its right; a finger moves things. A pointer that is a `:pen` draws what
it is bound to, `:mode`, or what the board is set to.

## The board

*To change: no `:mode` of the board, select mode on things, the canvas an object, see [The board to come](#select-mode).*

A `Board` is a document, a stage, bindings, what is selected, and steps to
undo. It has two actors of its own.

The `Surface` is at the back, and every point is on it. A pointer that goes
down on it is the tool its binding says. `:pen` is whatever the board is set
to draw, `:mode`: `:pen :line :arrow :arrow2 :rect :ellipse :frect :fellipse
:text :eraser`. With `:mode :select` the pen is the hand, so one mouse button
can do everything. The hand picks up what it is on, or anything in the box
round what is selected, and moves it; on nothing, it drags a box round things.
Two hands on what is selected move it, turn it and size it, by where the two
were when the second took hold and where they are; when one lets go the other
carries on.

The `Handles` are in front of it, in select mode, and a point is on them
only where one is. They are how a shape is edited as the shape it is:

* One thing selected has eight squares and a ring in its own frame, the box
of what it is with its own matrix, `(. handles :frame)`. A box that has been
turned has them at its own corners and is sized along its own sides: the
pointer is taken into the frame, the size is changed there, and it is put
back, so it stays a box and is never sheared. A group is one thing.

* Several things have them on the box round them all.

* A line, an arrow, has neither. It has a point at each end,
`(. handles :ends)`, and an end is dragged to where it is to be: its path is
written again, `M x y L x y`, in its own space. With angles that snap the
line is held to the nearest 15 degrees from its other end.

The eraser rubs out the part of a line it goes over. `(. board :rub x y)`
takes what is within its reach out of every line that is near, and what is
left of each is lines in its place, new items, cut where the circle of the
eraser met them, `(cwb-rub)` in `lib/cwb/doc.inc`. A line is a shape that is
drawn, not filled, and has ends: one drawn by hand, a straight one, an
arrow, an arc from a protractor. An end that was its own keeps its arrow. A
box, a filled thing, words and a group are each all there or not, and go
whole, and only when no line was touched. `:rub_mode :whole` on a board has
a line go whole too, as it did.

### An eraser that bites

The eraser Chris's boards had: what the eraser covers is taken out of
whatever it touches, a box, a filled thing, words, a thing in a group, and
no more than it covers, a line too. `:rub_mode` says which way a board
rubs out:

* `:bite`, what a board has. Chris: "erase should take bites out of lines
  as well ! No cutting of things, a line is just flatterned to a polygon
  like all else if erase takes a bite".

* `:part`. A line is cut where the eraser went, and what is left is lines
  still, with a width and ends. Anything else has a bite out.

* `:lines`. Only a line is cut. If none was touched, the thing on top
  goes whole. It is what the eraser was before there was a bite.

* `:whole`. A line goes whole as well.

```image
docs/diagrams/wb_bite.cwb
```

That picture is made by the board, as it is each time the docs are made.

* `(. board :bite points [r shape]) -> :t | :nil`. `points` is the way the
  eraser went, `x y x y ...`. What it covers on that way is a polygon,
  `(poly-sweep)`, and that is taken out of each thing it touches,
  `(poly-difference)`, [`polygon_sets.md`](polygon_sets.md). The eraser is a circle of
  `r`, or `shape`, a path about `0 0` with no dent in it: a cloth, the side
  of a hand.

* `(. board :rub)` and `(. board :rub_along)` do it, so the eraser tool,
  the eraser end of a pen and `cwb -p` all bite. `(:rub_along)` takes the
  bite of all its way at once.

* A thing that loses a bite is flat from then on. It is one shape for what
  is left of its fill and one for what is left of its line, each a filled
  outline, `:rule :evenodd`, with the points of the outline for its path.
  It is still a shape: it is selected, moved, turned, sized and coloured as
  any is. It is not a box, a line or words any more, a line has no width
  to change and words can not be typed again.

* A thing that is not touched is not changed. It is found in three steps,
  the cheap ones first: its box against the box of what the eraser
  covers; its hull against the eraser's, `(poly-convex-hit?)`, GJK; and
  only then all of its polygons, the set logic. Chris: "this is why
  correct bounding box then polygon to polygon colision hearchy is
  important". On a page of 300 strokes written by hand and 40 slanted
  lines, the boxes said touched 330 times for 60 moves of the eraser and
  51 were. The hulls said 52, at a fourteenth of what the set logic takes
  to say no. A shape's hull is worked out when first asked for and kept
  with its polygons, `(board-hull)`.

* It is a step like any other. The eraser makes one as it goes down, and
  undo brings back the things as they were, not flat.

* A thing in a group is bitten where it lies, turned and sized as the
  group has it. A group that has nothing left in it goes.

* Things on a layer that is hidden or locked are left.

What it costs is the set logic, a millisecond or two for a thing and each
move of the eraser, and by how many points the thing is drawn with. A
line drawn by hand is drawn with ten for each of its own, the round of
every join: a squiggle of 2,000 points is 22,000, and a bite of it took
160ms, each move of the eraser. Chris: "large squggly strokes, halt the
cursor as soon as the erase shape touches the stroke, and then chugs
across it." What was done for that, 160ms to 45:

* The eraser's way is kept as it goes and one bite of all of it is taken
  at the next tick, or as it lifts, `(. surface :rub_flush)`. However
  long a bite takes, the eraser is where the hand is, and ten moves of it
  are one bite, not ten.

* The points of a thing that are within a quarter of a pixel of the line
  between their neighbours are left out before it is bitten,
  `(path-filter)`. Half of them, of a smooth line.

* What is left of a thing keeps the hull of what it was, which has it in
  it, and the polygons that were just worked out, in place of reading its
  path back from its text.

* Of a thing a bite has left, is it touched is the one sum: what is left
  is less than there was, or not. And only its rings that are near what
  is swept are worked on.

Not done, and what would do most: the set logic's own loop, a band at a
time, is Lisp, with the three native functions called from it. A bite of
a long line is 7,000 bands. And the bite is taken by the board's own task.

The app has no button for the mode and keeps none, it is the board's.

How far the eraser reaches is the width its pointer draws with, three
times it, `(. board :reach [width])`: 9, 18 and 36 pixels for the app's
three widths, and that on the screen whatever the zoom. Chris: "The
eraser mode, should use the current pen size ?" Each pen has its own
width, so each has its own eraser.

Not done of what he told: the shape of what touches the board is not
known, the eraser is a circle; and the work is done by the board's own
task as the eraser moves, not given out to others.

A thing that is moved with snap on has the top left of the box round it go
to the grid, not the pointer, so that what is moved lines up with the grid
and with what else is on it.

Everything a hand can do is a method, for a script: `:select`, `:select_all`,
`:transform`, `:style`, `:group`, `:ungroup`, `:order`, `:align`, `:duplicate`,
`:delete`, `:clear`, `:undo`, `:redo`, and `:add`.

Every point is in the space of the document. A size that is of the screen,
how near is near enough, how big a handle is, is divided by `:zoom`.

What is selected is in flight, `:floating`: it is not drawn with the
document, `(. board :draw)`, it is drawn by itself, `(. board :draw_flight)`,
and so is a line as it is drawn. The document is one picture that is not
drawn again while a thing is moved, turned or sized. Its flight is in front
of the document or behind it, `:float_back`: a thing taken hold of comes to
the front of its layer, one taken by the right button goes to the back, and
it stays there while its handles are used. It lands when it is let go of.
The app has a canvas for each: the paper, the document, what is in flight,
which it puts in front of the document's or behind, and the handles. What
is put on top of the top layer is `:appended`, and can be drawn onto what
is already there.

Each instrument has a canvas of its own, as big as the instrument is, and
what puts the screen together lays it over the rest. It is Chris's, from
his old boards: "that's what I did before ;) And had the instruments
bounds be the size of the pixmap/canvas, the instrument has to
co-ordinate the offset of that cache canvas/pixmap/texture so that
rotations don't look back", and "Let the compositor deal with it." An
instrument is drawn when it is turned, sized, or says another angle. Moved,
or with anything else on the board changing, its canvas is only put where
it now is, at whole pixels, and nothing is drawn. Where on the canvas the
instrument's own `0 0` falls is kept, so turned about that it stays on the
one pixel. While it moves it can be half a pixel from true, and when it
comes to rest it is drawn once more just where it is, to an eighth of a
pixel. `(draw-tools)` and `(tool-show)` in `app.lisp`. A palette, and the
rings where fingers are, are on one more canvas in front of them all.

## The view: a pan and a zoom

The things of a document are anywhere. The paper is not what they are
on, in the sense that a view is what its children are on: it is a
background, a colour and lines, of a size, and the document is seen
through it from some point at some zoom. Chris: "the shapes live in an
infite 2d cord space, the canvas is still a finiite size view", and "The
canvas/paper is just a background with a paticular colour and a drawing
grid or line, with the snap interaction. But it's not the parent of the
shapes in a scene graph sence."

So the app has `*pan_x* *pan_y*`, the point of the document at the top
left of the paper, and `*zoom*`, and one matrix of them, `(view-matrix)`,
that everything is drawn through. No shape is changed by a move of the
view. A point of the view is taken to the document the other way,
`(. board_view :to_doc)`. The canvases are the size of the paper whatever
the zoom, so a zoom makes none, it is only all drawn again.

What moves the view is in `view.inc`, and calls the app: the middle
button and the wheel, `:on_pan` by so many pixels, `(pan-by)`; two
fingers on nothing, `:on_pan` and `:on_zoom`, `(zoom-pinch)`, and when
they let go `:on_rest`, `(zoom-rest)`. The board has each finger first, as
it has every pointer, and is asked which are on nothing,
`(. board :pointer_free? id)`; the first two take the view, and the board
is told to give up what each was doing, `(. board :pointer_idle id)`.

### Nobody's hand is moved under

Chris: "you have to imaginbe that there are mutiple monkeys all stood at
the board and doing drawing and using instruments... and none of it has
to conflict." What each does is their own, their pen, their instruments,
what they have hold of, and nothing one does stops another: "no,
definatelty no lock as people move shapes around ! or draw, or selecting
! none of those actions locks out the other users."

What is seen is not anybody's own, there is one pan and one zoom for the
board. Two things keep a move of it from being done to somebody:

* While anybody is drawing a line it is not moved at all,
  `(. board :busy?)`, which asks each actor `(:drawing?)`: "lock the view
  while anyone is drawing, that onlt applie to the global actions !" The
  wheel, the middle button, the bars and the zoom buttons do nothing
  then. Two fingers that have hold of the view do nothing till the line
  is done, and then go on from where they are. A zoom that is on its way
  when somebody begins a line goes all the way at once, and a glide
  stops.

* A hand that holds a thing does not stop it, and what it holds stays in
  the hand: "If I hold a shape, and somone else moves the canvas, my
  shape/group comes with me ! It is 'in flight'". When what is seen has
  moved, the app's view tells the board every pointer that is down again,
  where it is in the view, which is now another place of the document,
  `(. board_view :resend)`. So a thing held is moved to the hand, and a
  box being dragged out still ends at it.

First come first served, for a thing as for the view. Chris: "and first
come first served for who is manipulating what shapes/groups !" What one
has hold of is theirs till they let it go: another's hand that goes down
on it does nothing, it does not take it, share it, or drag a box out from
it. "Another" is another device, a second pen, a mouse, a person far away.

Not the eraser. Chris: "I had the shape/grouop you are holding can be
erased by yourself ! That was a nice feeling UI, you could pick up and
hold somthing, turn it and shape it with the eraser... that REALLY
impressed people !" "You pick up an Apple and take a bite out of it with
the large araser, and the pupils all laugh so much !" A thing that is
held is bitten as any is, and what is left of it is in the hand still
and selected still, `(. surface :swapped)`: the hand goes on moving it,
and two hands go on turning it, from where it is, with no jump. If
nothing is left the hand has nothing. Whose eraser it is can not be
known, a finger that holds and a pen that rubs out are two devices and
may be one person, so anybody's eraser does. A second
finger of the device that has it is the same one's other hand, and the
two of them move, turn and size it between them as they did. An id is a
device and which of its contacts, `lib/cwb/pointer.inc`.

### All of the screen

Chris: "Why don't see say that max mean fullscreen mode ? the canvas is
made to be the only thing visibale on the screen ! BUT we need a way on
the ring menu to go restore back to the Window with furtiture look."

`(full-screen)` and `(full-window)`, `ui.inc`. The window is not taken
apart. Chris: "Why are we removeing the Window widget at all ? It can be
just places outside the screen edge and larger than the screen ? NO need
to break the widget heiarchy at all." And: "we are allow to place the
window wherever we want ! The gui calls are just info, not restrictions."
So the frame the board lies in is asked to be the size of the screen, the
window is made the size that gives it, and is put where that frame is at
the corner of the screen: its title, its toolbars and its edge are off
the edges. The frame has no bars, `:bare`, and the paper is made the size
of the screen: it is a view, it is no change to what is drawn.

The board is given `:on_window`, and a palette that finds a board with
that has one more thing to do, `:window`, `lib/cwb/palette.inc`. It asks,
and the app does it on its next tick, not in the middle of what the
palette is doing. The escape key does the same. Closed while it is so,
it is made a window first, and is kept as one.

### The bars

The paper and the desk round it lie in a `Board-frame`, `view.inc`, in the
middle of it, with a bar along the bottom and one up the side. The bars
do not move the paper, which is what a scroll's would do and is not what
anything else here does. Chris: "they just don't marry up with what you
do in the app." They move what is seen, as the wheel does, and are there
for a mouse that has no wheel, and to say where one is.

`(bars-update)`. All of a bar is all there is to see that way: from the
first of what is drawn or of the view, whichever is first, to the last of
either. Its thumb is the view. So a view moved out past the drawing is
part of what there is, and no bar has an end to come to. A bar moved by
hand, `(bars-moved)`, puts what is seen where its thumb says, and while
that goes on the bars are not set again: a thumb is not to change size
under the hand that has it. They are set when the hand has gone, a tick
after, `(bars-tick)`.

### The zooms it rests at, and the paper's lines

```vdu
zoom:       1     1.5     2     3      4
coarse:     32    48      32    48     32     pixels apart, full
fine:       16    24      16    24     16     half ... three quarters ... half
finest:     8     12      8     12     8      none ... a quarter ... none
```

`(draw-paper)`. The paper is a `Backdrop`, `gui/backdrop/lisp.inc`, with
an origin, where the document's `0 0` is seen, and three rulings, each
half the gap of the last. From one zoom it rests at to the next the gap
of each goes to twice, the coarse one stays full, the next goes from half
to full and the finest from nothing to half: which is where it began,
with each ruling in the place of the one before. So it never ends.

It is only right at rest. Chris: "you do need zoom magnets for that
effect to work well", "you don't want to lave the board with all lines
75% opaque !" The zooms it rests at are `+zooms`, each twice the last.
Fingers that pinch feel one when they are within a sixteenth of it. Let
go, the zoom goes to the nearest, a third of the way each tick,
`(zoom-go)` and `(zoom-tick)`: "ease to the magnet on release. it's
better than a jump." The zoom buttons go to the next the same way.

The snap is to every line that shows at the zoom it rests at, the fine
ones, at half, too, `(snap-gap)`. Chris: "Snap are gird lines, should be
to the 5mm !"

### Turned

`*turn*`, quarters, 0 to 3. Chris: "Full canvas rotation is a rae thing
to do and that mainly for people wanting to change from horizontal to
vertical", "set the magnets for angles on the canvas as 90 degrees, full
locks", "the action and the final jump of the canvas is enough in this
use case, it more of a gesture than a micro control action."

It is a third part of the one matrix, `(view-matrix)`: what is at the pan
at the top left, as big as the zoom, and then turned about the middle of
the paper, `(view-turned)`, by numbers that are exactly a quarter,
`(turn-matrix)`. The paper is the document's size the other way up when
it is one or three, `(canvas-size)`. A point of the view is taken to the
document by the matrix the other way, `:inv` of the app's view.

`(view-turn by)`. Two fingers on nothing, the one turned about the other
past half way to a quarter, ask for it, `:on_turn`, once, and not again
till they are half way to the next. Control and R asks too. It is not
done while anybody is drawing. What lies on the glass is held by where it
is from the middle of the paper, which is what stays put in the window,
and by its angle there: an instrument is as it was on the screen, and an
open palette is the right way up, `:view_turn` of the board. The paper's
lines are the same lines, those of `:lines` go the other way, `:turned`.

A move of what is seen is so many pixels of the paper as it is seen, and
is turned back to move the pan, `(pan-by)`. The bars are worked out in
pixels of the paper as it is seen, what is drawn put through the matrix.

### Kept with the document

`:view` of a document, `(x y zoom turn)`, is how it was last seen. The app
writes it as it saves and reads it as it loads. It is not a step that is
undone, and nothing of `lib/cwb` reads it: a `.cwb` shown as a picture is
all of what was drawn, from wherever it was looked at.

### Not here

* The nodes that draw a big board in stripes do not know of a pan or a
  zoom. They draw one that is seen from `0 0` at 1, and the app's own
  task draws it otherwise.

* A move of the view draws all of it again. Nothing is slid.

Saving makes the paper the size of what is drawn, with a little room
round it, and brings it to the top left, as it did. Chris: "yes,
definately, with the small broder area. So that diagrams get minsize and
a easy edge."

### Instruments lie on the glass

An instrument is not of the document. What is seen goes by under it and
is zoomed under it, and it stays where it is on the screen, the size it
is there. Chris: "Instrument move with the whell, that's definately not
wanted." So when the pan or the zoom is another, each instrument's place
in the document and its `:size` are made what keeps it so, `(tools-held)`
and `(tools-hold)`, and a new one is made the size it is on the screen at
the zoom there is. Its marks are then of the screen, not the document's.

And of the paper. Chris: "We should make the Ruler's and other markings on
the instruments match up with the background marks, it could be 2x the
background grid, but the lines should match up." The zoom rests only at
its standard levels, "it's only an effect" between them, and there the
lines of the paper are the document's grid apart on the screen, with a
fainter one at each half. So a millimetre of an instrument's marks is a
tenth of the grid, `(. instrument :mm)`, `+tool_grid_cm` squares of the
paper to a centimetre, 1: a centimetre on each line and 5 millimetres on
each fainter one, which is what the snap goes to. The inches of a ruler
are inches by that and lie on no line.

Each instrument has a `:zero`, the 0 of its marks: a ruler's centimetre 0,
the middle of a protractor's straight side, the square corner of a set
square. With the board's snap on, an instrument that is moved has its
zero put on a line each way, `(:snap_zero)`: as it is moved, as it is made,
from the palette or the toolbar, as a ruler is made longer by its 0 end,
which makes it as long as has the 0 on a line, and, since an instrument
stays where it is on the screen while the paper goes by under it, when a
pan or a zoom has come to rest and when the snap is put on,
`(tools-snap)` in the app. A set square is turned about
its square corner, and made bigger or smaller from it, Chris: "The snap
corner and the rotation cneter of the SetSquers should be the 0 scale
corner."

An instrument that no hand has hold of and none of which shows is put
away, `(tools-cull)`, each tick. It is how one is thrown away, and it is
why one can not be lost where it can not be got at, now that moving what
is seen does not bring it back. Chris: "Do we cull them imediatly ?" As
it is let go, not while it is held.

A turn of the wheel is 16 pixels, which is half the gap of the lines at
rest, and moved in steps of that the board looked as if it snapped. So
the wheel says how far and what is seen glides there, `(pan-glide)` and
`(pan-tick)`, a third of what is left each tick.

## The instruments

An `Instrument` lies on the board in front of what is drawn, and is not part
of the document. It is where it is, turned as it is, as long as it is, and
the rest is a table of paths in its own space, about its own middle:

```vdu
:outline	a path, closed. A point in it is on the instrument
:edges		((:line x0 y0 x1 y1) | (:arc cx cy r a0 a1) ...)
:parts		((what path [x y]) ...) what is :move :turn :size :close
		or :mode
:marks		shapes, the ticks and numbers, which are only drawn
```

A pen that goes down near an edge is held to it, and what it draws is that
edge from where it went down to where it is: a line along a line, an arc
round an arc, the way the pen went. It is a true `L` or a true `A` in the
document, not the points of the hand. An edge is as long as its marks, from
their 0 to where they end, and the pen is held to that: nothing is drawn
along an edge past its 0. A protractor has `:mode`, `:line`, `:pie`, `:fpie`,
`:circle` or `:fcircle`, the two with an f filled, of which it offers those in its `:modes`, a half
one no circle, and a part of it, `:mode`, that a tap takes to the next. The
instrument says what it does, no toolbar does.

Near is `+tool_band`, 6 pixels of the screen either side of the edge at any
zoom, so the catch of an edge is as wide across as the widest line the
Whiteboard draws. With the board's `:hover_edges`, on as a board is made, a
pen or a mouse that is near and not down is shown the catch of the one edge
that would hold it if it went down, a thin blue dotted line round it,
`(. instrument :catch_shape edge)`. Nothing is shown over the body of the
instrument, where a pointer would take hold of it, nor for an instrument
that another is in front of there, nor to a pointer bound to something
other than the pen. The board keeps where each such pointer is in
`:hovers` and what is shown in `:hover_shown`, `(. board :hovering
events)`, and draws them in `(:draw_actors)`. The Whiteboard keeps the
setting in `whiteboard.tre`, `:hover_edges`, 1 or 0, and sends the board a
mouse that moves with nothing held.

A `:size` part with a third thing, -1.0 or 1.0, is an end: dragged along
the instrument, that end goes with the pointer and the other end stays
where it is on the board. The two ends of a ruler are.

A pointer that goes down in a part does what the part is for: moves the
instrument, turns it about a point of it, makes it longer, puts it away. Only
a pointer that is alone on it does. A finger that holds a ruler while a pen
draws along it does not move it, and nor does the pen. Two pointers on its
parts move and turn it together.

`Ruler`, `Protractor`, `Circle`, the whole protractor, which is a
`Protractor`, `Setsquare` and `Setsquare45`, a `Setsquare` with another
`:ratio` of its two square sides, are each a `:rebuild` that fills in that
table from `:length`. Another instrument is another table.

## The palette

*To change: no menu from a tap on the canvas, the palette a floating menu made from the toolbar, see [The board to come](#binders-the-toolbar-and-floating-menus).*

```image
apps/media/whiteboard/data/palette.cwb
```

Chris: "Animating interactive pellets and menu's, mostly keep the surface
clear for the user to work unhindered". A hand that goes down and comes up
where there is nothing, with nothing selected, opens a `Palette` there,
`lib/cwb/palette.inc`. The hand is the right button of a mouse, a finger,
or the left button when the board is set to select. A pen has no hand of
its own: the button on its side rubs out, and a tap of that on nothing,
that rubbed nothing out, opens the palette for it. `(palette-enable board)` is what makes a board do it, the app
and the `cwb` command both do.

It is three rings about the point. The tools. The colours and the widths.
Things to do: undo, redo, a ruler, a protractor, a whole protractor, the
two set squares, snap, duplicate, delete. The wedge of what is set has a line of light at its rim,
and the spot in the middle is the colour, as wide as the width. A tap on a
tool sets it and puts the palette away. A colour or a width is set and it
stays. A tap on its middle, or anywhere off it, puts it away.

It is an actor on the stage, like a ruler, so it is in front of the
document, takes any pointer, and there can be more than one. It belongs to
the pointer that opened it. What a pen picks is that pen's: the pen is
bound, by its id, to the tool, the colour and the width, and another pen, or
the mouse, keeps what it had. So two people at a board with a pen each have
a palette each. What a mouse or a finger picks is the board's, and the
app's toolbars are made to show it.

It opens out, a ring after a ring, each turning as it comes, and shuts
quicker. It does not ask what the time is. `(. board :tick time)`,
microseconds, tells everything on the stage, and says if anything is still
moving. The app calls it from its timer. A test, or a script, steps it.

For a script:

```vdu
(defq p (palette-open board 400 300))   ;or by a tap, as a hand does
(. board :tick 1000000)                 ;a second on, it is open
(. p :pick :tool :rect)                 ;what a tap on that wedge does
(. p :where :color 0xffe03131)          ;where that wedge is, to tap it
(palettes board)                        ;those that are open
```

A new kind of thing on the board is a class on `Actor` with `:hit`,
`:pointers`, `:draw`, and `:tick` if it moves, `:dismiss` if it is to go when
a pointer goes down elsewhere.

## Pens and fingers, from the host up

*To change: a contact said by the event, not by a button; the eraser a device of its own, see [The board to come](#devices-contacts-and-buttons).*

The mouse comes as it always did. A pen or a finger is three more events of
the host, `src/host/gui_event.h`, pointer down, motion and up, with an id, a
kind, pen, eraser or touch, the buttons it has held and how hard it is
pressed. The SDL3 driver fills them from SDL's finger and pen events. A
finger has the left button while it touches. A pen has the left while its
tip touches, the right or the middle in its place if a button of its barrel
is held, and none when it is only near.

SDL makes a mouse out of a finger or a pen for programs that know no better.
That mouse is not passed on. The GUI service sends a pointer event,
`+ev_type_pointer`, to the owner of the view the pointer is on, or went down
on. `(. window :event)` gives it to a view that has a `:pointer` method. For
a view that has none, the first pointer that is down is made its mouse,
down, moves and up, so every button and slider there is works by touch with
nothing done to it.

The board's view has `:pointer`, and gives the board the pointer as it is.

### Devices and contacts

Everything that points is a device: the mouse, each pen, each touch panel. A
device has contacts, the places it touches at once. A pen has one. A panel
has one for each finger, and each finger that lands is given the next
number, counting up, and no number is given again: a number is one touch,
from when it lands to when it lifts. So a later touch has a bigger number,
and nothing said late of one that has gone can be taken for another. The
driver gives both numbers, `src/host/gui_sdl3_event.h`, and the id of a
pointer is the two together, device << 24 | contact. The mouse
is device 0. `(ptr-device id)` and `(ptr-contact id)`, `lib/cwb/pointer.inc`.

What a pointer is bound to, its tool and colour, `Bindings`, is its
device's: a rule that names a device matches every contact of it. So whose
a touch is, is its device, and each device can be its own colour and tool.
Two pens are two devices. Two mice are two where the host tells them
apart, which a Mac does not.

A pointer belongs to what it went down on till it comes up. The stage has
that for a ruler or a palette, and the surface has it for the things of the
document: each hand has hold of the thing it landed on and no other, a box
dragged out is its pointer's own, and a finger that goes down somewhere
puts away nobody's palette. Many hands on the board are none of them in
the way of the rest.

A finger on a trackpad is not a pointer, it moves the mouse. With
`CL_TOUCH_TRACKPAD` set in the environment a node is started in, it is taken
as one, the pad is the window, to try many fingers on a machine with no touch
screen. The pad then moves no mouse in the window, SDL gives one or the
other. And on a Mac the pad must be on: with a mouse there it may be set to
be ignored, System Settings, Accessibility, Pointer Control. The board
draws a ring where each finger is:

```vdu
CL_TOUCH_TRACKPAD=1 ./run.sh
```

Two more of the SDL3 host are read the same way, as a desktop is started.
`CL_FULLSCREEN` has its window take the whole screen from the start.
`CL_GPU_DEBUG` has SDL check every call made of the GPU and stop at what it
finds wrong, for work on the driver: without it nothing is checked, and a
thing SDL finds wrong is written to the terminal and passed.

## Many shapes: stripes

A document is drawn by the task that has it, till that takes a while. The
app times each whole draw, and one that took more than 40ms, some thousands
of shapes, is from then on drawn by the nodes of the machine, a stripe of the
rows each, straight onto the pixels of the app's canvas, which are in shared
memory, `(canvas-shared)`. It is `Stripes` in `lib/cwb/stripes.inc` and its
child `lib/cwb/stripes_child.lisp`, on `Jobs`, `lib/task/jobs.inc`.

A child has to have the document, and is not sent it for each frame. Each
keeps a copy, with what its shapes flatten to. The board says what changes,
`(. board :changed ids)` at every place it changes an item, `:all` where it
is not known, an undo, a new document. `(. stripes :note)` makes that a
version, a number that goes up. A stripe to draw names the version. A child
that is behind asks, and is sent the layers as ids in order and the items
that are new or not what they were, as text, `(stripes-delta)`, or the whole
of it if what happened since is not all known.

A child does not look at every item for every stripe. For the version it
has it knows which items have any part in each band of 64 rows of the
document, and a stripe is the items of the bands it lies on.

The app does not wait. `(. stripes :frame canvas zoom style)` sends the
stripes and returns, the answers come to mailboxes the app waits on with
its own, and `(. stripes :handle index msg)` says `:done` at the last. A
change made while a frame is out is drawn when it is back. After a change
that would have each child sent the whole document, `(. stripes :cheap?)`
is `:nil`, and the app draws that one itself and has the children brought in
step behind it, `(. stripes :warm)`.

The children are kept off the node of the app, each pinned to a node of its
own in turn, `Jobs` with `away`, and give the other tasks of their node a
turn as they read and flatten. A child that did not held its node for as
long as that took, and tests of other things, on a slow machine, timed out
beside it.

What it is worth, on a MacBook with ten nodes, a board of 1600 by 1200 with
shapes all over it, a whole draw in milliseconds:

```vdu
shapes     one task    the nodes
2,000         11           3
10,000        57          11
40,000       243          42
```

The first frame after a document is loaded is the other way about, each
child reads and flattens all of it, 3 seconds for the 40,000 where one task
takes one. That is why the app draws the first itself, and has the children
brought in step behind it.

The pixels are the small part of a draw. Most of it is Lisp going through
the shapes, which is why the bands matter more than the stripes, and why one
task is good for some thousands of shapes with no help.

It is tested with no screen: a board of 240 shapes drawn by the nodes is, to
the pixel, what one task draws, through changes, an undo and another zoom,
`tests/system/test_cwb_stripes.lisp`, and the app's own redraw is driven
through it in `tests/system/test_whiteboard.lisp`.

## The command

`cwb` is the board without a window. See `cwb -h`.

```vdu
cwb -n 640x420 a.cwb -s make.lisp -i -o a.tga -b 0xffffffff
```

`-e` and `-s` run Lisp with `doc` and `board`. `-p` plays a file of pointer
events, a line a moment, `id kind buttons x y` with `;` between those that
are at once. `-i` lists what is in it, each item with its id, what it is and
the box round it. `-o` draws it, a `.tga` or a `.cpm`, with what is on the
board over it: the handles of what is selected, a ruler, a palette. `-k`
saves nothing.

The app's sample, `apps/media/whiteboard/data/test.cwb`, was made that way.

## Diagrams made by a program

`lib/cwb/diagram.inc` lays a diagram out: `(dia-pill)`, words in a box;
`(dia-card)`, a title and lists under headings; `(dia-row)`, pills in rows;
`(dia-tree)`, a tree from a list of each thing and what it comes of;
`(dia-arrow)` and `(dia-elbow)`, the lines between. Each gives a group and
how big it is, words are as wide as the font says they are, so a box is the
size of what is in it. `(dia-doc parts)` makes a document of them, as big as
they are.

A diagram that is thought out and not worked out from data is a scene:
`(dia-scene)`, then `(dia-put scene 'name (dia-box ...) x y)` for each box,
`(dia-under)` for one that others are in, `(dia-join scene 'from 'to
"label")` for a line between two names, which leaves and arrives at the
sides that face, `(dia-say)` for words, and `(dia-scene-doc scene)`.

The diagrams in the pages of the docs are scripts, `docs/diagrams/*.lisp`,
a scene each, that end with `(diagram "name" doc)`. `make docs` runs every
one and saves `docs/diagrams/name.cwb` beside it, and a page shows it with
an `image` section. To change one, change its script and `make docs`. To
see one as you write it, `cwb docs/diagrams/name.cwb -k -o name.tga`.

`make docs` makes the diagrams of the reference with it, from the source,
each time: a card for every VP class, `docs/reference/vp_classes/`, with
what it comes of, its fields, its methods by kind and what comes of it; one
for every Lisp class, `docs/reference/classes/`; and the tree of each kind,
`docs/reference/vp_hierarchy.md` and `class_hierarchy.md`. They are `.cwb`
files, shown in a page by an `image` section, and can be opened on a
Whiteboard. None is drawn by hand, so none is out of date, and if the file
format changes they are made again.

## For an AI that is to draw

A picture is a script. Write Lisp that makes shapes and give it to `cwb`:

```vdu
(defun box (x y w h label fill)
	(cwb-add doc (cwb-group (list
		(cwb-shape (cwb-d-rect x y (+ x w) (+ y h) 10) :fill fill :stroke 0xff203040 :width 2.5)
		(cwb-text-mid label (+ x (/ w 2)) (+ y (/ h 2)) :font_size 26)) :name label)))
(box 40 100 150 70 "Pointers" 0xffffe08a)
(cwb-add doc (cwb-shape (cwb-d-line 194 135 244 135) :cap2 :arrow :width 3))
```

Things worth knowing:

* Numbers that are given to the functions here can be whole or not. In
arithmetic of your own they can not be mixed, `(+ 1 0.5)` is an error,
`(n2f)` makes one the other.

* A string in Lisp that is on a command line is in `{braces}`.

* To see what was made, draw it, `-o a.tga`. A `.tga` is the pixels with a
header of 18 bytes, and most things that show pictures show it.

* To check where things are without looking, `-i`.

* To try a tool as a hand would, write a pointers file and play it, `-p`. An
instrument is put on the board first, `-e "(. (. board :get_stage) :add (Ruler
board 320 330))"`.

* A line of a pointers file is a sixtieth of a second, and `wait 400` is 400
thousandths more. A palette opened by a tap is in a picture as far open as it
then is.

## The board to come

What follows was settled with Chris on the evening of 15 October 2026, from
how his boards worked, and is what the board is to become. Where it says
otherwise than the sections above, this is the one. Each of those sections
says so where it is touched. Nothing of this is built yet.

### Universal input

Chris: "I had all devices stream their inputs into the top of the app and
get distributed (via collision detection) then I had each object (canvas was
just an object like any other) act on what their input vector of inputs was.
Nothing was treated specially." He calls it universal input.

* Every device streams into the top of the board: the mouse, each pen, each
eraser, each touch panel, and the devices of other machines in a shared
session. Nothing comes in by a door of its own.

* A contact goes to the object it lands on, by collision, and is that
object's till it lifts. The objects are the instruments, the floating menus,
the shapes and groups in select mode, and the canvas, which is the object at
the back and has whatever lands on nothing.

* Each pass of the board's loop, each object is given its input vector, all
of its contacts at once, and acts on it. Both fingers of a pinch arrive in
the same step.

* The canvas is not special. A pan and a pinch of the board are what the
canvas does with two contacts of one touch device, as a ruler turns with two.

What is here now that this replaces: the `Surface` hit tests the shapes for
them rather than the shapes being objects; the view takes two fingers on
nothing for itself, `(. board_view :finger)`, outside the stream; the mouse
comes by `:mouse_down` and its kin, pens and fingers by `:pointer`; and
events are given out one at a time as they come.

It is the board's for now. Chris: "This for now is Whiteboard specific,
although we might have the entire GUI do it once we go multi touch panel."

### Devices, contacts and buttons

* Touch is a device with many contacts and no buttons. All of its contacts
are the one device, and have the one binding.

* A pen is a device with one contact and two buttons, the tip and the
barrel, held in any combination, near or touching.

* A mouse is a device with one contact and two or three buttons.

* An eraser is always in erase mode, and erases along the path its erase
shape sweeps. A tap of it is a small bite. On Chris's boards it was a
device of its own, and the teacher's: "the eraser device input was always
assumed to be under the control of the teacher. The bound to erase action
of a pen was bound to the user id of that device." SDL3 gives the other end
of a pen as an `:eraser`, with the pen's number, so here the other end of
any pen is the teacher's eraser, and a pen's barrel button bound to rub
out is that pen's user.

* Contacts of a device count up and are never given again.

* A contact is not a button. Touching or not is said by the event, down,
motion or up, not by a button being held. The host change for this, and for
an id that has room for where a device comes from, is to be done with
Chris's desktop free, with the Windows host built and a new snapshot. Today
the driver gives a finger the left button, and a pen's barrel the right in
place of the left, so tip and barrel together can not be told from the
barrel alone.

### What a contact is, decided as it lands

* Only a touch device can have its contacts on several objects at once, and
they all work. Ten fingers on five instruments, two each, is five pairs.

* On an instrument the zone a contact lands in decides what it is till it
lifts. On an edge it draws along it, any contact, a third or fourth finger
of the hand holding the ruler too. On the body it moves, turns or sizes, and
two contacts of one touch device are a pair. More than two there are
ignored, so students can not fight over it. A pen on the body of an
instrument fingers hold does nothing; on its edge it draws.

* A pair is two contacts of one touch device on one object. A pen, a mouse
or an eraser never pairs, with each other or with a finger.

* A tap is a really quick touch and lift, about a fifth of a second and two
millimetres. On a shape or a group it takes it in or out of select mode,
whatever the device is bound to. On the canvas it leaves a dot. A contact
that moves further, or stays down longer, is a draw, by its binding; a press
and hold is a draw that has not moved, a dot, never a select. The two
numbers are to be in one place, to be tuned on a panel.

### Select mode

There is no selection list and no selection object. A shape or a group is
in select mode or it is not, it shows its handles while it is, and contacts
that land on it work it as they work anything. A thing in select mode
remembers which user put it there. An action on a selection, group, delete,
duplicate, to front, pressed by a device, is on everything that device's
user has in select mode, from any of their devices, and nobody else's.

### Binders: the toolbar and floating menus

Chris: "the device ID, its buttons, and its contact points, and what those
combinations are bound to drive the app, not some state held in the singular
toolbar. The toolbar should really be a place for a device/button combo to
have its bound action changed."

* There is no current tool, colour or width of the board. The board's
`:mode`, `:color` and `:width` go.

* A press on a binder binds what pressed it: the device, and the buttons it
had held. A finger binds touch. A pen's tip binds the tip; the tip with the
barrel held binds that. A mouse's right button binds the right.

* Floating menus are binders, made from a button on the toolbar, as many as
anyone wants, dragged about, and each with a button to shut it. They are
objects among the rest. Two students at the board can each have one, and a
tap on one binds the device that tapped. Nothing opens from a tap on the
canvas. The palette, which opens now from a tap on nothing, is to become a
floating menu.

* What a device is bound to is not shown on the board. On a floating menu
each device has a small dot of its own colour on what it has chosen.

* Bindings are kept on their owner's machine, a user's own, and come back
next time, by user, kind of device and buttons, as a device's number is only
that sitting's. A student's pen starts from the student pen's.

* The finger rings the board draws now, `:show_contacts`, are clutter by
this, and are to be off but for debugging.

### Users and what they may do

* Every shape and group says which user made it, and it is kept in the file.
A user, not a device: the teacher is user 0 every time.

* The mouse and the keyboard are the teacher's, user 0, and so is the
eraser. The teacher may make
a pen the teacher's pen, from a floating menu, which only a teacher's device
can press: the next pen to touch it is the teacher's. Other pens are
students, each their own. Touch is anybody, with a student's rights.

* A student can not erase what the teacher or another student made.

* Only the teacher undoes and redoes, and it is of the whole document, the
last change whoever made it.

### Layers

The document has no layers. Chris: "What's saved and loaded to a file is the
committed layer, and that's that." It is one list of shapes and groups, back
to front. A file of today, with layers, is to be read into the one list in
the order its layers are in.

The board draws three layers. In the middle is the committed one, a picture
of the document drawn once and kept, redrawn only when a thing goes into it
or comes out. It is how fast drawing is, it holds no state. In front of it
and behind it are two layers in flight, drawn as things move: what is being
pulled to the front and what is being pushed to the back. Two layers, as
two people can be doing each at once. Now there is one, which goes behind
the committed one for everyone when anyone pushes a thing back.

### A board shared between machines

* Every Whiteboard says it is there as `*Whiteboard`, with its user's
name, as Chat does with `*Chat`, so it is seen on every machine of the
mesh.

* The teacher, or presenter, starts a session and invites. A board that
joins is given the committed document once, and the changes after it.
When the teacher goes the session ends. A board that joined then has its own
document again.

* The teacher's board is the middle of it. Each other board has a stream to
it and back, and the teacher's sends on to the rest. So there is one order
of everything, and two that reach for one thing at once are settled by
which the teacher's board had first: first come first served, between
machines as on one board. The one that lost has the thing go to where the
winner has it, and their hand does nothing till it lifts.

* What goes is changes, never the document again: a shape made, things
moved by a matrix, a bite into pieces, things deleted, an order changed.
They must all arrive, in order, and the stream does that, an `:out` waits
for the `:in` to say it has them.

* What is in flight, a line as it is drawn, a thing as it is carried, goes
as it is now, not every step of it. Each board has one of these gone to
each other board at a time, and sends the next when that one is answered,
whatever is newest then. So a fast link has many a second and a slow one
fewer, each the newest, and nothing is measured. Chris: "we don't need to
update the remotes as fast as the board the person is using".

* A student sees what they draw at once. Everyone sees everyone's lines as
they are drawn.

* Each board has its own view, its own bindings, menus and instruments. The
devices of another machine are alien here, bound on their owner's menus.
The view is not held still on one board by a line drawn on another, as each
has its own.

* Whether those who joined may save a copy of the document is the host's to
say as the session starts, and they may not unless it is said. A business
meeting may not want it. It is the app that keeps to it: a board that
joined holds the document to draw it.

### In the order it is to be built

1. Who made a thing, kept on it, and what a user may do (item 59). Built:
`:user` on an item, `(. board :user_of event)`, `(. board :appoint event)`,
an eraser that keeps to its own, `tests/system/test_cwb_users.lisp`.
2. Universal input: the canvas an object, input vectors, the pair rule in one
place, tap and draw. Built so far: a pair is two contacts of one touch
device, on an instrument, a shape and the board; an instrument's edge draws
for any contact but the eraser and its body is the first device's; a tap,
`+board_tap_time` and `+board_tap_near`, on a thing toggles its select mode
whatever the contact draws, on nothing a dot.
3. Select mode on things, and actions on a user's own. Built so far: a thing
tapped remembers who, `(. board :selected_by user)`; drawing no longer lets
go of what is selected. The actions still take all that is selected, till
the binders say who pressed them.
4. The binders: the toolbar and floating menus, bindings by device and
buttons, saved by user. Built so far: a window keeps who pressed a view,
`:pressed_by`; the ink, tool and width bars bind it, `(. bindings :set)`,
and show no pick; `(palette-float)` from the toolbar's menu button, any
number, stays till its middle is tapped, binds what taps it; no menu from
a tap on the board; undo and redo only by the teacher. Not yet: the dots
of each device on a menu, bindings saved by user.
5. Two layers in flight.
6. No layers in the document.
7. The shared board.
8. The host change, with Chris's desktop free.

## What is not done

* A pen or a finger has never been on it. The SDL3 driver tells of them, see
below, and all above it is tested with them made up, but there has been no
touch screen and no pen to try. Only the SDL3 driver does; the framebuffer
and raw drivers give a mouse.

* Stripes have not been seen on a screen, only in tests of the pixels. The
nodes draw onto the canvas while it is shown, as the Canvas demo's do.

* A whole draw is all of the canvas. Nothing draws only the part of it that
changed, or only the part that shows in the window.

* What is left of a line that was rubbed is straight pieces, its curve as
it was flattened. It looks the same and is more points than it was.

* A shape can hold `:props`, anything, kept in the file, for the Lisp that is
to act on it, what it collides with, how it moves. Nothing reads them yet.

* The paper, plain, grid, lines or axis, `lib/cwb/paper.inc`, is to work on.
It is not in a picture of the document.
