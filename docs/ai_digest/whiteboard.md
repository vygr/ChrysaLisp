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

## A document

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

What a pointer does is looked up. `Bindings` are rules, each a kind, an id
and buttons to match, any of them `:nil` for any, and what a pointer that
matches is to be:

```vdu
(. bindings :bind :pen 9 :nil '(:tool :pen :mode :rect :color 0xffff0000 :width 8.0))
(. bindings :bind :mouse :nil +pev_right '(:tool :hand))
```

The first rule that matches is the one. `(bindings-default)` is how a board
starts: a pen draws, its other end rubs out, and with the button on its side
held it moves things; the mouse draws with its left button and moves things
with its right; a finger moves things. A pointer that is a `:pen` draws what
it is bound to, `:mode`, or what the board is set to.

## The board

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
which it puts in front of the document's or behind, and the handles and
instruments. What is put on top of the top layer is `:appended`, and can be
drawn onto what is already there.

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

A `:size` part with a third thing, -1.0 or 1.0, is an end: dragged along
the instrument, that end goes with the pointer and the other end stays
where it is on the board. The two ends of a ruler are.

A pointer that goes down in a part does what the part is for: moves the
instrument, turns it about a point of it, makes it longer, puts it away. Only
a pointer that is alone on it does. A finger that holds a ruler while a pen
draws along it does not move it, and nor does the pen. Two pointers on its
parts move and turn it together.

`Ruler`, `Protractor`, `Circle`, the whole protractor, which is a
`Protractor`, and `Setsquare` are each a `:rebuild` that fills in that
table from `:length`. Another instrument is another table.

## The palette

```image
apps/media/whiteboard/data/palette.cwb
```

Chris: "Animating interactive pellets and menu's, mostly keep the surface
clear for the user to work unhindered". A hand that goes down and comes up
where there is nothing, with nothing selected, opens a `Palette` there,
`lib/cwb/palette.inc`. The hand is the right button of a mouse, a finger, a
pen with the button on its side held, or the left button when the board is
set to select. `(palette-enable board)` is what makes a board do it, the app
and the `cwb` command both do.

It is three rings about the point. The tools. The colours and the widths.
Things to do: undo, redo, a ruler, a protractor, a set square, snap,
duplicate, delete. The wedge of what is set has a line of light at its rim,
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
