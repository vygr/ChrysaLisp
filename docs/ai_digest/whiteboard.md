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
(. bindings :bind :pen 9 :nil '(:tool :pen :color 0xffff0000 :width 8.0))
(. bindings :bind :mouse :nil +pev_right '(:tool :hand))
```

The first rule that matches is the one. `(bindings-default)` is how a board
starts: a pen draws, its other end rubs out, the mouse draws with its left
button and moves things with its right, a finger moves things.

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

The `Handles` are in front of it: eight squares and a ring round what is
selected, in select mode. A point is on them only where one of those is.

Everything a hand can do is a method, for a script: `:select`, `:select_all`,
`:transform`, `:style`, `:group`, `:ungroup`, `:order`, `:align`, `:duplicate`,
`:delete`, `:clear`, `:undo`, `:redo`, and `:add`.

Every point is in the space of the document. A size that is of the screen,
how near is near enough, how big a handle is, is divided by `:zoom`.

What is being moved is `:floating`: it is drawn over the document, not in it,
so only it is drawn as it moves. What is put on top of the top layer is
`:appended`, and can be drawn onto what is already there.

## The instruments

An `Instrument` lies on the board in front of what is drawn, and is not part
of the document. It is where it is, turned as it is, as long as it is, and
the rest is a table of paths in its own space, about its own middle:

```vdu
:outline	a path, closed. A point in it is on the instrument
:edges		((:line x0 y0 x1 y1) | (:arc cx cy r a0 a1) ...)
:parts		((what path [x y]) ...) what is :move :turn :size or :close
:marks		shapes, the ticks and numbers, which are only drawn
```

A pen that goes down near an edge is held to it, and what it draws is that
edge from where it went down to where it is: a line along a line, an arc
round an arc, the way the pen went. It is a true `L` or a true `A` in the
document, not the points of the hand. A protractor has `:mode`, `:line`,
`:pie` or `:circle`.

A pointer that goes down in a part does what the part is for: moves the
instrument, turns it about a point of it, makes it longer, puts it away. Only
a pointer that is alone on it does. A finger that holds a ruler while a pen
draws along it does not move it, and nor does the pen. Two pointers on its
parts move and turn it together.

`Ruler`, `Protractor` and `Setsquare` are each a `:rebuild` that fills in that
table from `:length`. Another instrument is another table.

## The command

`cwb` is the board without a window. See `cwb -h`.

```vdu
cwb -n 640x420 a.cwb -s make.lisp -i -o a.tga -b 0xffffffff
```

`-e` and `-s` run Lisp with `doc` and `board`. `-p` plays a file of pointer
events, a line a moment, `id kind buttons x y` with `;` between those that
are at once. `-i` lists what is in it, each item with its id, what it is and
the box round it. `-o` draws it, a `.tga` or a `.cpm`. `-k` saves nothing.

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

## What is not done

* The host gives one pointer, the mouse. Everything above the host takes any
number, of any kind, and is tested with them made up. A pen or a finger has
never been on it.

* Drawing is by one task. A document of very many shapes is drawn in stripes
by the nodes, as the Canvas demo is, not yet.

* The eraser takes out a whole item. It does not rub out part of one.

* A shape can hold `:props`, anything, kept in the file, for the Lisp that is
to act on it, what it collides with, how it moves. Nothing reads them yet.

* The paper, plain, grid, lines or axis, is the app's. It is not in a picture
of the document.
