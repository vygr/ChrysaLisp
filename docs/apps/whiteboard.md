# Whiteboard

The `Whiteboard` application is a board to draw on. What is on it is shapes,
not pixels: lines, boxes, ellipses, arcs, words, lines drawn by hand, each one
a thing that can be picked up, moved, turned, sized and grouped after it is
drawn. A board is saved as a `.cwb` file, which is text, and can be shown as a
picture anywhere a picture can, the `image` section of a page of the
[`Docs`](docs.md) app among them. This is one:

```image
apps/media/whiteboard/data/test.cwb
```

It was not drawn by hand. It was made by a script, with the `cwb` command,
see below. A board can be driven by a person, a script or an AI, and is the
same board to each.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the [`keys.md`](../reference/keys.md) documentation.

## UI

```widget
apps/media/whiteboard/widgets.inc *window* 512 512
```

## Drawing

The second row of buttons says what the pen draws: a line by hand, a straight
line, an arrow, an arrow at both ends, a box, an ellipse, either of those
filled, words, and last the eraser, which takes out what it is dragged over.
The row of colours says in what, and the three dots how thick.

Words are typed into the field on that row, and put down where the pen next
goes down.

The left button of the mouse is the pen. The right button is the hand: it
picks up what it is on and moves it, whatever the pen is set to. The middle
button moves the board about in its window.

## The palette

```image
apps/media/whiteboard/data/palette.cwb
```

Click the right button where there is nothing, with nothing selected, and a
palette opens there. So does a tap of a finger, a pen with the button on its
side held, and the left button when the arrow is the tool.

* The inner ring is the tools. Pick one and the palette goes.

* The next is the colours, and four widths. Pick as many as you like, the
spot in the middle shows what you have.

* The outer ring is undo, redo, a ruler, a protractor, a set square, snap to
the grid, duplicate and delete.

* Click its middle, or anywhere off it, to put it away.

A pen that opens a palette has what it picks to itself. Another pen, and the
mouse, keep what they had. So two people with a pen each can each have their
own tool and colour on the one board.

## Selecting, and changing what is selected

The first button of the second row is the arrow, select. With it the left
button is the hand as well, so a mouse with one button can do everything.

Press on a thing to select it, or drag a box round several. What is selected
has a box round it with eight squares and a ring:

* Drag inside the box to move it.

* Drag a square to size it. A corner keeps its shape, the middle of a side
moves only that side. The side or corner across from it stays where it is.

* Drag the ring above it to turn it.

One thing that has been turned keeps its box turned with it, the squares are
at its own corners and it is sized along its own sides. A line or an arrow
has no box: it has a ring at each end, and each is dragged to where that end
is to go.

The third row acts on what is selected: group and ungroup, delete, duplicate,
bring to the front, send to the back, and six ways to line things up. The
colours and the thicknesses set the colour and thickness of what is selected
too.

## The paper, snapping and zoom

The first row has the paper, plain, a grid, lines, or two axes. It is there to
work on and is not part of what is saved as the picture.

The two buttons after it are snap. With the first on, a point that is drawn or
dragged goes to the nearest point of the grid, a thing that is moved has its
top left corner go there, and the lines of the paper are where the grid is.
With the second on, a thing that is turned goes to the nearest 15 degrees,
and so does a line pulled by an end.

Then zoom in and zoom out, and the size of the board, as `1024x768`. Type
another size and press return and the board is that size, what is on it stays
where it is.

## The instruments

The ruler, the protractor and the set square are on the second row. Each is
put on the board, over what is drawn, and is not part of the document. What
is drawn with it is.

* Run the pen along an edge of one and what is drawn is that edge, from where
the pen went down to where it came up: a true straight line along a ruler, a
true arc round a protractor, however the hand wobbles. The pen only has to go
down near the edge.

* The three buttons after them say what the round edge of the protractor
draws: the arc, a slice with its two straight sides, or the whole circle.

* Drag the middle of an instrument, with the hand, to move it.

* Drag the strip along a long side of the ruler to turn it, about the far end
of that side. Drag an end to make it longer or shorter.

* The ring in the middle puts it away.

More than one pen can draw along an instrument at once. One that is being
held still is not moved by a pen that draws along it.

## Driving it without a board: the cwb command

`cwb` makes, changes, lists and draws a `.cwb` file from a command line.

```vdu
cwb -n 400x300 a.cwb -e "(cwb-add doc (cwb-shape (cwb-d-rect 20 20 200 120 12) :fill 0xffffd070))"
cwb a.cwb -e "(cwb-add doc (cwb-text-mid {Start} 110 70))" -i
cwb a.cwb -o a.tga -z 2 -b 0xffffffff
```

The first makes a new document and puts a box with round corners in it. The
second puts a word in the middle of the box and lists what the document then
has. The third draws it to a picture at twice the size on white.

The Lisp given to `-e`, or in a file given to `-s`, has `doc`, the document,
and `board`, the board of it, and everything in `lib/cwb/`. A shape is an SVG
path, text: `(cwb-d-rect)`, `(cwb-d-ellipse)`, `(cwb-d-line)`, `(cwb-d-arc)`
and `(cwb-d-points)` write the common ones, and `(cwb-d "M" 0 0 "L" 10 5 "Z")`
writes any. A colour is `0xAARRGGBB`.

`-p` plays a file of pointer events to the board, as a pen, a mouse and
fingers would give them, so the tools can be driven too. Each line is one
moment, one or more events with `;` between, each `id kind buttons x y`:

```vdu
# a finger holds the ruler while a pen is run along it
7 touch 1 300 330
1 pen 1 150 286 ; 7 touch 1 300 330
1 pen 1 480 288 ; 7 touch 1 300 330
1 pen 0 480 288 ; 7 touch 0 300 330
```

The picture at the top of this page is `cwb -n 640x420 ... -s` of a script of
twenty lines: a function that makes a box with a word in the middle of it, one
that makes an arrow, and the calls to them.

## How it is made

[`docs/ai_digest/whiteboard.md`](../ai_digest/whiteboard.md) has the whole of it: the document and its file,
pointers and the stage that hands them to what is on the board, the board, the
instruments and how another is written, and what is not done yet.

* `lib/cwb/doc.inc`, the document, shapes on SVG paths, the file.

* `lib/cwb/pointer.inc`, pointers, a stage, and bindings that say what each
pointer is.

* `lib/cwb/board.inc`, the board: a document, what is selected, undo, and the
tools of the surface.

* `lib/cwb/tools.inc`, the instruments.

* `lib/cwb/palette.inc`, the palette that opens on the board.

* `lib/cwb/stripes.inc`, a board of very many shapes drawn by the nodes of the
machine, a stripe each. The app does it by itself when a draw gets slow.

* `apps/media/whiteboard/`, the window round a board: `view.inc` makes the
mouse a pointer, `widgets.inc` is the toolbars, `ui.inc` what each does, and
`app.lisp` the canvases and the loop.

* `cmd/cwb.lisp`, the command.
