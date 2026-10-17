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
filled, words, and last the eraser. The eraser rubs out what it is dragged over
and leaves the rest: a bite is taken out of each thing it touches, a
line, a box, a filled shape, words, just where the eraser went. A thing
that has lost a bite is flat from then on, the outline of what was left
of it. Undo brings it back as it was. The eraser is as big as the pen
is thick: with the thin pen it rubs out a little, with the thick one a
lot. Its ring shows how much.
The row of colours says in what, and the three dots how thick.

Words are typed into the field on that row, and put down where the pen next
goes down.

Lines drawn by hand one soon after another, as the letters of a word are,
are put in a group as they are drawn, so a word that was written is picked
up and moved as one thing. Ungroup breaks it into its lines again. The
eraser still takes its bite out of just the lines it touches.

How it is done, and the other ways a board can be set to rub out, are
in [`docs/ai_digest/whiteboard.md`](../ai_digest/whiteboard.md), an eraser that bites.

The left button of the mouse is the pen. The right button is the hand: it
picks up what it is on and moves it, whatever the pen is set to. The middle
button moves the board about in its window.

A pen with a button on its side rubs out while that is held, as its other
end does if it has one.

## Moving about and zooming

What is drawn is not on the paper as ink is. The things of a board are
anywhere, as far out as you like, and the paper is a view of some of
them: a colour and some lines to work on.

* The middle button, dragged, moves what is seen. So does the wheel, or
a swipe on a mouse that has a top to swipe. There is no edge to come to.

* Two fingers down on empty board move what is seen as they move, and
zoom it as they are moved apart or together, what is between them staying
between them.

* The two zoom buttons make what is seen twice the size, or half.

* The bar along the bottom and the one up the side move what is seen
too. All of a bar is all there is to see that way, what is drawn and
where you are looking now, and its thumb is what you see of that: how
much, and where in it. Move out past the drawing and the bar takes that
in, it has no end to come to.

* Two fingers on empty board, the one turned about the other, turn the
board a quarter: past half way it jumps there, it has no other angle to
be at. It turns about the middle of the paper, and a paper that was wider
than tall is taller than wide. Control and R does the same. What lies on
the glass, the instruments and the menus, stays as it is.

The paper does not grow when it is zoomed, what is seen on it does. And
the zoom always comes to rest at twice or half of where it was, however
far the fingers went: it eases there when they let go. That is for the
lines of the paper. They are drawn as the board is seen, so a thing on a
line stays on its line, and as the zoom goes finer lines come up between
the coarse ones, faint at first, till they are the coarse ones and
fainter ones are coming up between them. At rest the coarse lines are
full and the fine ones half, never between.

With snap on, things go to the lines that show at that zoom, the fine
ones as well as the coarse.

What is seen is everybody's, and more than one can be at a board. So
none of these moves it while anybody is drawing a line: it moves when
they have done. Somebody who is holding a thing does not stop it, and
what they hold stays in their hand as the board moves under it.

A thing is the one's who took hold of it first, till they let it go.
Another's hand on it does nothing.

The eraser bites what is held as it bites anything, and what is left of
it stays in the hand. So a thing can be picked up, turned, and shaped
with the eraser as it is held.

One finger on empty board drags a box out to select with, as it did.

How a board was last seen is kept in its file, and it opens so.

## Menus, and what each pen is

There is no one tool or colour of the board. Each pen, the mouse, and the
fingers of a touch screen, has its own, and what you pick is picked for
whatever you picked it with.

* The last button of the instruments' bar, the dial, makes a menu in the
middle of the board. Make as many as you like, one each.

* A menu stays where it is till the cross at the right of its bar is
tapped. Drag it by its bar to move it.

* Its rows are the tools, the pens, four widths, and the things to do:
undo, redo, snap to the grid, duplicate, delete, the instruments, and the
palette of inks.

* A tap on a menu sets what tapped it: a pen's tip, a pen with the button on
its side held, the mouse's left button or its right, or all the fingers of
a touch screen, which are one. So two people with a pen each have their
own tool and colour, and each sees a small dot of their own colour on the
menu on what they have.

* The toolbars do the same: the tools, the widths, the inks, snap and the
lock set what pressed them, and show nothing set of their own. A button has
a small dot in the corner for each pen, or button of the mouse, or touch,
that is set to it, the same colour as on the menus.

* A tap on the board leaves a dot. No menu comes from a tap there.

What you have selected takes a colour or a width you then pick.

## Who may do what

The mouse and the keyboard are the teacher's, the presenter's. Every other
pen is a student's, each its own, and the fingers of a touch screen are
anybody's. A student can not rub out what the teacher, or another student,
drew. Only the teacher undoes and redoes. The eraser, the other end of a pen,
is the teacher's.

## Selecting, and changing what is selected

A quick tap on a thing selects it, whatever you are drawing with, and
another quick tap lets it go. Each tap changes only the thing tapped, so
tapping several things selects them all. A tap that is held, or that moves,
draws. A tap or a press is on a thing only where it is drawn, its line or
its fill, never just inside the box round it: the middle of a circle that
is not filled is the board.

Each person's selection is their own: what the buttons of the selection
bar do, they do to what the one who pressed them has selected. What
someone else has selected is theirs till they let it go: your tap, your
hand and your box leave it alone.

The first button of the second row is the arrow, select. With it the left
button is the hand as well, so a mouse with one button can do everything.
Drag a box across several things and your selection is what the box
touches, it need not go all the way round a thing; the rest of yours is let
go. A tap on nothing is a box that touches nothing: it lets go of all of
yours.

Drag a thing to move it. Dragging one you have selected moves all of
yours; dragging one you have not moves it alone, and selects nothing.

One thing selected, or a group, has a box round it with eight squares and
a ring:

* Drag a square to size it. A corner keeps its shape, the middle of a side
moves only that side. The side or corner across from it stays where it is.

* Drag the ring above it to turn it.

A thing that has been turned keeps its box turned with it, the squares are
at its own corners and it is sized along its own sides. A line or an arrow
has no box: it has a ring at each end, and each is dragged to where that end
is to go.

Several things selected are not a thing: each has a thin line round it, and
there is a box round them all, with no squares and no ring. They move
together and the selection bar acts on them all, but to size or turn them,
group them first. Two fingers on them move them and nothing more.

The third row acts on what is selected: group and ungroup, delete, duplicate,
bring to the front, send to the back, and six ways to line things up. The
colours and the thicknesses set the colour and thickness of what is selected
too. A group, an ungroup or a duplicate leaves what it made selected. An
undo or a redo lets go of everything that was selected.

## The paper, snapping and zoom

The first row has the paper, plain, a grid, lines, or two axes. It is there to
work on and is not part of what is saved as the picture.

The two buttons after it are snap, and the lock after them, and like the
tools and the inks they are each pen's own, and the mouse's buttons', and
touch's: a press turns over what that one has, and its dot shows it is on.
One that has never been set has the board's, off to begin with. With the
first on, a point that is drawn or
dragged goes to the nearest point of the grid, a thing that is moved has its
top left corner go there, and the lines of the paper are where the grid is.
With the second on, a thing that is turned goes to the nearest 15 degrees,
and so does a line pulled by an end.

The lock is on to begin with: a corner that is dragged keeps the
shape of what it sizes. With it off a corner goes where it is put, and the
thing is made wider or taller.

Then zoom in and zoom out, and the size of the board, as `1024x768`. Type
another size and press return and the board is that size, what is on it stays
where it is.

New, the first button, is an empty board. Like a change of size it is a step
that undo takes back, with all that was on the board.

## The paper, the desk, and the window

The paper is the document. The darker space round it is the desk it lies
on, and is not part of the document: an instrument can lie half off the
paper there, as a ruler does on a real desk, and a thing can be dragged
out onto it and back.

The two buttons of the title bar are the two ways to have it.

* The smaller makes the window as tight round the paper as it can be,
the paper and no desk.

* The bigger makes the board all there is on the screen: no title, no
toolbars, no bars, and the paper the size of the screen. What you do
there you do with menus: one is made in the middle if none is open, and
while it is so a menu has one more thing in its outer ring, a window: pick
it and the window is back as it was, where it was. The escape key does the
same.

## The paper is the size of what is on it

When a board is saved the paper is made the size of what is drawn, with a
little room round it, and so it is when one is loaded. The file, shown as a
picture, is all that was drawn and no more.

The scissors, after clear, take away everything that is nowhere on the
paper. A thing that is partly on it stays. It can be undone.

## The instruments

The ruler, the protractor, the whole protractor, a circle of 360 degrees,
and the two set squares, one of 30 and 60 degrees and one of 45, are on
the second row, the two protractors together and then the two set squares. Each is put on the board, over what is drawn, and is not part of the document. What
is drawn with it is.

* Run the pen along an edge of one and what is drawn is that edge, from where
the pen went down to where it came up: a true straight line along a ruler, a
true arc round a protractor, however the hand wobbles. The pen only has to go
down near the edge.

* A straight edge is an edge from the 0 of its marks to where they end. A
line drawn along it stops there, however far the pen goes on.

* What the round edge of a protractor draws is set on the protractor: the
ring near its middle shows it, an arc, a slice with its two straight sides,
or a slice that is filled, and a tap on the ring goes to the next. The whole
protractor has the whole circle too, and a circle that is filled.

* A protractor is numbered every ten degrees both ways from its 0, as a
real one is: the bigger numbers one way, the smaller under them the other.

* Each is marked as the real thing is, a ruler and a set square to the
millimetre, a protractor to the degree, and the parts of it that do things
are the parts it has, each with a sign on it.

* Drag anywhere on an instrument that is not one of those to move it.

* A ruler is turned by the strip along either side, where its numbers are,
about the 0 of that side. Its middle moves it.

* To turn one, drag a part with the arrow that goes round: the hole by
either end of the ruler, which turns it about the 0 of the marks at its
other end, the top side is marked from the left and the bottom from the
right, so with a 0 on a point lines can be drawn from that point every way; the inner scale
of the protractor; a sharp corner of the set square.

* An end of the ruler dragged makes it longer or shorter at that end. Its
other end stays where it is, so a ruler with its 0 on a point can be pulled
out as far as a line from that point is to go.

* To make one bigger or smaller, drag a part with the arrow with two heads:
an end of the ruler, a square by the straight side of the protractor, the
mark of the square corner of the set square.

* The outer scale of the protractor makes it bigger or smaller too: drag it
out or in. A pen close on its round edge draws along the edge instead.

* Two fingers on an instrument, anywhere on it, move it, turn it and size it
all at once.

* An instrument that is turned is pulled to the angles that matter when it
is near one: strongly to every 45 degrees, and to every 10. Between them it
is free.

* An instrument that is made bigger has more marks along its straight
sides, as far apart as they were: a longer ruler, a bigger set square that
measures further. The degrees round a protractor are the one thing that is
spread out, and their numbers are bigger with it.

* The angle it is turned to is written on it, in degrees. The ruler has it
twice, once for each side it can be read from: its top side is centimetres
from the left, its bottom side inches from the right, written the other way
up.

* Take hold of an instrument and it comes in front of the others. Press the
right button on one and it goes behind them, to get at one that is under it.

* The ring with the cross puts it away. So does dragging it off the edge
of what shows and letting go: when none of it can be seen it is gone.
While you still have hold of it, it can go off and come back.

More than one pen can draw along an instrument at once. One that is being
held still is not moved by a pen that draws along it.

## It opens as it was closed

When the Whiteboard is closed it keeps what it was, in `whiteboard.tre` in
your folder under `usr/`, as every app keeps its own: where its window was
and how big, what each toolbar was set to, the zoom and where the view was,
the words in the text field, the instruments that were out, and what was
drawn. Opened again, it is that. What could be undone is not kept.

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

* `lib/cwb/palette.inc`, the menus that float on the board.

* `lib/cwb/stripes.inc`, a board of very many shapes drawn by the nodes of the
machine, a stripe each. The app does it by itself when a draw gets slow.

* `apps/media/whiteboard/`, the window round a board: `view.inc` makes the
mouse a pointer, `widgets.inc` is the toolbars, `ui.inc` what each does, and
`config.inc` what is kept when it is closed, and `app.lisp` the canvases and
the loop.

```image
docs/diagrams/wb_layers.cwb
```

* `cmd/cwb.lisp`, the command.
