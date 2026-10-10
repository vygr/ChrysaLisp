# Functions

### CPM-info

```code
(CPM-info stream) -> (width height type) | (-1 -1 -1)
```

### CPM-load

```code
(CPM-load stream) -> :nil | canvas
```

### CPM-save

```code
(CPM-save canvas stream type [rle lz4 ident]) -> canvas
```

### CWB-info

```code
(CWB-info stream) -> (width height type) | (-1 -1 -1)
```

### CWB-load

```code
(CWB-load stream [scale]) -> :nil | canvas

each pixel is scale by scale samples, default 1
```

### SVG-info

```code
(SVG-info stream) -> (width height type) | (-1 -1 -1)
```

### SVG-load

```code
(SVG-load stream [scale]) -> :nil | canvas
```

### TGA-info

```code
(TGA-info stream) -> (width height type) | (-1 -1 -1)
```

### TGA-load

```code
(TGA-load stream) -> :nil | canvas
```

### TGA-save

```code
(TGA-save canvas stream type) -> canvas

a canvas as a .tga, not packed, 32 bits a pixel whatever type is,
the top row first. It is the pixels as they are with a header of 18
bytes before them, so it is quick, and most things that show
pictures show it
```

### XML-parse

```code
(XML-parse stream fnc_in fnc_out fnc_text)

break the stream into svg tokens, symbols, strings etc
parse the commands and attributes calling back to the user functions
```

### _structure

```code
a line of a structure, the base after it. A union is lines that all

start at the one base, and it ends where the longest of them does. A
union can have a union in it. This did call itself for each, by name,
and it is a function of a module, its name is gone once the module
has loaded, so a structure with a union in it could not be made. The
unions that are open are a list, each (lines next base longest)
```

### _structure_line

```code
a line that is not a union, the base after it
```

### abi

```code
(abi) -> sym
```

### action-maximise

```code
step zoom up to +zoom_max (3x)
```

### action-minimise

```code
step zoom down to +zoom_min (1x)
```

### action-pointer

```code
a pen or a finger. It goes to the view it is on, or the one it went

down on while it is down. The mouse is not moved by it
```

### action-quit

```code
launch logout app
```

### action-resized

```code
the host window is a new size
```

### action-shown

```code
the host window is on show again
```

### aead-open

```code
(aead-open key nonce aad sealed) -> :nil | str
```

### aead-seal

```code
(aead-seal key nonce aad data) -> str
```

### aead-tag

```code
the tag of what was encrypted and what goes with it. The key of the

tag is the start of block 0 of the stream, and is for this nonce
alone. Each of the two is made up with 0 to a whole 16 bytes, and
how long they were comes last
```

### age

```code
(age path) -> 0 | time ns
```

### ahead

```code
(ahead chain cands tokens) -> :nil | token index

look ahead. A break has been found inside the forms that open on this
line at the tokens in chain, outermost first. Those forms are going to
be over several lines. Better that such a form starts a line of its
own, near the left, than hangs off the end of this one. So give the
gap before the outermost of them, if there is one and it is more than
a last resort.
```

### align

```code
(align num div) -> num
```

### apply-tokens

```code
(apply-tokens tokens body ind end [start])

move the stack of open forms over the tokens up to end. The tokens
are of a line, or a part of one that begins at the char start.
```

### array?

```code
(array? form) -> :t | :nil
```

### array??

```code
(array?? form) -> :t | :nil
```

### ascii-lower

```code
(ascii-lower num) -> num
```

### ascii-upper

```code
(ascii-upper num) -> num
```

### atom?

```code
(atom? o) -> :t | :nil
```

### ball-rows

```code
(ball-rows centre radius mat4x4_obj mat4x4_frust height) -> (y y1)

the rows of a frame of that height a ball may be on, as it is seen.
All of them if it is at the eye or behind it. The ball is as big as
the most the matrix of its object stretches anything
```

### bindings-default

```code
(bindings-default) -> bindings

how a board starts out. A pen draws and its other end rubs out, and
with the button on its side held it moves things. The mouse draws
with its left button, moves things with its right and the board
itself with its middle. A finger moves things
```

### bit-mask

```code
(bit-mask mask ...) -> val
```

### bitcnt

```code
(bitcnt n) -> num bits
```

### board-copy-items

```code
(board-copy-items items) -> items

items that can be changed and leave those they were copied from as
they were. What a shape is flattened to goes with it, it is the same
```

### board-item-corners

```code
(board-item-corners item) -> :nil | (x y x y x y x y)

the four corners of the box of an item, its own box by its own
matrix, where they are in the space it is in. :nil if it draws nothing
```

### board-pen-d

```code
(board-pen-d points) -> d

the path of a line drawn by hand, points is x y x y ... It goes
from the first to the last, and between is curved: each point is
where the line turns toward, and it passes half way between each two
```

### board-rub-group

```code
(board-rub-group doc group x y r) -> :nil | items

the items of a group with what of its lines is within r of a point,
of the space the group is in, rubbed out. :nil if none was touched.
What is in it that is not a line, or is a group itself, is left
```

### board-snap

```code
(board-snap v step) -> v

to the nearest step, as it is if the step is 0
```

### board-two-point-mat

```code
(board-two-point-mat a0 b0 a1 b1) -> matrix

the move, turn and change of size that takes two points to where
they are now, as two fingers on a thing do. Each is (x y)
```

### boot-id

```code
(boot-id) -> hex | ""

the id of the boot image this node's machine has, "" if it was made
before there was one
```

### boot-id-file

```code
(boot-id-file cpu abi) -> path
```

### boot-id-of

```code
(boot-id-of files) -> hex

the id of those source files, whatever order they are given in. The
name of each and the hash of what is in it, all hashed
```

### bracket-cursors

```code
(bracket-cursors cursors start_y end_y) -> (si ei)

find range of cursors intersecting [start_y, end_y].
we bracket the range by including one extra cursor before and after
to ensure merges are handled correctly.
```

### build-method-overrides

```code
initialize :overrides as an empty list for ALL existing method entries
```

### build_tree_and_codebook

```code
Builds a deterministic, canonical Huffman tree and codebook from a frequency map.

This version uses a more optimal insertion sort pattern for tree building.
```

### byte-to-hex-str

```code
(byte-to-hex-str num) -> str
```

### canvas-brighter

```code
(canvas-brighter col) -> col
```

### canvas-darker

```code
(canvas-darker col) -> col
```

### canvas-flush

```code
(canvas-flush)

flush any shared pixmaps that have no users.
4 refs are held by the cache Lmap and this loop !
```

### canvas-info

```code
(canvas-info file) -> (width height type) | (-1 -1 -1)
```

### canvas-key

```code
(canvas-key canvas) -> 0 | key

the key of the shared memory the pixels of a canvas are in, 0 if
they are its own
```

### canvas-load

```code
(canvas-load file flags [swap_mode]) -> :nil | canvas
```

### canvas-mesh-create

```code
(canvas-mesh-create verts) -> 0 | mesh

the vertices of a mesh, kept on the GPU, from the bytes of a reals,
the attrs of a vertex one after another
```

### canvas-mesh-destroy

```code
(canvas-mesh-destroy mesh) -> mesh
```

### canvas-pair-create

```code
(canvas-pair-create vertex fragment layout) -> 0 | pair

a vertex shader and a pixel shader that draw triangles, from the text
of their stages. The layout is a byte for how many attrs a vertex
has, a byte for the cull, 0 none, 1 what faces away, 2 what faces us,
then a byte for the floats of each attr
```

### canvas-save

```code
(canvas-save canvas file type [optionals...]) -> :nil | canvas
```

### canvas-shade

```code
(canvas-shade col) -> col

a little darker, a quarter of the way to black, the edge of a thing
that is in shadow
```

### canvas-shader-create

```code
(canvas-shader-create vertex fragment) -> 0 | shader

a shader from the text of its vertex and fragment stages
```

### canvas-shader-destroy

```code
(canvas-shader-destroy shader) -> shader

a shader, or a pair
```

### canvas-shader-format

```code
(canvas-shader-format) -> 0 | 1 | 2

the shading language the host GUI driver takes for a shader,
0 if it can not draw one, 1 for MSL, 2 for SPIR-V
```

### canvas-shared

```code
(canvas-shared width height scale [key]) -> :nil | canvas

a canvas with its pixels in shared memory, that the nodes of this
machine can all draw on. With no key the pixels are made, and
(canvas-key) is the key of them. Given that key, and the same size,
a task on another node has a canvas on the same pixels. :nil if the
host has no shared memory, or there are no pixels of that key.
```

### canvas-tint

```code
(canvas-tint col) -> col

a little brighter, a quarter of the way to white, the edge of a thing
that the light is on
```

### chacha20

```code
(chacha20 key nonce counter data) -> str
```

### char-class

```code
(char-class key) -> str

create char class
these are sorted interned char strings
can be searched with (bfind)
```

### check-date

```code
(check-date td) -> :t | :nil
```

### circle

```code
(circle r) -> path

cached circle generation, quantised to 1/4 pixel
```

### civil-from-days

```code
(civil-from-days days) -> (year month day)

O(1) arithmetic calendar conversion (month 0..11, day 1..31)
```

### cpu

```code
(cpu) -> sym
```

### cpu-arith

```code
one step of + - * /, by the types of the two sides
```

### cpu-block

```code
forms for a block. With no flag it is the body of the function,

and the value of the last form is the return value. With a flag
it is the body of a loop, and the flag is set to leave the loop.
The forms of a block go onto a list. An if and a for have lists of
their own, for the blocks in them, and the rest of a block can go
inside the arms of an if that leaves it. Each is a thing to do, a
block, its flag and the list its forms go onto, on a list of those
still to do. No function here calls itself
```

### cpu-edge-cut

```code
where the edge from vertex a, in front of the near plane, to vertex b,

behind it, crosses the plane, every number of the vertex
```

### cpu-exits?

```code
does this block hold a return, or a break of the loop it is in ? The

blocks in it are asked in turn, from a list of those still to ask
```

### cpu-fold

```code
run it now if all the args are constants
```

### cpu-funcs

```code
-> the lambda of main, or of the function named

the constants worked out, and each function a lambda
```

### cpu-loop-return?

```code
does this block hold a return from inside a loop ?
```

### cpu-mat-mul

```code
a matrix times a matrix, each row of the one dotted with each

column of the other
```

### cpu-mat-vec

```code
a matrix times a vec4, each row of the matrix dotted with it
```

### cpu-mat-vec3

```code
the 3 by 3 of a matrix times a vec3
```

### cpu-near-cut

```code
(cpu-near-cut (v0 v1 v2)) -> ((v0 v1 v2) ...)

a triangle cut by the near plane, where z is -w, as the native code
cuts it. None, the triangle, a smaller one, or two
```

### cpu-op

```code
a built in op
```

### cpu-pow

```code
whole part of the power by squaring, the rest by repeated roots
```

### cpu-share

```code
that much of a 32 bit argb pixel, a share of 0 to 256
```

### cpu-zero

```code
what a value of the type is before it is set
```

### create

```code
a child is started, the word that it has comes to the task mailbox.

If the children are to be away from this node, and there is another,
it is on one of the others, each in turn
```

### create-word

```code
tail is what follows the word, a space, or nothing for a part of a

word that goes on in the next line. With a link, where it goes, the
word is a Link, and is connected to the :link_event of the Md, if
whoever made the Md gave it one
```

### csr-cmp

```code
(csr-cmp csr1 csr2) -> + 0 -
```

### csr-floor

```code
(csr-floor csr) -> csr

floor cursor to line boundaries
sorted cursor with point towards the bottom
```

### csr-map-delete

```code
(csr-map-delete px py cx cy ax ay) -> (nx ny)

map a point (px py) against a deletion from (cx cy) to (ax ay)
```

### csr-map-insert

```code
(csr-map-insert px py icx icy ecx ecy dy ax ay) -> (nx ny)

map a point (px py) against an insertion at (icx icy) ending at (ecx ecy)
```

### csr-sort

```code
(csr-sort csr) -> csr

sort cursor so (cx cy) <= (ax ay)
```

### csr-within

```code
(csr-within csr1 csr2) -> :t | :nil

returns :t if csr2 is enclosed within (or equal to) csr1
```

### cwb-add

```code
(cwb-add doc item [index]) -> item

put an item on top of a layer, default the top one
```

### cwb-adopt

```code
(cwb-adopt doc item) -> item

give an item, and each item in it if it is a group, an id of this
document, if it has none
a list that grows as it is gone along, so ids are in the order of
the items, a group and then what is in it
```

### cwb-bounds

```code
(cwb-bounds items [m]) -> (x y x1 y1) | :nil

the box round all that the items draw, :nil if they draw nothing
```

### cwb-box-join

```code
(cwb-box-join a b) -> box | :nil

the box round two boxes, either can be :nil
```

### cwb-box-meet?

```code
(cwb-box-meet? a b) -> :t | :nil
```

### cwb-box-of

```code
(cwb-box-of box m) -> (x y x1 y1)

the box round a box that a matrix has moved. Each side is the least
or the most of what each of its two parts can come to
```

### cwb-d

```code
(cwb-d [cmd | num] ...) -> d

a path from its commands and numbers, (cwb-d "M" 0 0 "L" 10 5 "Z")
```

### cwb-d-arc

```code
(cwb-d-arc cx cy r a0 a1 [pie]) -> d

part of a circle about a point, from one angle round to another the
way the angle grows, in radians from the x axis. A slice of pie, with
the two lines to the middle, if pie. Once round or more is a circle.

It is written as arcs of no more than a third of a turn each. The A
of a path says where an arc ends and not where its middle is, that
is worked out from the two ends, and when they are close together,
an arc most of the way round, or across from each other, half way,
there is little to work it out from: the arc came off its circle
```

### cwb-d-ellipse

```code
(cwb-d-ellipse cx cy rx [ry]) -> d

an ellipse about a point, a circle if only one radius is given
```

### cwb-d-line

```code
(cwb-d-line x y x1 y1) -> d
```

### cwb-d-points

```code
(cwb-d-points points [closed]) -> d

a line through each point in turn, points is x y x y ..., back to
the first if closed
```

### cwb-d-rect

```code
(cwb-d-rect x y x1 y1 [r]) -> d

a box from one corner to the other, its corners round by r if given
```

### cwb-doc

```code
(cwb-doc [width height]) -> doc

a new document, of one empty layer. The background is a colour, 0
for none, what is behind shows. The style is what the app draws
behind it to work on, :plain :grid :lines or :axis, with :grid the
gap, it is not part of the picture
```

### cwb-draw

```code
(cwb-draw canvas doc [m clip skip]) -> count

draw every layer that is not hidden, the first at the back. skip is
ids, of items of the layers, that are not drawn, those being moved
about, which are drawn over the rest
```

### cwb-draw-items

```code
(cwb-draw-items canvas items m [clip]) -> count

draw items on a canvas, by a matrix, :nil for as they are. With a
clip, a box in the canvas's space, a shape that is nowhere in it is
not drawn. How many shapes were drawn
```

### cwb-each

```code
(cwb-each doc fnc)

call (fnc item layer_index holder) for every item of the document,
those in groups too, the holder is the list it is in
```

### cwb-find

```code
(cwb-find doc id) -> :nil | (item layer_index holder)

an item by its id, with the layer it is in and the list that holds it
```

### cwb-fit

```code
(cwb-fit doc [pad]) -> :nil | (dx dy)

make the document the size of what is in it: every item of every
layer is moved, all by the same, so that the box round them all is
pad in from the top left, default 16, and the width and height are
that box and pad all round. How far they were moved, :nil for a
document with nothing in it, which is left as it is
```

### cwb-flat

```code
(cwb-flat shape) -> (fills strokes (x y x1 y1) | :nil)

the polygons a shape is drawn with, those of its fill and those of
its stroke, and the box they are all in, in its own space. :nil for
the box of a shape that draws nothing. Worked out once and kept
```

### cwb-get

```code
(cwb-get item key) -> value
```

### cwb-group

```code
(cwb-group items [key val] ...) -> group
```

### cwb-hit

```code
(cwb-hit doc x y [tol m]) -> :nil | (item layer_index shape)

the item of a layer a point is on, the one on top, in a layer that is
not hidden or locked. The item is one of the layer's own, a group if
the shape that was hit is in one. m is the matrix the document is
seen by, if the point is in the space of a view of it
```

### cwb-hit-box

```code
(cwb-hit-box doc x y [m]) -> :nil | (item layer_index :nil)

the item of a layer whose box a point is in, the smallest of them if
it is in more than one, in a layer that is not hidden or locked. For
a finger, which is not fine enough to be on the line of a box that is
not filled, and means the box
```

### cwb-id-set

```code
(cwb-id-set ids) -> ids | set

ids as what is quick to ask of: the list if it is short, or a set of
them. One that is a set already is that set
a set says it is a list too, so it is asked if it is a set, first
```

### cwb-id?

```code
(cwb-id? ids id) -> :nil | found

is an id one of these, a list or what (cwb-id-set) made of one
```

### cwb-in

```code
(cwb-in data) -> :nil | doc

a document from the tree of one, (cwb-out). One from before this
version is made into one of this. Anything else is not a document
```

### cwb-in-box

```code
(cwb-in-box doc box [m]) -> ((item layer_index) ...)

the items of the layers that are wholly in a box, as a drag round
them has it, not those of a layer that is hidden or locked
```

### cwb-item

```code
(cwb-item type key_vals) -> item
```

### cwb-item-in

```code
(cwb-item-in tree) -> item

an item from what a file has of it. A key that is not known is left.
Each item is put in its place in the list that is to hold it, which
is made first, with a place for each
```

### cwb-item-out

```code
(cwb-item-out item) -> tree

an item as it is in a file: its type, then each thing it has that is
not what it would have anyway, a key and its value. A group has its
items the same way. What is left to do is a list
```

### cwb-items

```code
(cwb-items doc [index]) -> items

the items of a layer, default the top one, the list itself
```

### cwb-layer

```code
(cwb-layer doc [index]) -> (name flags items)

a layer, default the top one
```

### cwb-load

```code
(cwb-load stream) -> :nil | doc

a document from a .cwb file
a file that is not a tree at all throws as it is read, where there
are errors to throw
```

### cwb-mat

```code
(cwb-mat [a b tx c d ty]) -> matrix
```

### cwb-mat-invert

```code
(cwb-mat-invert m) -> matrix | :nil

the matrix that undoes it. One that flattens everything to a line
has none, and that of no change is given
```

### cwb-mat-move

```code
(cwb-mat-move tx ty) -> matrix
```

### cwb-mat-mul

```code
(cwb-mat-mul ma mb) -> matrix | :nil

the one matrix that does what mb does and then what ma does
```

### cwb-mat-paths

```code
(cwb-mat-paths m paths) -> paths

new paths, each moved by the matrix, the same ones if it is :nil
```

### cwb-mat-point

```code
(cwb-mat-point m x y) -> (x y)
```

### cwb-mat-scale

```code
(cwb-mat-scale sx [sy cx cy]) -> matrix

about a point, default the origin
```

### cwb-mat-turn

```code
(cwb-mat-turn angle [cx cy]) -> matrix

by an angle in radians, the way from the x axis to the y axis, about
a point, default the origin
```

### cwb-new-id

```code
(cwb-new-id doc) -> id
```

### cwb-num

```code
(cwb-num n) -> str

a number as a path has it, no more of it than there is
```

### cwb-old-in

```code
(cwb-old-in data) -> doc

a document from a file of before this, version 2 or 3: polygons, in
groups in 3. Each polygon list is a shape that is filled, the size is
the box round them all and a margin
```

### cwb-out

```code
(cwb-out doc) -> emap

a document as the tree a .cwb file is, to be saved by itself or as a
part of something else that is
```

### cwb-outline

```code
(cwb-outline shape) -> ((closed path) ...)

the lines of a shape, each open or closed, in its own space. The
letters of a :text are closed. A line of less than two points is
left out
```

### cwb-paper

```code
(cwb-paper canvas width height back style gap [y y1]) -> canvas

on a canvas that is width by height: back, a colour, 0 for plain
paper, and the lines of the style, gap apart. Only rows y up to y1 if
given. It is all boxes, so that a clip, if the canvas has one, holds
```

### cwb-pick

```code
(cwb-pick items ids [out]) -> items

those of the items whose id is one of the ids, in the order they are
in, or with out those whose id is not
```

### cwb-remove

```code
(cwb-remove doc id) -> :nil | item

take an item out of the document, a group goes with all in it
```

### cwb-remove-ids

```code
(cwb-remove-ids doc ids) -> doc

take many items out at once. Those that are items of a layer, as
what is selected is, go in one pass of each layer. Any that are not,
inside a group, are then taken out one by one
```

### cwb-rub

```code
(cwb-rub item x y r) -> :nil | items

an item that (cwb-rub?) with what of it is within r of a point, of
the space it is in, rubbed out. The lines that are left, each a new
item like it, none if all of it went. :nil if none of it was that
near. An end that was its own keeps how that end was drawn, an arrow
say, and an end that was cut is round
```

### cwb-rub-points

```code
(cwb-rub-points p cx cy r) -> :nil | ((x y x y ...) ...)

a line through points, x y x y ..., with what of it is within r of a
point taken out. What is left, each a line of two points or more,
cut where it meets the circle, to the nearest sixteenth, a root is
not exact and a path is text. :nil if none of it was within r
```

### cwb-rub?

```code
(cwb-rub? item) -> :t | :nil

can part of it be rubbed out: a line, one that is drawn and not
filled, and has an end. A box, a filled thing, words and a group can
not, each is all there or not there
```

### cwb-save

```code
(cwb-save doc stream) -> stream
```

### cwb-set

```code
(cwb-set item [key val] ...) -> item

set what an item has. What it was flattened to is let go of, unless
all that was set was its matrix, its name or its props, which do not
change what it is flattened to
```

### cwb-shape

```code
(cwb-shape d [key val] ...) -> shape

a shape of that path, black, 2 wide and not filled unless it is said.
The keys are :fill :stroke, each a colour, 0 for none, :width, :rule,
:nonzero or :evenodd, :join, :miter :bevel or :round, :cap1 and :cap2,
its two ends, :butt :square :tri :arrow or :round, :m, :name, :props
```

### cwb-shape-hit?

```code
(cwb-shape-hit? shape m x y [tol]) -> :t | :nil

is a point on what a shape draws, its fill or its stroke, or within
tol of it, default 0
```

### cwb-slot

```code
(cwb-slot key) -> index
```

### cwb-text

```code
(cwb-text text x y [key val] ...) -> shape

words, their left end and the line they sit on at that point. :font
and :font_size say which letters, they are filled, black unless it is said
```

### cwb-text-mid

```code
(cwb-text-mid text x y [key val] ...) -> shape

words with their middle at a point, to label a box by its middle say
```

### cwb-walk

```code
(cwb-walk items m fnc)

call (fnc shape matrix) for each shape of the items, and of the
groups in them, the one at the back first. The matrix is the one that
puts the shape where it is in the space the items are in, given one
for that space, or :nil. A list of what is left to do, no function
here calls itself
```

### date

```code
(date [secs]) -> (sec min hour day month year dotw)
```

### day-of-the-week

```code
(day-of-the-week dotw) -> str
```

### days-from-civil

```code
(days-from-civil year month day) -> days
```

### days-in-month

```code
(days-in-month month year) -> days (28..31)
```

### days-in-year

```code
(days-in-year year) -> 365 | 366
```

### decode-date

```code
(decode-date str) -> td
```

### destroy

```code
a child is told to go, the job it had goes back on the queue
```

### diff-hunk

```code
one change, the lines of a that go and the lines of b that come, from

after line ha of a and line hb of b. The ironed path has them in
blocks, so they are written as blocks, a run of lines deleted, a run
added, or the one changed for the other
```

### diff-range

```code
a line, or the first and last of a run of them
```

### dispatch

```code
the next job on the queue to a child, if there is one
```

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```

### ed-add

```code
(ed-add p q) -> p

the point p and the point q, added, into p. A point is 4 numbers
```

### ed-car

```code
(ed-car o) -> o

each part down to 16 bits, what is over going on to the next, and
what is over the top coming back in at the bottom 38 times
```

### ed-copy

```code
(ed-copy a) -> a new number of the field, the same as a
```

### ed-decode-neg

```code
(ed-decode-neg bytes) -> :nil | point

the point of 32 bytes, with its x the other way, which is what a
check wants. :nil if they are not a point of the curve
```

### ed-encode

```code
(ed-encode p) -> str

a point as 32 bytes, its y, with whether its x is odd in the top bit
```

### ed-inv

```code
(ed-inv a) -> 1 over a, a new number
```

### ed-mod-l

```code
(ed-mod-l x) -> str

a list of 64 numbers, the bytes of a number, the low one first, less
every multiple of the order of the base point that will come off it,
as 32 bytes. The list is changed
```

### ed-mul-ref

```code
(ed-mul-ref o a b) -> o

what (ed-mul) does, in Lisp, to check it by
```

### ed-odd?

```code
(ed-odd? a) -> 0 | 1
```

### ed-pack

```code
(ed-pack n) -> str

a number of the field as its 32 bytes, the low byte first, and less
than the prime, the prime is taken off twice if it will come off
```

### ed-point

```code
(ed-point x y) -> a point, of its x and y
```

### ed-pow2523

```code
(ed-pow2523 a) -> a to the power of (p-5)/8, a new number
```

### ed-reduce

```code
(ed-reduce hash) -> str

the 64 bytes of a hash as a number, less the order of the base point
as often as it will come off, 32 bytes
```

### ed-scalarmult

```code
(ed-scalarmult q scalar) -> point

the point q added to itself the number of times the 32 bytes of the
scalar say, the low byte first. q is not changed
```

### ed-secret

```code
(ed-secret seed) -> (scalar prefix)

the number a seed stands for, and the other half of its hash
```

### ed-unpack

```code
(ed-unpack bytes) -> a number of the field, from its 32 bytes
```

### ed25519-public

```code
(ed25519-public seed) -> str

the public key of a secret seed of 32 bytes, 32 bytes
```

### ed25519-sign

```code
(ed25519-sign seed message) -> str

the signature of a message under a secret seed, 64 bytes
```

### ed25519-verify

```code
(ed25519-verify public message signature) -> :nil | :t

is it the signature of this message, by the holder of the secret
that this is the public key of ?
```

### elem-end

```code
(elem-end tokens i n) -> pos

where the element that starts at token i ends
```

### empty?

```code
(empty? form) -> :t | :nil
```

### encode-date

```code
(encode-date [td]) -> str
```

### env?

```code
(env? form) -> :t | :nil
```

### escape-regexp

```code
(escape-regexp str) -> str
```

### even?

```code
(even? num) -> :t | :nil
```

### exec

```code
(exec form)
```

### exfat-add

```code
a new file or directory, of this data, in the directory it names
```

### exfat-alloc

```code
(exfat-alloc vol want) -> :nil | (clusters no_fat)

clusters for a file. A run of free clusters if there is one long
enough, and the file then needs no chain. If not, the first free
clusters there are, with a chain through them. :nil if there are
too few. The clusters are marked as used.
```

### exfat-begin

```code
(exfat-begin vol) -> vol

the start of some writing. Every (exfat-begin) has its (exfat-end), and
they can be one inside another. The writing in between goes to the
device at the last (exfat-end), so many small files cost little more
than one.
```

### exfat-bitmap-save

```code
the bitmap back to the clusters it is kept in, the ones that changed
```

### exfat-block

```code
a block of the device, from those kept if it is there
```

### exfat-busy

```code
the volume says of itself that it is being written, and that goes to

the device at once, before anything else does. If the device is pulled
out, or the power goes, whoever mounts it next can tell.
```

### exfat-chain

```code
(exfat-chain vol start len no_fat) -> (cluster ...)

the clusters of a file or a directory. A len of :nil is all of the chain
```

### exfat-checksum

```code
the checksum of the entries of a file, all but the checksum itself
```

### exfat-cluster-offset

```code
where a cluster is on the device, the first is cluster 2
```

### exfat-data

```code
(exfat-data vol start len no_fat [keep]) -> str

the bytes of a file or a directory, a run of clusters is one read.
keep is for a directory, its blocks are kept
```

### exfat-delete

```code
(exfat-delete vol path) -> :nil | :t

a file, or a directory that has nothing in it
```

### exfat-dir

```code
(exfat-dir vol entry) -> ((name dir size start no_fat index count) ...)

what is in a directory
```

### exfat-dir-put

```code
entries to a directory, from an index on. They can cross from one

cluster of the directory to the next, so they go an entry at a time
```

### exfat-drop

```code
a block is no longer kept, and is not to be written
```

### exfat-end

```code
(exfat-end vol) -> vol
```

### exfat-entries

```code
(exfat-entries vol start len no_fat) -> ((name dir size start no_fat index count) ...)

the files and directories of a directory. A file is a set of entries,
one for the file, one for its data, then as many as its name needs
```

### exfat-eset

```code
the entries of what was found, as they are in the directory above it
```

### exfat-fat-set

```code
the cluster that follows a cluster, to the allocation table
```

### exfat-find

```code
(exfat-find vol path) -> :nil | (name dir size start no_fat index count)
```

### exfat-flush

```code
(exfat-flush vol) -> vol

the blocks that were changed, to the device, in the order they are on
it, and those that are next to each other as one write
```

### exfat-format

```code
(exfat-format path | stream size [label cluster_shift]) -> :nil | :t

a new volume of that many bytes with nothing in it. A path is a new
file of the host, the image of a disk. A stream is written where it
is, and must be that long, a memory stream of zeros say.
cluster_shift is the sectors of a cluster as a power of 2, it is 4096
byte clusters up to 256MB, 32768 up to 32GB and 131072 over that if
it is not given.
```

### exfat-free

```code
(exfat-free vol) -> bytes

the room there is left, the clusters that are not in use
```

### exfat-grow

```code
(exfat-grow vol found) -> :nil | found

a directory is given one more cluster. found is the entries from the
root down to it, and comes back with the directory as it now is.
```

### exfat-hash

```code
the hash of a name, of its upper case, a byte at a time
```

### exfat-keep

```code
a block is kept. When there are too many, those that were changed go

to the device and all are let go
```

### exfat-list

```code
(exfat-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
```

### exfat-load

```code
(exfat-load vol path) -> :nil | str
```

### exfat-mkdir

```code
(exfat-mkdir vol path) -> :nil | :t

a new directory, of one cluster with nothing in it
```

### exfat-mount

```code
(exfat-mount path | stream) -> :nil | vol

a path is a file of the host that is the image of a disk, opened
to be read and written. A stream can be a memory stream.
```

### exfat-name

```code
a name is 16 bit characters, low byte first, here made UTF-8
```

### exfat-next

```code
the cluster that follows this one, from the allocation table
```

### exfat-path

```code
(exfat-path vol path) -> :nil | (root ... entry)

the entries from the root down to a path
```

### exfat-raw-read

```code
whole blocks from the device, :nil if they are not all there
```

### exfat-raw-write

```code
whole sectors to the device
```

### exfat-read

```code
(exfat-read vol offset len [keep]) -> str

bytes of the device. A block that is kept comes from where it is kept,
it may have been changed. A run of blocks that are not is one read of
the device, and they are kept if the run is short or if keep is asked
for, as it is for a directory, which is read over and over.
```

### exfat-release

```code
the clusters of a file are free again
```

### exfat-rename

```code
(exfat-rename vol from to) -> :nil | :t

a file or a directory is given a new name, a new directory to be in,
or both. Its data is not moved. :nil if there is one of that name
there, or if a directory would end up inside itself.
```

### exfat-replace

```code
a file that is there is given new data. Its entries stay where they

are and are changed. The new data is put beside the old, which is let
go when the new is in place. Only if there is no room for both is the
old let go first.
```

### exfat-root

```code
the root as an entry, its length is not kept, its chain is followed
```

### exfat-same?

```code
are two names the same, as the volume sees it, with no regard to case
```

### exfat-save

```code
(exfat-save vol path data) -> :nil | :t

a file of this data. One of that name is replaced, and is still there
as it was if there is no room for the new data.
```

### exfat-scan

```code
look for a run of free clusters from one cluster up to another. The

first few free ones are noted on the way, in case there is no run.
A byte of the bitmap that is all in use is stepped over in one.
```

### exfat-set

```code
(exfat-set vol name dir size start no_fat) -> str

the entries of a file, one for the file, one for its data, and as
many as its name needs, with the checksum of them all
```

### exfat-slot

```code
(exfat-slot vol found want) -> :nil | (index found)

room for that many entries, one after another, in a directory. A
deleted entry is room, and so is everything from the end mark on.
If there is none the directory is grown, till there is.
```

### exfat-split

```code
(exfat-split path) -> (parent name)
```

### exfat-stamp

```code
the time now, as a directory entry keeps it, months from 1
```

### exfat-store

```code
data to clusters, a run is one write. What is left of the last block

is made zero. The rest of the last cluster is not written, nobody is
given it to read, and a directory is always whole clusters
```

### exfat-sum

```code
the checksum of the boot sectors and of the upper case table, 32 bit
```

### exfat-sync

```code
(exfat-sync vol) -> vol

all that was written is on the device. A file stream holds its last
write till it is flushed or closed, and a task that ends with the
stream open would lose it. A memory stream is not flushed, a flush
ends the string where it stands.
```

### exfat-units

```code
(exfat-units name) -> (unit ...)

a UTF-8 name as the 16 bit characters the disk holds
```

### exfat-upcase-load

```code
the table of the volume, the upper case of every character in turn.

0xffff then a count stands for that many that are their own upper case
```

### exfat-upcase-table

```code
(exfat-upcase-table) -> str
```

### exfat-used?

```code
is a cluster in use, the bitmap has a bit for each, cluster 2 is bit 0
```

### exfat-walk

```code
(exfat-walk vol path fnc) -> vol

(fnc path entry)
every file and directory under a path, a directory before what is in
it. It keeps a list of the directories still to do, it does not call
itself, so the depth of the tree costs no stack.
```

### exfat-write

```code
(exfat-write vol offset data) -> vol

bytes to the device. A run of whole blocks goes straight to it. The
rest, the ends of a long write and all of a short one, change the
blocks that are kept, a block that is only part written is read first.
```

### exfat-zeros

```code
(exfat-zeros len) -> str

always a string of its own, it is often changed in place
```

### exfat-zone

```code
the time zone a stamp is in, quarter hours from UTC, and the bit

that says it is known, or a reader takes the stamp as its own time
```

### export

```code
(export env symbols)
```

### export-classes

```code
(export-classes classes)
```

### export-symbols

```code
(export-symbols symbols)
```

### fat32-chain

```code
(fat32-chain vol start) -> (cluster ...)

the clusters of a file or a directory. A chain that goes round on
itself is cut off at the size of the volume
```

### fat32-checksum

```code
the checksum of an 8 and 3 name, each part of a long name has it
```

### fat32-data

```code
(fat32-data vol start len [keep]) -> str

the bytes of a file or a directory, a run of clusters is one read.
A len of :nil is all of the chain, a directory has no length. keep is
for a directory, its blocks are kept
```

### fat32-dir

```code
(fat32-dir vol entry) -> ((name dir size start no_fat index count) ...)

what is in a directory
```

### fat32-entries

```code
(fat32-entries vol start) -> ((name dir size start no_fat index count) ...)

the files and directories of a directory. The parts of a long name
come before the entry of their file, the last part first, and are
its name if they are all there and their checksum is that of its
8 and 3 name. Then that is its name.
```

### fat32-find

```code
(fat32-find vol path) -> :nil | (name dir size start no_fat index count)
```

### fat32-list

```code
(fat32-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
```

### fat32-load

```code
(fat32-load vol path) -> :nil | str
```

### fat32-mount

```code
(fat32-mount path | stream) -> :nil | vol

a path is a file of the host that is the image of a disk. A stream
can be a memory stream. A volume that is FAT12 or FAT16 is not taken.
```

### fat32-next

```code
the cluster that follows this one, the top 4 bits are not part of it.

Every file has a chain, so it is read from the block as it is kept,
an entry of the table never lies over the end of a block
```

### fat32-path

```code
(fat32-path vol path) -> :nil | (root ... entry)

the entries from the root down to a path
```

### fat32-root

```code
the root as an entry
```

### fat32-shift

```code
the power of 2 that n is, :nil if it is not one
```

### fat32-short

```code
the 8 and 3 name of an entry. It is held in upper case, with a bit

each for a name and an end that are to be shown in lower. A character
past 127 is of a code page that is not known here, it is taken as
Latin 1
```

### fat32-walk

```code
(fat32-walk vol path fnc) -> vol

(fnc path entry)
every file and directory under a path, a directory before what is in
it. It keeps a list of the directories still to do, it does not call
itself, so the depth of the tree costs no stack.
```

### fcluster

```code
the boot sectors, 12 of them, and the same again as a spare
```

### files-all

```code
(files-all [root exts cut_start cut_end]) -> paths

all source files from root downwards, none recursive
```

### files-all-depends

```code
(files-all-depends paths [imps end]) -> paths

create list of all dependencies, with implicit options
```

### files-all-vp-source

```code
(files-all-vp-source) -> ordered_files
```

### files-classes-info

```code
(files-classes-info [forced]) -> :nil | class_db
```

### files-depends

```code
(files-depends path [end]) -> paths

create list of immediate dependencies
```

### files-dirs

```code
(files-dirs paths) -> paths

return all the dir paths
```

### files-function-info

```code
(files-function-info [forced]) -> :nil | func_db
```

### files-scan

```code
(scan-files files handler [split_class comment]) -> files

iterates through files, processing lines.
new files returned by the handler are merged into the work list.
```

### find-break

```code
(find-break tokens body ind limit) -> :nil | token index
```

### find-gap

```code
(find-gap tokens body ind limit collect) -> :nil | :again | token index

where to break a line, by the rules of the forms on it. The gaps are
only gathered up if the line is too long, or collect is given, as most
lines are not, and need no more than a look for a break that must be.
:again if such a break was found, and the gaps are needed to look ahead.
```

### fixed?

```code
(fixed? form) -> :t | :nil
```

### fixed??

```code
(fixed?? form) -> :t | :nil
```

### fixeds?

```code
(fixeds? form) -> :t | :nil
```

### fixeds??

```code
(fixeds?? form) -> :t | :nil
```

### flatten

```code
(flatten list) -> list
```

### flm-add

```code
(flm-add film canvas) -> film

the next frame of the film, what is on the canvas now. It must be
the size of the first. The pixmap of the canvas is left as argb.
```

### flm-close

```code
(flm-close film) -> frames

no more frames. The feed is flushed and let go, which is the end of
it for the encoder, and when it has written the last of the film it
says how many frames it made, which is the result
```

### flm-encode

```code
the encoder task. The feed is the size of a frame, then frame after

frame of pixels, till it ends
```

### flm-encode-frame

```code
the next frame of the film, written to its stream
```

### flm-encoder

```code
the state of a film being written to a stream
```

### flm-open

```code
(flm-open stream format) -> film

a film to be written to a stream, format is the bits of a pixel,
1, 8, 12, 15, 16, 24 or 32. The encoder is started, and waits for
frames
```

### flm-stage

```code
the source of the encoder task, it says where its feed is, makes the

film, and says how many frames there were
```

### float-time

```code
(float-time [smooth]) -> (sec min hour)
```

### flush-bits

```code
(flush-bits stream (array bit_pool bit_pool_size))
```

### form-elems

```code
(form-elems tokens i n) -> num

the count of elements in the form that opens at token i, operator and
all, or 1000 if it does not close on this line
```

### form-op

```code
(form-op tokens body i) -> :nil | str

the operator of the form that opens at token i
```

### form-rule

```code
(form-rule op par_rule par_argc tokens body i n) -> rule

the rule for the form that opens at token i, given the rule of the
form around it, and the index of that form's last element
```

### format-lisp

```code
(format-lisp data [limit wide]) -> str

format the source text. limit is the line length to break at, default
80, VP assembler lines get half as much again. wide if it is all VP.
A limit of 0 keeps the line breaks of the source, and only indents and
tidies.
```

### found?

```code
(found? text substr) -> :t | :nil
```

### func-load

```code
(func-load name) -> (body links refs)

cache loading of function blobs etc
```

### func-refs

```code
(func-refs fobj) -> ([sym] ...)
```

### func?

```code
(func? form) -> :t | :nil
```

### gap-weight

```code
(gap-weight rule j) -> :nil | num

the weight of the gap before argument j of a form, :nil for never
```

### gather

```code
(gather map|set [key] ...) -> (val|key|:nil ...)

gather a list of [key|val|:nil]
```

### gen-norms

```code
(gen-norms verts tris) -> (norms new_tris)
```

### get-cstr

```code
(get-cstr str idx) -> str
```

### glsl-block

```code
the lines of a block. An if and a for have blocks in them: what is

still to come is a list, the next thing last, a statement and how far
in it is, or a line that closes what was opened before it
```

### glsl-decls

```code
the inputs as uniforms, the constants, the globals and the functions

of a program, onto the lines of whoever calls
```

### grow

```code
start children till the herd is the size the worker nodes call for
```

### gui-rpc

```code
(gui-rpc (view cmd) -> :nil | view
```

### gui-theme-rpc

```code
(gui-theme-rpc name) -> :nil | name

tell the GUI the theme of the desktop is now this one, it tells every
window, lib/theme/theme.inc
```

### handler

```code
(handler state page line) -> state
```

### hash-tree

```code
(hash-tree things) -> tree

the tree of things, each (path hash [meta]). A map, the path of a
folder, "" the top, to (hash kids), the kids in the order of their
names. A tree of nothing has a top, with nothing in it
```

### hash-tree-compare

```code
(hash-tree-compare mine theirs) -> (differ meta gone into only_mine only_theirs)

one folder of mine against the same folder of theirs, both as kids.
The names of: things I have that they have not, or not the same.
Things that are the same but for the meta, each (name meta) with
mine. Things they have and I have not. Folders we both have that are
not the same, to go into. Folders only I have, and folders only they
have. A name that is a thing on one side and a folder on the other is
both a thing and a folder that one has and the other has not
```

### hash-tree-kids

```code
(hash-tree-kids tree folder) -> kids

what is in a folder, nothing if there is no such folder
```

### hash-tree-read

```code
(hash-tree-read text) -> kids

a folder from its text. A name may have spaces in it, it is last
```

### hash-tree-root

```code
(hash-tree-root tree) -> hash

the hash of the top, which is of all of it
```

### hash-tree-text

```code
(hash-tree-text kids) -> str

a folder as text, a line each, in the order of their names. It is
what a folder's hash is the hash of, and what goes in a message
```

### hash-tree-under

```code
(hash-tree-under tree folder) -> paths

the path of every thing in a folder and in the folders below it
```

### held-mouse-id

```code
the view the mouse is on. While a button is held that is the view it

went down on, a window being dragged can lag behind the pointer, and
to look under the pointer again would let go of it
```

### hex-decode-stream

```code
(hex-decode-stream in_stream out_stream [chunk_size flags])
```

### hex-encode-stream

```code
(hex-encode-stream in_stream out_stream [chunk_size flags])
```

### hmac-sha256

```code
(hmac-sha256 key data) -> str

the hash of a str with a key, HMAC of RFC 2104, 32 bytes. Only one
who has the key can make it, or check it. A key longer than a block
is hashed first, and one that is shorter is made up with 0.
```

### http-body-str

```code
(http-body-str resp) -> str
```

### http-get

```code
(http-get url [headers dest_stream]) -> pmap | :nil
```

### http-head

```code
(http-head url [headers]) -> pmap | :nil
```

### http-pool-checkin

```code
(http-pool-checkin host port conn headers)
```

### http-pool-checkout

```code
(http-pool-checkout host port) -> ((in out) is_pooled) | :nil
```

### http-pool-clear

```code
(http-pool-clear)

Close and flush all idle pooled sockets
```

### http-post

```code
(http-post url body [headers dest_stream]) -> pmap | :nil
```

### http-read-body

```code
(http-read-body in [headers dest_stream]) -> stream
```

### http-read-headers

```code
(http-read-headers in) -> pmap
```

### http-request

```code
(http-request method url [headers body dest_stream]) -> pmap | :nil
```

### huffman-build-freq-map

```code
Scans a stream to build a frequency map for static Huffman coding.
```

### huffman-compress

```code
(huffman-compress in_stream out_stream token_bits)
```

### huffman-compress-static

```code
Compresses a stream using a pre-built static model.
```

### huffman-decompress

```code
(huffman-decompress in_stream out_stream token_bits)
```

### huffman-decompress-static

```code
Decompresses a stream using a pre-built static model.
```

### huffman-read-codebook

```code
Reads a self-describing codebook from a stream and reconstructs the model.
```

### huffman-write-codebook

```code
Writes a self-describing codebook (via the frequency map) to a stream.
```

### idle

```code
every child that is up and has no job is given one
```

### import

```code
(import path [env])
```

### import-from

```code
(import-from [symbols classes])
```

### in-get-state

```code
(in-get-state in) -> num
```

### in-mbox

```code
(in-mbox in) -> mbox
```

### in-set-state

```code
(in-set-state in num) -> in
```

### int-to-hex-str

```code
(int-to-hex-str num) -> str
```

### iso-surface

```code
(iso-surface grid isolevel) -> tris

determine the index into the edge table which
tells us which vertices are inside the surface
```

### join

```code
(join seqs seq [mode]) -> seq
```

### json-container?

```code
(json-container? obj) -> :t | :nil
```

### json-escape-str

```code
(json-escape-str str) -> str
```

### json-from-tre

```code
(json-from-tre obj) -> str
```

### json-get-items

```code
(json-get-items obj) -> list
```

### json-parse

```code
(json-parse str_or_stream) -> val
```

### json-read-number

```code
(json-read-number stream first_c) -> (val next_c)
```

### json-read-string

```code
(json-read-string stream) -> str
```

### json-serialize-scalar

```code
(json-serialize-scalar obj) -> str
```

### json-skip-ws

```code
(json-skip-ws stream [lookahead]) -> str | :nil
```

### json-stringify

```code
(json-stringify obj) -> str
```

### json-to-tre

```code
(json-to-tre str_or_stream) -> pmap | list | scalar
```

### keep-word?

```code
(keep-word? text pos) -> :t | :nil

would a source scanner, reading a line that starts at pos, take it
for a form it looks for ? Such a line must start where it did in the
source, no more and no less.
```

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```

### lambda?

```code
(lambda? form) -> :t | :nil
```

### leapyear?

```code
(leapyear? year) -> :t | :nil
```

### lerp

```code
vertex kd is where the edge from vertex ka, in front of the near

plane, to vertex kb, behind it, crosses the plane
```

### lighting

```code
(lighting col at)

very basic attenuation and diffuse
```

### lighting-at3

```code
(lighting-at3 col at sp)

very basic attenuation, diffuse and specular
```

### line-indent

```code
(line-indent tokens body lead) -> indent

the indent of a line, from the open forms and what it starts with
```

### line-op

```code
(line-op tokens body) -> :nil | str

the operator of the form the line starts with
```

### link-text

```code
(link-text line) -> line

each link of the line, [text](target) or ![text](target), is made
the words " <l:target> text </l> ", for (format-words) to make the
text a Link. One with no text, no target, or a space in its target
is left as it is written. A word is drawn with a space after it, so
where there is none in the text, before a link or after, it is said
```

### lisp-nodes

```code
(lisp-nodes [system]) -> nodes

the nodes known. With a system id, only those on that machine, and
with :t, only those on the same machine as this node, so those that
share its file system.
```

### lisp-systems

```code
(lisp-systems) -> systems

the system ids of the machines known, this one first. A node not yet
heard from has no system id, all zero.
```

### list?

```code
(list? form) -> :t | :nil
```

### list??

```code
(list?? form) -> :t | :nil
```

### load-stream

```code
(load-stream path) -> :nil | stream
```

### log2

```code
(log2 num) -> num
```

### lognot

```code
(lognot num) -> num
```

### long-to-hex-str

```code
(long-to-hex-str num) -> str
```

### lz4-compress

```code
(lz4-compress in_stream out_stream [window_size])
```

### lz4-decompress

```code
(lz4-decompress in_stream out_stream [window_size])
```

### macro-func?

```code
(macro-func? form) -> :t | :nil
```

### macro?

```code
(macro? form) -> :t | :nil
```

### mail-read-timeout

```code
(mail-read-timeout mbox [timeout_us]) -> msg | :nil
```

### mat3x2-mul-f

```code
(mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f
```

### match

```code
(match text meta start) -> -1 | end
```

### match?

```code
(match? text regexp) -> :t | :nil
```

### matches

```code
(matches text regexp) -> matches
```

### max-length

```code
(max-length list) -> max
```

### md-anchor

```code
(md-anchor text) -> name

the name a heading is found by, as the place of a link, file.md#name.
The text of the heading in lower case, each space a -, and all but
letters, digits, - and _ left out, with where a link in it goes to.
"A heading with [a link](x.md) in it!" is a-heading-with-a-link-in-it
```

### md-link-file

```code
(md-link-file file target) -> path

the file a link of a document is to. It is from the folder the
document is in, unless it starts at the root with a /. Each .. is a
folder up, each . is no move, and what is after a # is a place in
the file, not part of its name
```

### mesh-ball

```code
(mesh-ball mesh) -> (centre radius)

a ball that the mesh is inside, the middle of its box and how far
the furthest vertex is from there
```

### mesh-corners

```code
(mesh-corners mesh) -> str

a mesh as the shaders want it. A face is lit flat, so a vertex that
faces share is a vertex of its own for each of them, where it is and
then the normal of that face, 7 numbers. The triangles are then just
the vertices in the order they come. It is the bytes of the numbers,
as they go in a message to a child that draws.
```

### mesh-corners-smooth

```code
(mesh-corners-smooth mesh) -> str

a mesh as the shaders want it, lit smooth. The normal at a vertex is
that of all the faces that share it, taken together, so a face shades
from corner to corner and a ball of faces is round
```

### min-length

```code
(min-length list) -> min
```

### month-of-the-year

```code
(month-of-the-year month) -> str
```

### msafe?

```code
(msafe? o) -> :t | :nil
```

### msl-block

```code
the lines of a block. An if and a for have blocks in them: what is

still to come is a list, the next thing last, a statement and how far
in it is, or a line that closes what was opened before it
```

### msl-inputs

```code
the inputs block, packed types and pad words give the std140 offsets.

A matrix is on a 16 byte boundary as it is, and is its columns
```

### msl-name

```code
a name is given a trailing _ so it can not be a word of MSL, half say
```

### msl-set-inputs

```code
the members that are inputs, from the block
```

### msl-struct

```code
the shader as a struct. The inputs, the globals, the attrs and the

varyings are members, the constants are members with a value, and the
functions are methods
```

### must-break?

```code
(must-break? rule elems) -> :t | :nil

is the form broken however short, as it has more than the one of its
last arguments
```

### neg?

```code
(neg? num) -> :t | :nil
```

### nempty?

```code
(nempty? form) -> :t | :nil
```

### net-quiet

```code
(net-quiet [delay_us] [stable_count] [last_cnt]) -> (node_id ...)

wait till the number of nodes known has stayed the same for a while.
A network that never settles must not hang the caller, so it gives
up, with what it has, after 20 times as long as a settled one takes.
```

### next-op

```code
(next-op tokens body i n) -> :nil | str

the word a line would start with, were it to start at token i
```

### nil?

```code
(nil? o) -> :t | :nil
```

### nlo

```code
(nlo num) -> num
```

### nlz

```code
(nlz num) -> num
```

### node-auto

```code
(node-auto [per_cpu]) -> (pid ...)

size the network to this machine. Start nodes till there are per_cpu
for each processor, default 1, and no more than 32, then wait for
them to be seen, so what is started next can spread over them. One
for each is where a full build is quickest, more only share them out.
```

### node-link

```code
(node-link name) -> net_id

start a shared memory link on this node. The node at the other end
starts one of the same name.
```

### node-net

```code
(node-net shape [cnt fronts kind script id sid]) -> (pid ...)

make this node node 0 of a network of a shape, (node-shape) has them
and what cnt is, and wait for the nodes to be seen. It is how a
session starts, from one node, and how a network is added to one
that is running, hung from this node. The first fronts of them,
default none, are of kind and run script, as (node-start) has it,
the other desktops of a session. With an id, a name for these nodes,
they are noted under it, to be stopped as one, (node-stop). With a
sid, a system id, they are a system of their own: this node is not
one of the shape, the whole of it is new nodes with that id, and
one link from here to the first of them is the way in.
```

### node-nets

```code
(node-nets) -> ((id shape total (pid ...) (link ...) file sid) ...)

the networks added to this machine's sessions by name, (node-net)
with an id, each with the processes of its nodes, the names of its
links, the note it is in, and its system id, :nil if it has this
machine's. Whichever node of the machine started them
```

### node-shape

```code
(node-shape shape [cnt]) -> (total pairs)

the links of a network of a shape, each a list of two node numbers.
:full, every node to every other, :ring, :star, node 0 in the middle,
:tree, two below each, :mesh, a square that wraps round, and :cube. cnt
is the number of nodes, the width for a :mesh or a :cube. Not given, or
0, it is sized to this machine, the most that is no more than one for
each processor. No more than 64 nodes, 32 for :full.
```

### node-spawn

```code
(node-spawn [num kind script]) -> (pid ...)

start num more nodes on this machine, default 1, each linked to this
node and to each other. kind and script are as (node-start) has them,
so a GUI node can be added to a TUI network, and a TUI node to a GUI
one.
```

### node-start

```code
(node-start total pairs [fronts kind script net sid]) -> (pid ...)

this node is node 0 of total, start the others, 1 on, linked as pairs
has them, each a list of two node numbers. A pid of -1 is a node the
host could not start. The first fronts of them, all if not given, are
of kind, the host program, :gui or :tui, this node's own if not given,
and run script. A node with a script is a front of its session, a way
in to it, a desktop is service/gui/app.lisp. net is a line to head
the note of them with, so they can be found and stopped as one,
(node-nets) and (node-stop). sid is a system id for them, in place
of this machine's, 16 characters, the kernel's -sid option.
```

### node-stop

```code
(node-stop id) -> :nil | num

stop the nodes of a network that was added by name, all of them, and
clear away its links and the note of it. :nil if there is none of
that name, else how many nodes are to go. Each node of this machine
is sent a task that exits it if it is one of them, not at once: the one
that asks may itself be a node of the network, and is given the time
to say what it did
```

### node-wait

```code
(node-wait want)

wait till that many nodes are seen, so what is started next can
spread over them, and not for ever
```

### nto

```code
(nto num) -> num
```

### ntz

```code
(ntz num) -> num
```

### num-to-utf8

```code
(num-to-utf8 num) -> str
```

### num?

```code
(num? form) -> :t | :nil
```

### num??

```code
(num?? form) -> :t | :nil
```

### nums?

```code
(nums? form) -> :t | :nil
```

### nums??

```code
(nums?? form) -> :t | :nil
```

### obj-args

```code
(obj-args field [offset]) -> args

convert to obj-get/obj-set common args
```

### obj-set-args

```code
(obj-set-args (field value [offset])) -> args

convert to obj-set args
```

### odd?

```code
(odd? num) -> :t | :nil
```

### open-child

```code
(open-child task mode) -> net_id
```

### open-pipe

```code
(open-pipe tasks [modes]) -> ([net_id | 0] ...)
```

### open-remote

```code
(open-remote task node mode) -> net_id
```

### open-starts

```code
(open-starts tokens end) -> (token index ...)

the forms that open on this line, and are still open at token end, as
the tokens they open at, outermost first
```

### open-task

```code
(open-task task node mode key_num reply)
```

### opt-flag

```code
(opt-flag 'opt_var) -> args
```

### opt-mesh

```code
(opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
```

### opt-num

```code
(opt-num 'opt_var) -> args
```

### opt-nums

```code
(opt-nums cnt 'opt_var) -> args
```

### opt-str

```code
(opt-str 'opt_var) -> args
```

### opt-toggle

```code
(opt-toggle 'opt_var) -> args
```

### opt-vector

```code
(opt-vector vector part) -> (new_vector new_indices)
```

### options

```code
(options stdio optlist) -> :nil | args

scan the stdio args and process according to the optlist
```

### options-find

```code
(options-find optlist arg) -> :nil | opt_entry
```

### options-print

```code
(options-print &rest _)
```

### options-split

```code
(options-split args) -> (a0 [a1] ...)
```

### os

```code
(os) -> sym
```

### out-set-state

```code
(out-set-state out num) -> out
```

### pad

```code
(pad form width [str]) -> str
```

### pairs-value?

```code
(pairs-value? rule j) -> :t | :nil

is argument j of a form, with this rule, the value of a binding
```

### pal-ease

```code
(pal-ease x) -> num

0 to 1, by a little past 1 and back, as a thing that springs open does
```

### pal-icon

```code
(pal-icon what val) -> shapes

what is drawn on a wedge, about 0 0, in a box of 20 or so
```

### pal-line

```code
(pal-line d [key val] ...) -> shape

a line of an icon, round at its ends and its corners unless it is said
```

### pal-point

```code
(pal-point r a) -> (x y)

the point that far from the middle, at an angle from straight up
round the way a clock goes
```

### pal-wedge-d

```code
(pal-wedge-d r0 r1 a0 a1) -> d

the path of a wedge of a ring, between two radii and two angles
```

### palette-enable

```code
(palette-enable board [colors widths]) -> board

from now a hand that taps where there is nothing, with nothing
selected, opens a palette there, its own. With the colours and the
widths given, or its own twelve and four
```

### palette-open

```code
(palette-open board x y [kind id]) -> palette

a palette on the board, about a point, for the pointer of that kind
and id, default the mouse. One that pointer had open shuts. It is kept
on the document, if that is big enough to hold it
```

### palettes

```code
(palettes board) -> palettes

those that are open on a board
```

### parse-text

```code
links, if the links of the text are to be made Links. They are not for

an Md that was given no :link_event, nothing would follow one, and it
is shown as it is written, with where it goes to be read
```

### path-angle

```code
(path-angle x y) -> angle

the angle of a vector, from the x axis round towards the y axis, in
radians, more than -pi and no more than pi. 0.0 for no vector at all.
A first guess good to a part in 300, then stepped twice toward where
the vector turned back by it lies along the x axis
```

### path-gen-earc

```code
(path-gen-earc x1 y1 rx ry phi large sweep x2 y2 dst) -> dst

the arc of an ellipse from one point to another, as the A of an SVG
path has it: the two radii, the turn of the ellipse in radians, the
larger of the two arcs if large is not 0, drawn the way the angle
grows if sweep is not 0. The first point is taken to be in dst, the
rest are added, the last is the end point exactly. Radii too small
to reach are made as big as is needed, and with no radius, or no
distance to go, it is a line
```

### path-gen-ellipse

```code
(path-gen-ellipse cx cy rx ry dst) -> dst
```

### path-gen-paths

```code
(path-gen-paths svg_d) -> ((:nil|:t path) ...)

:t closed, :nil open
```

### path-gen-rect

```code
(path-gen-rect x y x1 y1 rx ry dst) -> dst
```

### path-smooth

```code
(path-smooth src) -> dst
```

### path-stroke-polygons

```code
(path-stroke-polygons dst radius join src) -> dst
```

### path-stroke-polylines

```code
(path-stroke-polylines dst radius join cap1 cap2 src) -> dst
```

### path-to-absolute

```code
(path-to-absolute target [current]) -> path

transform a relative filename to an absolute one
```

### path-to-file

```code
(path-to-file) -> path

the path to this file
```

### path-to-relative

```code
(path-to-relative target [current]) -> path

transform an absolute filename to a relative one
```

### pbkdf2-bytes

```code
(pbkdf2-bytes state) -> str

the 8 numbers of a state as the 32 bytes of the hash, each number the
top byte first
```

### pbkdf2-sha256

```code
(pbkdf2-sha256 password salt count size) -> str
```

### pii-kill

```code
(pii-kill pid) -> :t | :nil

end a process that can not be asked to go, a node that does not
answer. :t if it was told to end or was not there. The host has to be
one that can, it says how new it is in the fourth word of (pii-host),
and with one that is older nothing is done. The native function is
looked up here, when it is called, and not in class/lisp/root.inc: a
boot image of before it was written still starts
```

### pipe-farm

```code
(pipe-farm jobs [retry_timeout]) -> ((job result) ...)

run pipe farm and collect output. A job that is not answered within
retry_timeout, in microseconds, a minute if not given, is given to a
new worker, and so is one whose node has gone. A job that has been
given out three times with no answer stops the farm, and it and any
that are still out have no result.
```

### pipe-run

```code
(pipe-run cmdline [outfun])
```

### pipe-split

```code
(pipe-split cmdline) -> ((mode cmd) ...)
```

### pipe-task

```code
(pipe-task file) -> form

a command is started as a form that loads its file, not as the file,
so that a load that throws can be caught there and the pipe told,
(import-cmd) in class/lisp/root.inc
```

### pixmap-key

```code
(pixmap-key pixmap) -> 0 | key

the key of the shared memory its pixels are in, 0 if they are its own
```

### pmap?

```code
(pmap? form) -> :t | :nil
```

### poly1305

```code
(poly1305 key data) -> str

the tag of a str, 16 bytes
```

### poly1305-add

```code
(poly1305-add ctx data) -> ctx

more of what the tag is of
```

### poly1305-end

```code
(poly1305-end ctx) -> str

the tag of all that was added, 16 bytes. The ctx is done with.
```

### poly1305-start

```code
(poly1305-start key) -> ctx

a tag with nothing in it yet. The state the native code keeps, the
bytes that do not yet make a block, and the half of the key that
is added at the end
```

### pos?

```code
(pos? num) -> :t | :nil
```

### pow

```code
(pow base exponent) -> integer
```

### profile-report

```code
(profile-report name [reset])
```

### pset?

```code
(pset? form) -> :t | :nil
```

### ptr-event

```code
(ptr-event id kind buttons x y [pressure time]) -> event

kind is :mouse :pen :eraser or :touch. buttons is those held, 0 for a
pointer that is only over the board, or has just come up. pressure is
0.0 to 1.0, default 1.0 if a button is held. time in microseconds
```

### quasi-quote?

```code
(quasi-quote? form) -> :t | :nil
```

### query

```code
(query pattern whole_words regexp ignore_case) -> (engine meta pattern)

whole words is done by the regexp engine, a plain pattern is escaped
for it. An empty pattern stays empty, and matches as it would without.
```

### quote?

```code
(quote? form) -> :t | :nil
```

### rack-fresh

```code
(rack-fresh phases [user]) -> str

on this machine, a new session for each phase, a list of command
lines, one after another, and what they said. A session is started by
the host, so it is on the boot image as it is on disk now, which a
phase before it may have made. With a user the session is theirs, and
not whoever last signed on to the machine, as run.sh -u has it
the form has no space or tab in it, a node's args are split at those
```

### rack-run

```code
(rack-run cmdline [make_first leave_out gone user tree]) -> (line ...)

make every machine that takes a sync the same as this one, then run
the command line on each, and on this one. With make_first, make and
make all boot are run first, in a session before. leave_out is the
machines not to run on, as (sync-services) names them, they are still
made the same. gone is paths to remove on the others. tree is the one
that is sent, default the system's own, the test of this sends a small
one. The lines are SYNC and GONE for each other machine, RAN for each
machine, and DONE
```

### range

```code
(range start end [step]) -> list
```

### real-to-str

```code
(real-to-str real [precision]) -> str
```

### real?

```code
(real? form) -> :t | :nil
```

### reals?

```code
(reals? form) -> :t | :nil
```

### reflow

```code
(reflow words line_width [indent tab_width]) -> lines
```

### repl-error?

```code
(repl-error? tokens body i n) -> :t | :nil

is the form that opens at token i a jump or call to :repl_error, such
a line holds the usage of a function, and is never broken
```

### replace-compile

```code
(replace-compile rep_str) -> (p_nums z_nums c_map rep_str)

memoize the compilation. atomic due to cooperative scheduling.
```

### replace-edits

```code
(replace-edits text matches compiled|rep_str) -> ((start end rep_str) ...)

returns a list of edit operations compatible with Editor buffers
```

### replace-matches

```code
(replace-matches text matches compiled|rep_str) -> text

with nothing matched the text is given back as it is, which also
covers an empty text, that has no parts to splice
```

### replace-regex

```code
(replace-regex text pattern compiled|rep_str) -> text
```

### replace-regex-edits

```code
(replace-regex-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
```

### replace-str

```code
(replace-str text pattern compiled|rep_str) -> text

substr returns a list of matches, each match is a list containing just ((start end))
this works perfectly with $0 in replace-matches
```

### replace-str-edits

```code
(replace-str-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
```

### restart

```code
restart a child
```

### restart

```code
restart a child
```

### rle-compress

```code
(rle-compress in_stream out_stream [token_bits run_bits])
```

### rle-decompress

```code
(rle-decompress in_stream out_stream [token_bits run_bits max_tokens])
```

### rpad

```code
(rpad form width [str]) -> str
```

### rule

```code
a rule is kept as (weights low one fill pairs clauses), all worked out

here, the once, so that applying a rule is only a matter of looking
```

### s-arc

```code
(s-arc cx cy r a0 a1 [steps]) -> items

part of a circle, a stroke, from one angle to another in degrees,
anticlockwise as they grow
```

### s-arc-arrow

```code
(s-arc-arrow cx cy r a0 a1 [h]) -> items

an arc with a head on its end
```

### s-arrow

```code
(s-arrow x y x1 y1 [h]) -> items

a line with a head on its far end
```

### s-box

```code
(s-box x y x1 y1) -> items

a rectangle, filled, its corners as round as a stroke's
```

### s-corners

```code
(s-corners [a b l]) -> items

the four corners of a frame
```

### s-deg

```code
an angle in degrees as the system has angles
```

### s-disc

```code
(s-disc cx cy r) -> items

a circle, filled
```

### s-fill

```code
(s-fill x y x y ...) -> items

a shape, filled, its corners a little round
```

### s-flip

```code
(s-flip items) -> items

top for bottom
```

### s-head

```code
(s-head x y dx dy [h]) -> items

the head of an arrow, at x y, for a line that comes in along dx dy
```

### s-line

```code
(s-line x y x y ...) -> items

a stroke from point to point
```

### s-loop

```code
(s-loop x y x y ...) -> items

a stroke that comes back to where it began
```

### s-map

```code
(s-map items fx fy [fr]) -> items

every point of a thing moved, x by fx and y by fy, and a radius by fr
```

### s-mirror

```code
(s-mirror items) -> items

left for right
```

### s-ring

```code
(s-ring cx cy r) -> items

a circle, its line
```

### s-rrect

```code
(s-rrect x y x1 y1 [r]) -> items

a rectangle with round corners, its line
```

### s-small

```code
(s-small items [s]) -> items

a thing made smaller, kept to the top left, its strokes as thick as
ever. It leaves room for a badge
```

### s-turn

```code
(s-turn items) -> items

across for down, a thing that lies along is made to stand
```

### scan-line

```code
(scan-line body instr) -> (tokens instr)

the tokens of a line as (kind start end), as the Lisp reader would see
them. instr is the class of the closing char if in a string.
```

### scatter

```code
(scatter map|set [key]|[key val] ...) -> map|set

scatter a list of [key]|[key val]
```

### scene-order

```code
the triangles of count vertices taken three at a time
```

### scene-pipeline

```code
the two shaders a scene is drawn with, assembled the first time. Or

those of an object that has two of its own, their files
```

### search

```code
(search text meta start) -> (list submatches {-1 | end})
```

### seq?

```code
(seq? form) -> :t | :nil
```

### setoffset

```code
adjust text offset
```

### sh-block

```code
statements in a scope of their own. A block can have blocks in it, and

they are done here one at a time, with a list as the stack of those
that are part done, as (flatten) does it. A frame is a block: its
forms, how far through them it is, its statements so far, how long
syms was when it began, and what is to be made of it when it is done
```

### sh-const-int

```code
a loop bound, an int literal or an int constant
```

### sh-decimal

```code
the Lisp reader gives a float literal as a 16.16 fixed, which is good

to 4 decimal places, so that is what it is rounded to. A number that
needs more is written as a str, and is taken as it stands.
```

### sh-expr

```code
an expression, checked and typed. One can have others in it to any

depth, and no function here calls itself: what is still to be done is
a list, the next thing last, an expression or the making of one whose
args are done, and what has been made so far is another
```

### sh-float-to-real

```code
a real from the bits of a 32 bit float
```

### sh-fold

```code
(sh-fold node fnc) -> value

what a back end makes of an expression, from the bottom up. fnc is
(lambda (node args) ...) -> value, called for each node with what was
made of those under it, in order. A back end's own function for an
expression then calls nothing that calls it, the walk is here, with a
list as the stack of what is still to do, as (flatten) does it
```

### sh-literal

```code
default, min or max of an input
```

### sh-name

```code
a new name must be a plain symbol, and must not hide anything
```

### sh-op-type

```code
result type of a built in op, or :nil if the types are wrong
```

### sh-pixel-only

```code
for a back end that has not got the vertex stage yet. It has only

pixel shaders with no varyings and no matrix in them
```

### sh-real-to-float

```code
the bits of a 32 bit float, from a real, a fixed or an int
```

### sh-returns?

```code
does every path through the block end at a return ? The arms of an

if that is last are blocks to ask it of in turn, on a list
```

### sh-stmt

```code
a statement, checked and typed, onto out. One that has blocks in it

does not do them, a function that calls itself has no stack to spare.
It leaves each on frames for (sh-block) to do next, with what is to
be made of it when it is done
```

### sh-swizzle

```code
component indices of a swizzle like :xyz or :rgb
```

### sh-tail

```code
the last form of a function is its value, as in Lisp, so if it is not

a statement it is returned. And so for the last form of each arm of an
if that is last, and of a progn that is last. The back ends still see
a return, they are statement languages.
An arm can be an if again. Each form that is to be looked at is a
place, a list and where in it, on a list of those still to do
```

### sha256

```code
(sha256 data) -> str

the hash of a str, 32 bytes
```

### sha256-add

```code
(sha256-add ctx data) -> ctx

more of what is being hashed
```

### sha256-end

```code
(sha256-end ctx) -> str

the hash of all that was added, 32 bytes. The ctx is done with.
```

### sha256-start

```code
(sha256-start) -> ctx

a hash with nothing in it yet. The state, the first 32 bits of the
fractions of the square roots of the first 8 primes, the bytes that
do not yet make a block, and how many bytes there have been.
```

### sha512

```code
(sha512 data) -> str

the hash of a str, 64 bytes
```

### sha512-add

```code
(sha512-add ctx data) -> ctx

more of what is being hashed
```

### sha512-end

```code
(sha512-end ctx) -> str

the hash of all that was added, 64 bytes. The ctx is done with.
```

### sha512-start

```code
(sha512-start) -> ctx

a hash with nothing in it yet. The state, the first 64 bits of the
fractions of the square roots of the first 8 primes, the bytes that
do not yet make a block, and how many bytes there have been.
```

### shader-attrs

```code
(shader-attrs program) -> ((name type) ...)

what a vertex shader reads of each vertex
```

### shader-compile

```code
(shader-compile forms [head_only]) -> program

with head_only only what a caller of a ready made function needs is
taken, the inputs, the attrs and varyings, the kind, and what each
function gives and takes. No body is read or checked, and the consts
and globals are :lazy. (shader-full) reads the rest when it is wanted.
```

### shader-cpu

```code
(shader-cpu program) -> lambda

(lambda x y x1 y1 input ... varying ...) -> (vec4 ...)
the lambda shades the pixels of the tile, row by row, the
centre of pixel x y is at frag coord x + 0.5, y + 0.5. A pixel
shader with varyings is given a value for each, after the inputs,
and every pixel of the tile has those.
```

### shader-cpu-args

```code
(shader-cpu-args program [((name val) ...)]) -> (val ...)

the input args for the lambda, defaults for those not given, then a
value for each varying of a pixel shader, 0 for those not given
```

### shader-cpu-func

```code
(shader-cpu-func program name) -> lambda

(lambda arg ...) -> value
a function of a file of functions, as a Lisp lambda. It is what the
native code of (shader-vp-func) is checked by. A float is a real, a
vector a reals, and a matrix a reals of 16, a row at a time.
```

### shader-cpu-tris

```code
(shader-cpu-tris vertex pixel verts tris width height [vvals pvals cull tri_size x y x1 y1]) -> str

triangles drawn with a vertex shader and a pixel shader, in Lisp, a
pixel at a time. It is slow, and is what the native code is checked
by. The args are those of (shader-vp-draw-tris), with the size of the
frame in place of a pixmap. The result is the pixels, row by row, the
top row first, each a 32 bit argb, 0 where nothing was drawn.
```

### shader-cpu-vertex

```code
(shader-cpu-vertex program) -> lambda

(lambda verts input ...) -> ((position varying ...) ...)
the lambda places each vertex of a list. A vertex is a list of its
attrs, in the order the shader has them. For each it gives where the
vertex is, a vec4, as main gave it, then what main left each varying
as, in the order the shader has them. A varying that is not set is 0.
```

### shader-dim

```code
(shader-dim type) -> :nil | 2 | 3 | 4
```

### shader-full

```code
(shader-full program) -> program

a program with all of it there. One that was loaded with only its
head has the rest read and checked now, into the same program
```

### shader-func

```code
(shader-func program name) -> (name type ((name type) ...))

a function of a program that Lisp can call
```

### shader-funcs

```code
(shader-funcs program) -> ((name type ((name type) ...)) ...)

the functions of a program, what each gives and takes
```

### shader-glsl

```code
(shader-glsl program) -> str
```

### shader-glsl-pair

```code
(shader-glsl-pair vertex pixel) -> (vertex_text fragment_text)

a vertex shader and a pixel shader that go together, to draw
triangles with, as a vertex shader and a fragment shader of GLSL.

The vertex shader has each attr of a vertex as an attribute, and each
of its varyings as a varying, of the same names, and its inputs as
uniforms. A varying it does not set is 0. Where it puts a vertex is
gl_Position as it is, z of -1 to 1 in view is what GL has.

The fragment shader has the varyings the pixel shader reads, by name,
and its inputs as uniforms. The frag coord is GL's own, y up. A pixel
whose alpha is under 1 in 255 is not drawn. The color leaves with its
alpha multiplied in, for a target that is blended that way.

An input the two both have, of the one name, is the one uniform, as
GL links them, and both are given the same precision so that it can.
```

### shader-gui

```code
(shader-gui program) -> :nil | shader

a shader the GPU can draw into a canvas, with (. canvas :shade shader
block), where block is from (shader-pack). :nil if this host can not.
The driver builds it in its own time, till then :shade draws nothing.
```

### shader-gui-frame

```code
(shader-gui-frame canvas draws) -> :nil | :error | canvas

a frame of triangles drawn by the GPU into the texture of a canvas,
with a depth buffer. draws is a list of (pair mesh vblock pblock), a
pair, a mesh, and the blocks of (shader-pack) for the two shaders of
the pair. :nil is the GPU still busy with the last frame, or the
driver still building a pair, nothing was drawn, try again. :error is
a pair the driver could not build.
```

### shader-gui-mesh

```code
(shader-gui-mesh verts) -> :nil | mesh

the vertices of a mesh, kept on the GPU, to be drawn with a pair
whose vertex shader has those attrs. verts is the bytes of
(shader-verts-str). :nil if this host can not. It is let go of with
(canvas-mesh-destroy).
```

### shader-gui-pair

```code
(shader-gui-pair vertex pixel [cull]) -> :nil | pair

a vertex shader and a pixel shader that the GPU can draw triangles
into a canvas with, (shader-gui-frame). :nil if this host can not.
With cull a triangle that faces away is left out, :front those that
face us. The driver builds it in its own time. It is let go of with
(canvas-shader-destroy).
```

### shader-gui-pair-text

```code
-> (vertex_stage fragment_stage)

the two stages of a pair, kept, both made if either is not there.
The fragment stage is written last, so if it is there both are
```

### shader-kept-text

```code
(shader-kept-text name lambda) -> str

what a back end makes of a program, text or a module, kept in a
file of that name beside the native code, and made only if it is
not there
```

### shader-key

```code
(shader-key text) -> str

what a source is known by, a hash of it. The native code made from a
source is kept under its key, so a source that has been met before
is found again without a line of it being read
```

### shader-layout

```code
(shader-layout program) -> (size (name type offset) ...)
```

### shader-load

```code
(shader-load file) -> program

a program from a file. Only its head is read, and a hash of the file
taken, which is what its native code is kept under. The rest is read
and checked when a back end has to make something from it,
(shader-full), so a file whose native code is there already costs a
hash and a few lines
```

### shader-make

```code
(shader-make files) -> str

which make of a back end this is, a hash of the files it is written
in, 8 hex digits. It is part of the name of all that a back end
keeps, so that a change to the back end is a change of name, and
what the old one made is not found. Worked out once by a task.
```

### shader-mesh-send

```code
(shader-mesh-send msg verts)

answer a child that asked for a mesh, msg is what came to the ask
mailbox, verts the vertices of the mesh as a str, (shader-verts-str)
```

### shader-msl

```code
(shader-msl program) -> str

the entry point is fragment_main
```

### shader-msl-pair

```code
(shader-msl-pair vertex pixel) -> (vertex_text fragment_text)

a vertex shader and a pixel shader that go together, to draw
triangles with. The entry points are vertex_main and fragment_main.

The vertex function takes the attrs of a vertex as [[attribute(n)]],
in the order the shader has them, and its inputs at [[buffer(0)]].
What it hands on is each of its varyings, [[user(locn n)]]. Where it
puts a vertex has z of -1 to 1 in view, a GPU of this kind has 0 to
1, so z is moved as it leaves.

The fragment function takes the varyings the pixel shader reads, by
the place each has in the vertex shader's list, its inputs at
[[buffer(0)]], and the size of the target at [[buffer(1)]], from
which, and where the pixel is, comes the frag coord, y up. A pixel
whose alpha is under 1 in 255 is not drawn. The color leaves with its
alpha multiplied in, for a target that is blended that way.
```

### shader-msl-vertex

```code
(shader-msl-vertex) -> str

the vertex shader that goes with every fragment shader, one triangle
that covers the target. Its uniform is the size of the target, and it
gives each pixel its frag coord, with y up. The entry point is vertex_main.
```

### shader-pack

```code
(shader-pack program [((name val) ...)]) -> block

a value not given is the default of the input. A float can be a
real, a fixed or an int, a vector is a sequence of them.
```

### shader-pair

```code
(shader-pair vertex pixel) -> (vertex pixel)

a vertex shader and a pixel shader that go together. Every varying
the pixel shader reads must be one the vertex shader has, of the same
type. The vertex shader may have more, they are not used.
```

### shader-read

```code
(shader-read stream) -> forms
```

### shader-real

```code
(shader-real num) -> real

a fixed is rounded as a float literal of the language is
```

### shader-size

```code
(shader-size type) -> 1 | 2 | 3 | 4 | 16

how many numbers a value of the type is
```

### shader-spirv

```code
(shader-spirv program) -> str

the entry point is fragment_main
```

### shader-spirv-pair

```code
(shader-spirv-pair vertex pixel) -> (vertex_module fragment_module)

a vertex shader and a pixel shader that go together, to draw
triangles with. The entry points are vertex_main and fragment_main.

The vertex module takes the attrs of a vertex at locations, in the
order the shader has them, and its inputs as a block, set 1 binding
0. It hands on each of its varyings, at a location. Where it puts a
vertex has z of -1 to 1 in view, a GPU of this kind has 0 to 1, so z
is moved as it leaves.

The fragment module takes the varyings the pixel shader reads, at
the place each has in the vertex shader's list, its inputs as a
block, set 3 binding 0, and the size of the target, set 3 binding 1,
from which, and where the pixel is, comes the frag coord, y up. A
pixel whose alpha is under 1 in 255 is not drawn. The color leaves
with its alpha multiplied in, for a target that is blended that way.
```

### shader-spirv-vertex

```code
(shader-spirv-vertex) -> str

the vertex shader that goes with every fragment shader, one triangle
that covers the target. Its uniform, set 1 binding 0, is the size of
the target, and it gives each pixel its frag coord, with y up. The
entry point is vertex_main.
```

### shader-stage

```code
(shader-stage program) -> :pixel | :vertex | :func
```

### shader-strip

```code
(shader-strip vfile pfile ask shared width height y y1 cull draws) -> job

the job for rows y to y1 of a frame of that width and height. ask
is the mailbox a child asks for a mesh at, shared the key of the
pixels, (canvas-key). draws is a list of (mesh vblock pblock [y y1]),
the number of a mesh, the blocks of (shader-pack) for the two
shaders, and the rows of the frame the mesh may be on, if the app
knows, all of them if not.
```

### shader-tile

```code
(shader-tile file inputs x y x1 y1 width height shared) -> job

the job for a tile of a frame of that width and height, inputs is the
block from (shader-pack), shared the key of the pixels, (canvas-key)
```

### shader-tile-show

```code
(shader-tile-show canvas msg) -> canvas

the answer to a tile. If the child could not reach the pixels of the
canvas they came with the answer, and are put there
```

### shader-unpack

```code
(shader-unpack program block) -> ((name val) ...)

a float comes back as a real, a vector as a reals
```

### shader-varyings

```code
(shader-varyings program) -> ((name type) ...)

what a vertex shader sets for the pixel shader, or a pixel shader reads
```

### shader-verts-str

```code
(shader-verts-str reals) -> str

the bytes of a reals of vertices, to send in a message
```

### shader-vp

```code
(shader-vp program) -> (shade frame_size)

the native function for a pixel shader. It is assembled if this is
the first time this CPU has been given this program.
```

### shader-vp-argb

```code
(shader-vp-argb native frame x y x1 y1 [height]) -> str

the pixels of a tile, row by row, each a 32 bit argb. If the height
of the frame is given then row 0 is the top row, as a canvas has it.
```

### shader-vp-depth

```code
(shader-vp-depth width height) -> depth

a depth buffer for a pixmap of this size, with nothing in it, all of
it as far away as can be. A new one is made for each frame, and it is
a copy of one that is kept, the one copy of the bytes. What is kept is
all but the last 4 bytes, so that joining them on is always a new
str, the one that is kept is never the one that is handed out
```

### shader-vp-draw

```code
(shader-vp-draw native frame pixmap x y x1 y1 [height alpha]) -> :nil | pixmap

a tile drawn straight onto a 32 bit pixmap, where it belongs on it,
with no copy of the pixels in between. :nil if the tile is not all
inside the pixmap. The pixels are full on, so they are right for a
pixmap that is premultiplied as well as one that is not. With alpha
the alpha of a pixel is the one main gave, for a shader that is to be
seen through. A pixmap is premultiplied, so such a shader gives its
color times its alpha.
```

### shader-vp-draw-tris

```code
(shader-vp-draw-tris pipeline verts tris pixmap depth [vvals pvals cull tri_size x y x1 y1 depth_y]) -> pixmap

draw triangles on a 32 bit pixmap. verts is a reals, the attrs of a
vertex one after another, vertex after vertex, or the bytes of one
in a str. tris is a nums, three
numbers of vertices for each triangle, the first three of every
tri_size, 3 if not given. vvals and pvals are the inputs of the two
shaders. Where a vertex shader puts a vertex, x and y of -1 to 1 are
the edges of the pixmap, y up, and z of -1 to 1 is in view, nearest
first. With cull a triangle whose vertices go round clockwise as seen
is left out, it faces away. A cull of :front leaves out those that go
round the other way, for a mesh made the other way round, or a view
that is turned over. Only the pixels of x y x1 y1 are drawn,
all of the pixmap if they are not given, so that a frame can be drawn
a part at a time, or by several tasks. A task that draws rows y to y1
only can have a depth buffer of just those rows, and says so with a
depth_y of y, the row its depth buffer starts at. A pixel whose alpha
is 0 is not drawn, and leaves the depth buffer alone. One that is
full on is written. One between goes over what is there, which is
taken to have had its alpha multiplied in, and its depth is kept as
any other, so what is see through is to be drawn after what is not,
the furthest first.
A triangle that the near plane goes through, where z is -w, is cut by
it, and what is in front is drawn.
```

### shader-vp-fill

```code
(shader-vp-fill program) -> (fill frame_size params vary_slots)

the native function for a pixel shader that fills triangles with it
```

### shader-vp-frame

```code
(shader-vp-frame program native [((name val) ...)]) -> frame

the frame the native function works in, with the inputs set
```

### shader-vp-func

```code
(shader-vp-func program name) -> func

a function of a file of functions, as a native function Lisp calls,
(func arg ...) -> value. A :float is a real, an :int a num, a vector
a reals of its size, a :mat4 a reals of 16, a row at a time, and
what comes back is one of those, made new. It is assembled the first
time this CPU meets it, and kept.
```

### shader-vp-pipeline

```code
(shader-vp-pipeline vertex pixel) -> pipeline

a vertex shader and a pixel shader as native code, to draw triangles
with. Each is a function of its own, so a vertex shader is assembled
once however many pixel shaders it is used with, and a pixel shader
once however many vertex shaders. What ties the two is where in a
placed vertex each varying of the pixel shader is.
```

### shader-vp-pixels

```code
(shader-vp-pixels native frame x y x1 y1) -> (vec4 ...)

the pixels of a tile, row by row, each a reals of 4
```

### shader-vp-place

```code
(shader-vp-place native frame verts) -> reals

place the vertices of a reals, the attrs of one after another, vertex
after vertex. The result is a reals, for each vertex where it is, 4
numbers, then its varyings. The vertices can be a str, the bytes of
such a reals, 8 to a number, as (shader-verts-str) makes, which is
how they are when they have come in a message.
```

### shader-vp-vertex

```code
(shader-vp-vertex program) -> (place frame_size attr_slots out_slots)

the native function for a vertex shader, it places vertices. A vertex
is attr_slots numbers, its attrs one after another, and what comes
out for it is out_slots numbers, where it is, 4, then its varyings.
```

### short-to-hex-str

```code
(short-to-hex-str num) -> str
```

### shuffle

```code
(shuffle list [start end]) -> list
```

### slices

```code
(slices list) -> ((s0 e0) (s1 e1) ...)
```

### solid-frame

```code
(solid-frame) -> :nil | frame

the innermost open form that is not transparent
```

### sort

```code
(sort list [fcmp start end]) -> list

the default fcmp is cmp, which is for strings
```

### split-cells

```code
the cells of a row. One with nothing in it is a cell, it keeps what

comes after it in its own column
```

### spv-block

```code
the statements of a block, up to the one that leaves it. An if and a

for have blocks in them, and no function here calls itself: what is
still to be done is a list, the next thing last, a statement, or what
comes after a block that was put there before it. A statement that
leaves, a break or a return, has the rest of its own block taken off
```

### spv-block-var

```code
a uniform block of these members, -> (var_id ptr_type_id ...) the

pointer types are those of the members. A matrix is a column at a
time, each 16 bytes on from the last
```

### spv-const

```code
a float is given as the bits of it
```

### spv-emit

```code
an instruction that gives a value of this type, to the block
```

### spv-expr

```code
(spv-expr node) -> id
```

### spv-ext

```code
an instruction of the GLSL.std.450 set
```

### spv-fold

```code
an op of two, over more than two
```

### spv-function

```code
a function, fnc is given the ids of its parameters and makes its body
```

### spv-global

```code
a variable of the module
```

### spv-local

```code
a variable of the function, they all go at its start
```

### spv-memo

```code
the id of a type or a constant, made the first time it is asked for
```

### spv-module

```code
the bytes of the module, model is 0 vertex, 4 fragment
```

### spv-op

```code
an instruction, its first word has its length and its opcode
```

### spv-port

```code
a variable that comes in to the module, class 1, or goes out, class

3, at a location, decor 30, or as a built in, decor 11. A shader's
name for it, if it has one, reads and sets it as it is
```

### spv-program

```code
(spv-program program) -> block

the inputs block of a program, a variable for each of its inputs,
constants and globals, and its functions
```

### spv-ptr

```code
a pointer type, class is 1 input, 2 uniform, 3 output, 6 private, 7 function
```

### spv-splat

```code
a float that goes with a vector is made a vector
```

### spv-start

```code
at the start of the entry point the inputs are copied from the block,

and the constants and the globals are set
```

### spv-str

```code
the words of a string, it ends with a zero byte
```

### spv-type

```code
the id of a type, made the first time it is asked for. A vector is of

floats, and a matrix of vectors, its columns, and those are made here
if they are not there, this does not call itself for them. The ids
are given out in the order they always were, the float, then this
type, then the vector a matrix is of
```

### spv-var

```code
(spv-var name) -> (id type)
```

### squeeze

```code
(squeeze body tokens) -> str

the line with one space between its tokens, and none inside a bracket.
The gap before a comment at the end is kept, it may line comments up.
```

### start

```code
start a child
```

### start

```code
start a child, on the worker nodes in turn
```

### stdio-get-args

```code
(stdio-get-args stdio) -> cmd_line
```

### stop

```code
stop a child
```

### stop

```code
stop a child
```

### str-as-num

```code
(str-as-num str) -> num

return as type num even for fixed point
```

### str-to-real

```code
(str-to-real str) -> real

handles scientific notation like 1.5e-3 or 9.97231e-09
```

### str?

```code
(str? form) -> :t | :nil
```

### str??

```code
(str?? form) -> :t | :nil
```

### stream-diff

```code
(stream-diff a b c)

difference between streams a and b, write to stream c
outputs in standard "Normal diff" format
```

### stream-patch

```code
(stream-patch a b c)

patch stream a with stream b, write to stream c
accepts standard "Normal diff" format
```

### stripes-apply

```code
(stripes-apply doc items text) -> :nil | version

make a copy of a document what the text of a delta says it is. items
is the copy's items by id, an Fmap, those that are not sent again are
kept, with what they are flattened to. The version it is then at
```

### stripes-delta

```code
(stripes-delta doc log have version) -> text

what a copy of the document at version have needs to be the document
as it is, at version: the layers, as the ids of their items, and the
items that changed, or all of them
```

### stripes-full?

```code
(stripes-full? log have version) -> :t | :nil

does a copy at version have need the whole document to be at
version. log is ((version changes) ...), the newest last, changes is
ids or :all. It does if what happened since have is not all in the
log, or any of it was :all
```

### substr

```code
(substr text substr) -> matches
```

### sv-apply

```code
a built in op whose args are all worked out before it, they are in a,

b and c, as many as it has
```

### sv-arith

```code
one step of + - * /, by the types of the two sides
```

### sv-at

```code
-> (register offset)

where a slot of the frame is
```

### sv-block

```code
code for the statements of a block. An if and a for have blocks in

them, and no function here calls itself: what is still to be done is
a list, the next thing last, a statement or what is to come after a
block that was put there before it
```

### sv-branch

```code
code to jump to the label if the bool is as sense says
```

### sv-calls?

```code
does this expression call a function of the shader ? Walked with a

list as the stack of what is still to be looked at
```

### sv-code-text

```code
the code so far as lines of VP source
```

### sv-const

```code
a float constant, to a new register
```

### sv-define

```code
a new variable, set to the value of an expression
```

### sv-emit

```code
a call of the system can lose the registers things are reached by
```

### sv-expr

```code
code for an expression, the registers that hold the value
```

### sv-fill-code

```code
the code that walks the triangles, to the code list. The slots are

those of the bindings in (sv-fill-source)
```

### sv-fill-source

```code
-> (text frame_size params vary_slots)

the VP source of the native function that fills triangles with a
pixel shader. params is where in the frame the caller's numbers go,
(ntris stride tstride vw vh cx0 cy0 cx1 cy1 cull dy vary), vary is the
first of a slot for each number of the varyings, where in a placed
vertex that number is, in bytes
```

### sv-floor

```code
floor of a register, in place
```

### sv-func-source

```code
-> text

the VP source of the native function for a function of a file of
functions. It is called from Lisp as any native function is. Its
frame is on the stack. A number arg is copied to it. A reals arg is
read where it is, by a register, the first three of them, if the
function never sets it, else it is copied too. What it gives is a
new real, num or reals, and a reals is made first and written
straight into.
```

### sv-keep

```code
(sv-keep name func info) -> (func ~info)

keep the numbers that go with a native function just made
```

### sv-kept

```code
(sv-kept name) -> :nil | (func info)

the native function of that name if it is there from before, and
the numbers that were kept with it. Nothing of the program is read
to find out. The numbers are written last, after the function is
whole, so if they are there it is
```

### sv-ld

```code
a float from a slot of the frame, or from where that slot really is
```

### sv-leaf

```code
a value with nothing under it, the registers that hold it
```

### sv-lin

```code
(sv-lin items) -> steps

lay out what is to be done, in order. What is still to be laid out is
on a stack, a list, and the next thing is the last of it
```

### sv-lin-branch

```code
the steps of a jump to a label if a bool is as sense says. The label

is in a cell, a list of it, it may not be made till the steps are done
```

### sv-lin-expr

```code
the steps of an expression, those under it still to be laid out
```

### sv-mat

```code
code for an expression that is a matrix, where in the frame it is. A

variable is where it is kept. A product is worked out into new slots,
each number of it a row of the one by a column of the other. With
into, the last product of a chain is worked out there, a place that
is no part of what it is made from.
A product can be of products. It is laid out as steps first, a list
as the stack of what is still to be laid out, and the steps are then
done with a list of where each matrix so far is
```

### sv-mat-copy

```code
a matrix from one place in the frame to another
```

### sv-mat-vec

```code
a matrix times a vector that is in registers, to new registers. A

vec3 is by the 3 by 3 of the matrix
```

### sv-name

```code
the name of the native function of a program, from the hash of its

source, and which make of this back end it is
```

### sv-native

```code
-> :nil | func

the native function of this name, assembled from the text if this
CPU has not got it yet
```

### sv-place-again

```code
as (shader-vp-place), for a result that is used at once and let go

of, as a draw does. The reals the last such call of that size gave
is used again, every number of it is written, so there is no new one
to make and clear for each object of each frame
```

### sv-pow

```code
x to the power y, both kept, to a new register
```

### sv-program

```code
-> (main_param_offsets main_ret_offset), or those of the entry named

the code of a program, after its inputs have been given their slots.
A label that sets its constants and globals, then each function at a
label of its own. The labels start with the prefix, so that the two
programs of a pair can be in the one native function.
```

### sv-reals

```code
a reals of n numbers, all 0
```

### sv-run

```code
(sv-run steps) -> values

do the steps, the code comes out as they are done. The values are a
stack, a list, each the registers a value is in
```

### sv-sets?

```code
does any statement of a block set this variable. The block is walked

with a list as the stack of what is still to be looked at
```

### sv-sin

```code
sin or cos of a register, which is given up, to a new register
```

### sv-slots

```code
room in the frame for a value of this type
```

### sv-source

```code
-> (text frame_size)

the VP source of the native function for a program
```

### sv-spill

```code
save the live registers, all of them or those of a list
```

### sv-src

```code
a float goes with every component of a vector
```

### sv-var

```code
the frame offset of a variable, the last one of that name declared
```

### sv-vertex-source

```code
-> (text frame_size attr_slots out_slots)

the VP source of the native function that places vertices
```

### swap

```code
(swap list idx idx) -> list
```

### sym-code-hex

```code
a code as four hex digits, the high byte first
```

### sym-font

```code
(sym-font symbols radius joint cap) -> str

the whole of a .ctf, of symbols each (name items), in the order they
are given, from +sf_base
```

### sym-glyph

```code
(sym-glyph code paths) -> str

a glyph of a .ctf, docs/ai_digest/ctf_command.md. Its left edge is at
0, and its advance is how wide its ink is
```

### sym-names-text

```code
lib/consts/symbols.inc, the name of each symbol and its code
```

### sym-paths

```code
(sym-paths items radius joint cap) -> paths

the outlines of a thing, at the size they are worked in. radius is
half the weight of a stroke, on the grid
```

### sym-wind

```code
(sym-wind paths) -> paths

the outlines of one item, all turned if they must be so that the
biggest goes round the one way. A glyph is filled by the non zero
rule, and two outlines that go round opposite ways cut a hole where
they overlap, a disc on the end of a stroke did
```

### sym?

```code
(sym? form) -> :t | :nil
```

### sync-hear

```code
(sync-hear reply_mbox [wait]) -> :nil | (status data)

what it says back, :nil if it does not in the time
```

### sync-ignored?

```code
(sync-ignored? rules segs is_dir) -> :nil | :t

is a path, as its parts, left out ? The folders above it are taken
to have been asked of already, as a walk down the tree does
```

### sync-inside?

```code
(sync-inside? root path [real]) -> :nil | :t

is a path, safe as written, under the root as the host has it ? Every
folder on the way has to be a folder, and the file a file, or not
there yet. A link is neither, so a link in the tree to somewhere else
is never gone through, and nothing is written or removed beyond it.
real is a set of the folders found to be so, to ask the host the once
```

### sync-kind

```code
(sync-kind folder entry) -> :nil | kind

what the host says an entry of a folder is, "4" a folder, "8" a file,
anything else something else, a link for one. :nil if it is not there
```

### sync-list

```code
(sync-list root rules [kept live]) -> ((path size hash mode) ...)

the files of a tree, with the size and the SHA-256 of each. With a
file to keep them in, a hash is not worked out again for a file whose
time and size are as they were. One changed in the last two seconds is
always hashed, its time may not change if it is changed again.
live is a map to hold them in between one call and the next, for a
task that stays, a service. The file is then read the once, and is
what is there when the task starts again
```

### sync-push

```code
(sync-push svc root rules_text [check remove kept no_modes])

	-> :nil | :old | (sent bytes removed failed send gone remoded)
make the tree a sync service has the same as the one under root. With
check nothing is changed there. With remove, what is there and not
here is removed. send and gone are the paths that differ, and the ones
only there. A file is given the mode it has here, who may read, write
and run it, and one that is the same but for its mode has that set,
unless no_modes, for a host that has no such thing. :nil if the
service does not answer, :old if it is from before there were trees
```

### sync-root

```code
(sync-root svc rules_text [no_modes wait]) -> :nil | :old | hash

the top of the tree a sync service has, by those rules. It works it
out when asked, and keeps it to be asked of its folders. :nil if it
does not answer, :old if it is a sync from before there were trees
```

### sync-rules

```code
(sync-rules text) -> rules

the rules of a .gitignore, each (anchored dir_only segments). Those
that take back a rule, !, are not known and are left out. The .git
folder is never listed
```

### sync-safe?

```code
(sync-safe? path) -> :nil | :t

a path the service will write to, by how it is written. Under its
root, and not up out of it. No .., no start at the top of the host's
files or of a drive, no ~, and no character below a space
```

### sync-seg?

```code
(sync-seg? pat seg) -> :nil | :t

does one part of a path match one part of a rule, which may have a *
```

### sync-services

```code
(sync-services [name]) -> ((mbox system_id machine root) ...)

the machines that will take a sync, as their services say
```

### sync-tell

```code
(sync-tell svc reply_mbox kind data [at total mode])

a message to a sync service
```

### sync-tree

```code
(sync-tree root rules [kept no_modes live]) -> tree

the tree of hashes of the files under a root. The mode of a file is
part of it, unless no_modes, for a host that has no such thing. kept
and live are as (sync-list) has them
```

### sync-walk

```code
(sync-walk root rules) -> paths

every file under the root that the rules do not leave out, each a
path from the root, in order
```

### task-mboxes

```code
(task-mboxes size) -> ((task-mbox) [temp_mbox] ...)
```

### task-nodeid

```code
(task-nodeid [mbox]) -> nodeid
```

### task-timeout

```code
(task-timeout s) -> ns
```

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```

### theme-current

```code
(theme-current home) -> name

the theme a user has, home is usr/<user>/. The first if none was
ever chosen, or if what the file says is not a theme. The file is the
name and no more, it is read by every app as it starts, and there is
nothing in it to go wrong
```

### theme-file

```code
(theme-file name) -> symbols_file

the font of a theme, that of the first if there is none of that name
```

### theme-save

```code
(theme-save home name)

the theme a user has from now on
```

### time-from-date

```code
(time-from-date td) -> secs
```

### time-in-seconds

```code
(time-in-seconds time) -> str
```

### timezone-init

```code
(timezone-init tz_loc) -> tz
```

### timezone-lookup

```code
(timezone-lookup query [field]) -> tz | :nil
```

### timezone-offset-seconds

```code
(timezone-offset-seconds tz) -> seconds
```

### tok-rule

```code
(tok-rule tokens body i n prule pargc) -> token

the token that opens a form, with the rule of the form, whether the
form must be broken, and its operator, kept on the end of it. They are
worked out the once, the first time the token is come to.
```

### tool-angle-wrap

```code
(tool-angle-wrap a) -> a

an angle as one of more than -pi and no more than pi
```

### tool-glyph

```code
(tool-glyph what [mode]) -> shapes

the sign of a part of an instrument, about 0 0, some 20 across: an
arrow that goes round for :turn, one with two heads for :size, a
cross for :close. For :mode, in a ring, what the instrument is set
to draw: an arc, a slice of pie, or a circle, either of those filled
```

### tool-magnet

```code
(tool-magnet angle) -> angle

an angle, in radians, or the one that matters that it is near
by 180 and then over pi, and back by pi and then over 180: a degree
in radians is too small a number to be held well
```

### tool-nearest-on-line

```code
(tool-nearest-on-line x y x0 y0 x1 y1) -> (px py t)

the point of a line nearest a point, and how far along it that is, 0 to 1
```

### tool-ticks

```code
(tool-ticks x0 y0 x1 y1 nx ny step [small mid big]) -> d

marks along a line, every step from its start, each a little line
the way nx ny points, which is one long. Every fifth is longer and
every tenth longer still
```

### transfer

```code
(transfer src_map dst_map [key val] ...) -> dst_map

transfer a list of [key val]
```

### tree-buckets

```code
(tree-buckets collection type) -> num
```

### tree-collection?

```code
(tree-collection? type) -> :nil | type
```

### tree-decode

```code
(tree-decode atom) -> atom
```

### tree-encode

```code
(tree-encode atom) -> atom
```

### tree-load

```code
(tree-load stream) -> tree | :nil

:nil if there is no stream, or nothing in it to read
```

### tree-node

```code
(tree-node ((type [buckets])) -> collection
```

### tree-save

```code
(tree-save stream tree [key_filters]) -> tree | :nil
```

### tree-type

```code
(tree-type collection) -> type
```

### trim

```code
(trim str [cls]) -> str

when it is all cls the end is before the start, and slice would reverse
```

### trim-end

```code
(trim-end str [cls]) -> str
```

### trim-start

```code
(trim-start str [cls]) -> str
```

### tsort

```code
(tsort roots dep_fnc) -> order

iterative topological sort using a heap-allocated DFS stack
```

### type-to-size

```code
(type-to-size sym) -> num
```

### ui-merge-props

```code
(ui-merge-props props) -> props
```

### ui-save

```code
(ui-save stream view) -> tree | :nil
```

### ui-symbols

```code
(ui-symbols symbols) -> strs

what a bar's buttons show, as text. A symbol of the symbol font by its
name, +sym_undo, lib/consts/symbols.inc, or by its code, and a str is
itself. A name is in a list that is not run, so it is looked up here
```

### ui-tool-tips

```code
(ui-tool-tips view tips)
```

### unbreak

```code
(unbreak data) -> (lines frozen)

take out the line breaks inside forms, so the layout is then made
from nothing but the code. A line stays a line of its own if it is
blank, a comment, follows a comment, is in a string or the help text
of a command, or starts with a form the source scanners look for.
A frozen line is one that must not then be broken. The doc builder
reads the comments under a definition, and the lines of a key map.
```

### unique

```code
(unique seq) -> seq
```

### unzip

```code
(unzip seq cnt) -> seqs
```

### url-decode

```code
(url-decode str [query_flag]) -> str
```

### url-encode

```code
(url-encode str [query_flag]) -> str
```

### url-format

```code
(url-format u) -> str
```

### url-parse

```code
(url-parse url_str) -> pmap
```

### url-path-query

```code
(url-path-query u) -> str
```

### url-query-format

```code
(url-query-format q) -> str
```

### url-query-parse

```code
(url-query-parse query_str) -> pmap
```

### url-scheme-port

```code
(url-scheme-port scheme) -> num
```

### usort

```code
(usort list [fcmp start end]) -> list
```

### vector-bounds-2d

```code
(vector-bounds-2d paths) -> (min_v2 max_v2)
```

### vector-bounds-3d

```code
(vector-bounds-3d verts [stride]) -> (min_v3 max_v3)
```

### vector-bounds-sphere

```code
(vector-bounds-sphere verts [stride]) -> (center_v3 radius)
```

### vector-point-in-polygon

```code
(vector-point-in-polygon p paths winding_mode) -> :t | :nil
```

### vertex-interp

```code
(vertex-interp isolevel p1 p2 valp1 valp2) -> p
```

### view-fit

```code
(view-fit x y w h) -> (x y w h)
```

### view-locate

```code
(view-locate w h [flag]) -> (x y w h)
```

### within-compile-env

```code
(within-compile-env lambda)
```

### word-parts

```code
(word-parts word font room) -> (part ...)

a word too long for a line, a file path or a link, as the parts that
do fit. It is cut after a / _ - . or ) where it can be, and where a
run between two of those is still too long, after any character
```

### zip

```code
(zip seq ...) -> seq
```

