# Board

```image
docs/reference/classes/Board.cwb
```

```code
(Board [doc]) -> board
```

### :add

```code
(. board :add item) -> id

as (:commit), and the id of it, for a script
```

### :align

```code
(. board :align how) -> board

line up what is selected, each with the box round all of it. how is
:left :right :top :bottom, or :hcenter or :vcenter for the middles
```

### :angle_step

```code
(. board :angle_step [id]) -> step
```

### :appoint

```code
(. board :appoint event) -> board

the device of this pointer is the teacher's from now on, a pack of
pens has one that is the teacher's. Who may do it is the caller's
to say, only a device that is the teacher's already
```

### :back?

```code
(. board :back? id) -> :t | :nil

is it in flight behind the document, or was it as it landed
```

### :bite

```code
(. board :bite points [r shape user]) -> :t | :nil

what an eraser covers as it goes along a line, x y x y ..., taken
out of every thing it touches, each of which is then flat: a
filled outline of what was left of it. The eraser is a path about
0 0 with no dent in it, default a circle of r, default the
board's reach by its zoom. No step is made. Was anything touched
```

### :busy?

```code
(. board :busy?) -> :t | :nil

is anybody drawing: a line not yet done, along an instrument or
free, or the eraser going. Many work at a board at once, and what
is done to all of it, a move of the view, waits for that. Chris:
"lock the view while anyone is drawing, that onlt applie to the
global actions !" It does not wait for a hand that holds a thing:
"If I hold a shape, and somone else moves the canvas, my shape
comes with me ! It is 'in flight'", what shows the board sees to
that
```

### :changed

```code
(. board :changed ids) -> board

these items of the layers are not what they were, or are new, or
:all for anything might be. It is kept till it is asked for, for
what keeps a copy of the document in step with this one, the nodes
that draw it in stripes, lib/cwb/stripes.inc. An item that is gone
need not be said, the layers are told whole
```

### :clear

```code
(. board :clear) -> board

take out everything
```

### :commit

```code
(. board :commit item [user]) -> item

put an item on the layer that is drawn on, as a step that can be
undone, made by that user, the teacher's if not said
```

### :crop

```code
(. board :crop) -> num

take out every item that is nowhere on the paper, the document's
width and height from its top left, as a step that can be undone.
One that is partly on it stays, all of it. Those in a layer that
is locked stay. How many went
```

### :delete

```code
(. board :delete) -> board

take out what is selected
```

### :dirty?

```code
(. board :dirty? flags) -> :t | :nil

has that changed, and it is taken as drawn
```

### :dismiss

```code
(. board :dismiss event) -> :t | :nil

a pointer has gone down on the surface, what is only there till
then goes. Did any
```

### :draw

```code
(. board :draw canvas [m clip]) -> count

the document, on a canvas, by the matrix it is seen by
```

### :draw_actors

```code
(. board :draw_actors canvas [m pick]) -> board

what is on the stage and not of the document, a ruler, a palette,
the back one first. After (:draw_overlay), it is over that. With
pick, a function of an actor, only those it says: an app that has
some of them drawn another way, the Whiteboard its instruments
```

### :draw_flight

```code
(. board :draw_flight canvas [m side]) -> board

what is in flight and nothing else: the items being moved about,
and what is being drawn and not yet kept. Of one side, :front or
:back, if said: what is being drawn is in front
```

### :draw_overlay

```code
(. board :draw_overlay canvas [m no_flight no_selected]) -> board

what is over the document and not of it: what is in flight,
the box being dragged out, and the handles of what is selected.
With no_selected the last is left out, whoever shows the board
draws it where what is selected is, (:draw_selected)
```

### :draw_selected

```code
(. board :draw_selected canvas [m]) -> board

the lines round what is selected, and its handles. They go with
what they are of: a thing taken to the back has its box and its
handles at the back with it, behind what it is behind, and is
still turned and sized by them there
```

### :drop_temp

### :duplicate

```code
(. board :duplicate [dx dy]) -> board

a copy of what is selected, moved a little, default 16 each way,
and the copy is what is selected
```

### :fit

```code
(. board :fit [pad]) -> :nil | (dx dy)

the document is made the size of what is in it, (cwb-fit), as a
step that can be undone, and what is on the stage that has a
place, an instrument, is moved with it. How far it all moved
```

### :float

```code
(. board :float ids [back]) -> board

these items are selected, and so in flight, in front of the
document or with back behind it
```

### :fly

```code
(. board :fly ids back) -> board

these, in flight, are behind the document, or in front of it
```

### :forget

```code
(. board :forget) -> board

nothing was done after all, the last (:snapshot) is dropped
```

### :get_bindings

### :get_doc

### :get_selected

### :get_stage

### :group

```code
(. board :group) -> :nil | id

what is selected becomes one group, where the top one of them was
```

### :hovering

```code
(. board :hovering events) -> board

a pen or a mouse with nothing held is near, not down: its kind
and where it is are kept. If it went down where it is, and was
had by an instrument, held to an edge of it, to draw along that
edge, then the catch of that edge is shown, (:draw_actors), and
not till then. :hover_shown is those, instrument edge ... A
finger is never near, it is down or it is not there
```

### :in_flight?

```code
(. board :in_flight?) -> :t | :nil

is anything in flight: being moved about, or being drawn
```

### :keep_shape?

```code
(. board :keep_shape? [id]) -> :t | :nil
```

### :layer_index

```code
(. board :layer_index) -> index

the layer that is drawn on, the top one unless one was chosen
```

### :new

```code
(. board :new width height) -> board

an empty board of that size, as a step that can be undone: an
hour's work is not lost to one press in the wrong place
```

### :new_shape

```code
(. board :new_shape event tool) -> shape

a shape for a pointer to draw, with the colour and width that
pointer is bound to, or the board's. It is drawn over the
document till it is kept
```

### :order

```code
(. board :order front) -> board

what is selected goes to the front of its layer, or the back
```

### :pointer_free?

```code
(. board :pointer_free? id) -> :t | :nil

is a pointer down on the board on nothing: it has the hand, and
no thing in it. It is dragging a box out, or may be a tap
```

### :pointer_idle

```code
(. board :pointer_idle id) -> board

what a pointer that is down on the board was doing is given up:
its box is not dragged out, and when it comes up it was no tap.
What shows the board says so when it takes two fingers that are
on nothing to move its view by
```

### :pointer_settings

```code
(. board :pointer_settings event) -> (snap angle keep_shape)

the snap step, the angle step and keep shape of a pointer, as it
is bound. Snap on is to the grid, :snap_gap, or the document's
```

### :pointers

```code
(. board :pointers events) -> board

a batch of pointer events, each in the space of the document
where each finger that is down is, is kept, to be shown, (:draw_actors)
```

### :put_state

### :reach

```code
(. board :reach [width]) -> r

how far an eraser reaches, in the document, for a pointer that
draws that wide, default the board's width: three times the
width, so 9, 18 and 36 for the three widths of the app, and not
less than :rub_reach. It is that many pixels on the screen
whatever the zoom, as an instrument is the size it is
```

### :redo

```code
(. board :redo) -> :t | :nil
```

### :remove

```code
(. board :remove ids) -> board

take items out, no step is made, see (:delete)
```

### :resize

```code
(. board :resize width height) -> board

the paper is that size, what is on it stays, and it can be undone
```

### :rub

```code
(. board :rub x y [r lines]) -> :t | :nil

rub out at a point, as the eraser does, r is how far it reaches,
default the board's by its zoom. Of a line that is near, the part
that is within r goes, and what is left of it is lines in its
place, still lines. Anything else it touches, a box, a filled
shape, words, has a bite taken out of it, (:bite). That is
:rub_mode :part, what a board has. With :lines only a line is
cut, and if none was touched the thing on top at the point goes
whole. With :whole a line goes whole too. With :bite a line has a
bite out as the rest have, and is a line no more. With lines :t
only the lines are cut, whatever the mode, (:rub_along) takes the
bite of all its way at once. No step is made. Was anything
rubbed out
```

### :rub_along

```code
(. board :rub_along x y x1 y1 [r]) -> :t | :nil

rub out from one point to another, as the eraser goes, at every
step of half its reach
```

### :select

```code
(. board :select ids) -> board
```

### :select_all

```code
(. board :select_all) -> board
```

### :selected_by

```code
(. board :selected_by user) -> ids

what that user put in select mode by a tap. Chris: an action on a
selection is on "the user's own selections". A thing selected by
no tap, a hand or a box, says no one, and is anyone's
```

### :selected_items

```code
(. board :selected_items) -> items

the items that are selected, each one of a layer's own, in the
order they are in, the back one first
```

### :set_doc

```code
(. board :set_doc doc) -> board

another document, nothing of the last is kept
```

### :snap_step

```code
(. board :snap_step [id]) -> step
```

### :snap_x

### :snap_y

### :snapshot

```code
(. board :snapshot) -> board

what is about to be done can be undone
```

### :state

```code
(. board :state) -> state

the layers as they are, and the size of the paper, to go back to
```

### :style

```code
(. board :style [key val] ...) -> board

set the colour, fill, width and so on of every shape that is
selected, those in groups too
```

### :style_ids

```code
(. board :style_ids ids [key val] ...) -> board

as (:style), of those selected things only
```

### :take_appended

```code
(. board :take_appended) -> items

the items put on top since this was last asked
```

### :take_changes

```code
(. board :take_changes) -> :all | ids

what has changed since this was last asked
```

### :take_damage

```code
(. board :take_damage) -> :all | boxes

what of the document's picture is to be drawn again, since this
was last asked: all of it, or only these boxes of it, which may
be none
```

### :tick

```code
(. board :tick time) -> :t | :nil

the time is now that, microseconds, for what is on the stage that
moves by itself. Is any of it still moving, it is then to be
drawn again
```

### :toggle_select

```code
(. board :toggle_select id user) -> board

a thing in or out of select mode, by a tap. It is that user's
selection, an action on a selection is on a user's own, see
(:selected_by)
```

### :touch

```code
(. board :touch flags) -> board

something has changed that is drawn.

What is selected is in flight: it is not drawn with the document,
it is drawn by itself, (:draw_flight), in front of the document or
behind it, :float_back, and the document is not drawn again while
it is moved, turned or sized, however long that goes on. It lands
when it is no longer selected. So here, where every change comes,
what is in flight is made what is selected, and if that is not
what it was the document is to be drawn again, once

Not all of it need be. When a few things go into flight or come
out of it the picture of the document changes only where they
are: :damage is the box round each, and (:take_damage) gives them
to what draws. Anything else that changes the document is told
here with +board_dirty_doc, and then it is all of it
```

### :touch_order

```code
(. board :touch_order) -> board

the order of things in flight has changed and nothing else. The
picture of the document is as it was, they are not in it, but
what keeps a copy of the document is to be told
```

### :transform

```code
(. board :transform m) -> board

move what is selected by a matrix
```

### :undo

```code
(. board :undo) -> :t | :nil
```

### :ungroup

```code
(. board :ungroup) -> board

each group that is selected becomes its items, where it was, each
where it was on the board
```

### :user_of

```code
(. board :user_of event) -> user

who the device of a pointer is. The mouse is the teacher's, the
presenter's, Chris: "mouse was assumed to be the teacher/presenter".
Touch is anybody, "touch is anybody, we couldn't tell". The eraser
is the teacher's, "the eraser device input was always assumed to be
under the control of the teacher". A pen is a student, each its
own, numbered as it is first seen, unless the teacher has made it
the teacher's, (:appoint), and a pen bound to rub out is that pen's
user. SDL3 gives the other end of a pen as an :eraser, so here that
is the teacher's whoever's pen it is
A board that joined another's is a student's: what is the teacher
here is that board's own number, :user_base, see lib/cwb/share.inc
```

### :written

```code
(. board :written item who began) -> board

a line drawn by hand has just been kept, on top of its layer, by
that pointer, begun at that time. If the last such was ended a
moment before, and is still under it, the two are a group, or it
joins the group the last was put in, if that has not been moved.
It is part of the step of keeping the line, and nothing is seen
to change: they are where they were, in the order they were
```

