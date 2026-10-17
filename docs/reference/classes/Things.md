# Things

```image
docs/reference/classes/Things.cwb
```

## Actor

```code
(Things board) -> things

the things of the document, on the stage in front of the canvas, each
picking itself up. Chris: "The shape picks itself up, that's the
insight about oops !" A contact is theirs by their own rule, asked as it
lands, (:hit): one of the hand, or of select mode, on a thing's own
ink, never only in a box round it; one that draws, on what its user
holds, which a drag of it moves, Chris:
"When a shape/group is selected, drag always moves it". One that draws
on a thing no one holds is theirs till it is a tap, which picks the
thing up, Chris: "single tap to select a shape, in any mode"; moved
further it is the canvas's, given it as it was, (. stage :pass), and
draws over the thing. What rubs out is never theirs, it bites through.

A hand that goes down on a thing has hold of that thing, and of no
other, and what another hand does elsewhere is nothing to it. Two
hands that go down on the same thing move, turn and size it between
them
```

### :begin

```code
(. things :begin event) -> state

a pointer has gone down on them
```

### :end

```code
(. things :end event state)

a pointer has come up
```

### :grabbed

```code
(. surface :grabbed id) -> :nil | found

is a pointer one of two that have hold of the same things, (:two_hands)
```

### :held_ids

```code
(. surface :held_ids) -> (id ...)

every item some hand has hold of now
```

### :hit

```code
(. things :hit x y [event]) -> :t | :nil

is the contact theirs, see above. Asked with no contact, a point
that is only over the board, it is not
```

### :holder

```code
(. surface :holder item_id) -> :nil | (id state)

the hand that has hold of an item, the pointer it is and what is
kept for it
```

### :holders

```code
(. surface :holders) -> num

how many pointers have hold of things
```

### :holding

```code
(. surface :holding) -> :nil | id

a pointer that has hold of what is selected
```

### :pointers

```code
(. things :pointers stage events) -> things

two fingers, or two of anything that moves things, down on what is
selected, move it, turn it and size it together. One that may have
been a tap and is moved further is the canvas's, (. stage :pass)
```

### :same_things

```code
(. surface :same_things things things) -> :t | :nil

do two hands have hold of the same things
```

### :step

```code
(. things :step event state) -> state

a pointer that is down has moved: what it holds goes with it. With
snap, the top left of the box round it goes to the grid, not the
pointer, so things line up with it and each other
```

### :swapped

```code
(. surface :swapped swaps) -> surface

things have been made other things, ((id (shape ...)) ...), a
bite out of each. A hand that had hold of one has hold of what
stands in its place, from where things are now: it goes on moving
them, and two hands on them go on turning them, with no jump. If
nothing is left of what it held it holds nothing, and does nothing
```

### :tap?

```code
(. things :tap? state x y) -> :t | :nil
```

### :things

```code
(. surface :things) -> ((item matrix) ...)

what is selected, each with where it is now, to be moved from there
```

### :two_hands

```code
(. surface :two_hands)

if two pointers have hold of the same things, the two of them
move them, turn them and size them, by where they were when the
second took hold and where they are now. While they do, neither
moves them alone. When one lets go the other carries on from where
things are. Hands that have hold of other things go on as they were
```

