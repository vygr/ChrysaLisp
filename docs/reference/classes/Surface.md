# Surface

## Actor

```code
(Surface board) -> surface

the actor at the back, every point is on it

A pointer belongs to what it went down on till it comes up, and to
nothing else. The stage has that for the actors, a ruler, a palette.
Here it is so for the things of the document: a hand that goes down
on a thing has hold of that thing, and of no other, and what another
hand does elsewhere is nothing to it. One draws, two more size a
thing, somebody across the board moves another, and none of them is
in the way of the rest. Two hands that go down on the same thing
move, turn and size it between them. A box dragged out on nothing is
its pointer's own. And a finger that goes down somewhere puts away
nobody's palette, nor lets go of what somebody else has hold of
```

### :begin

```code
(. surface :begin event) -> state

a pointer has gone down
```

### :end

```code
(. surface :end event state)

a pointer has come up
```

### :grabbed

```code
(. surface :grabbed id) -> :nil | found

is a pointer one of two that have hold of the same things, (:two_hands)
```

### :hit

### :holder

```code
(. surface :holder item_id) -> :nil | state

the hand that has hold of an item, what is kept for it
```

### :holding

```code
(. surface :holding) -> :nil | id

a pointer that has hold of what is selected
```

### :pointers

```code
(. surface :pointers stage events) -> surface
```

### :same_things

```code
(. surface :same_things things things) -> :t | :nil

do two hands have hold of the same things
```

### :shape_d

```code
(. surface :shape_d state x y) -> d

the path of what a pointer is drawing, now it is at a point
```

### :step

```code
(. surface :step event state) -> state

a pointer that is down has moved, or come up
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

