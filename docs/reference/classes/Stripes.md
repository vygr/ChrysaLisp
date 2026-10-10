# Stripes

```code
(Stripes board [herd]) -> stripes

herd is the most children there are to be, default one for each node
of the machine, up to 64. They are kept off the node of the app, if
there is another: a child at work on a big document is a while
between the turns it gives the other tasks of its node
it has three mailboxes, (. stripes :mboxes), for the app to wait on
with its own, and what comes to one of them is given to (:handle).

The children are idle, or being brought in step, :warming, or have a
:frame to draw. :sent is the version they were last sent word of,
and :synced the last they all answered that they have
```

### :busy?

```code
(. stripes :busy?) -> :t | :nil

is a frame out with the children
```

### :cheap?

```code
(. stripes :cheap?) -> :t | :nil

are the children ready, and near enough in step that a frame
would not have each sent the whole document first
```

### :close

```code
(. stripes :close) -> stripes
```

### :frame

```code
(. stripes :frame canvas zoom style) -> :t | :nil

draw the document, all of it, onto a canvas whose pixels are
shared, a stripe for each child. With a style the paper is drawn
behind it, lib/cwb/paper.inc. With :nil there is none, the
shapes are drawn on what is there, which the caller has made
clear, for a board whose paper is a picture of its own. :nil, and nothing is done, if
the children are not ready or the canvas is not shared. When the
last stripe is answered (:handle) says :done
```

### :handle

```code
(. stripes :handle index msg) -> :nil | :warm | :done | :failed

what came to one of the three mailboxes, index says which, 0 1 or
2. :warm when the last child has answered that it is in step,
:done when it was the last stripe of a frame, :failed if it was
and a child could not draw its stripe, the canvas is then not all
drawn
```

### :job

```code
(. stripes :job key width height y y1 zoom back style gap skip) -> job
```

### :keep

```code
(. stripes :keep) -> stripes

called now and then, once a second say. A child that has gone, or
been too long over a stripe, is started again. Children that have
had nothing to do for a while are brought in step, which is also
what tells them not to go
```

### :mboxes

```code
(. stripes :mboxes) -> (task_mbox reply_mbox sync_mbox)
```

### :note

```code
(. stripes :note) -> version

the document has changed, by what the board says has. Called each
time it has, whoever then draws it
```

### :ready?

```code
(. stripes :ready?) -> :t | :nil

are the children up, and idle
```

### :start

```code
(. stripes :start) -> stripes

start the children, on the nodes of this machine, and bring them
in step. Till they all answer (:ready?) is :nil
```

### :warm

```code
(. stripes :warm) -> :t | :nil

bring the children in step with the document as it is, each is
sent a stripe of no rows. :nil if they are not there or not idle.
When the last answers (:handle) says :warm
```

