# Render

```image
docs/reference/classes/Render.cwb
```

```code
(Render canvas) -> render

what every kind of render has: the canvas, the mode, the farm there is
and its life, and the mail of it. A kind adds what a frame is, (:frame),
and what an answer of its children says, (:mail). It starts on the GPU
if this node has one
```

### :busy

```code
(. render :busy) -> :nil | :t

a frame is out with a farm, or with the GPU that stands in for the
CPU's, the next is not to be given yet
```

### :canvas

```code
(. render :canvas canvas) -> render

another canvas to draw on from the next frame, the app's was made
again, of another size say. A frame that is out for the old one
is not waited for. The farm is kept, its children find the pixels
and the texture a frame is for from the frame itself
```

### :close

```code
(. render :close) -> render

the farm let go of, the app is closing
```

### :drop

```code
(. render :drop) -> render

a frame that is out is not waited for, the view it was of has
gone, and the next may be given at once. The farm is kept
```

### :how

```code
(. render :how) -> str

how the last frame was drawn, and how long it took, for a line
of status. A frame the GPU drew straight into the canvas's texture
has no time said: an app that draws them one after another says
its rate
```

### :mode

```code
(. render :mode [mode]) -> :gpu | :cpu

how frames are drawn, and with a mode, :gpu or :cpu, how they are
to be from the next. The GPU is not to be had on a node with none,
or once its child has said it can not
```

### :read

```code
(. render :read select before) -> (idx msg)

the next mail of the app's mailboxes, select, or of the render's
own, which are read as if they were in select before the one of
that index: an app has its timers last, and puts them after these
too, so a timer that is always due does not leave them unread.
idx is the app's mailbox the mail came to, and msg the mail. :nil
for idx is mail of the render's, dealt with: if a frame was out,
(:busy) says whether it still is
```

### :stop

```code
(. render :stop) -> render

the farm let go of, its children told to go, and what it has out
not waited for: its work has stopped, or is no longer wanted. The
next frame makes a new one, and nothing late comes to it
```

### :tick

```code
(. render :tick) -> render

called from the app's timer: now and then a child that has gone,
or been too long, is started again. And a farm that has had no
frame for a while is let go of, its work has stopped, the next
frame makes a new one
```

