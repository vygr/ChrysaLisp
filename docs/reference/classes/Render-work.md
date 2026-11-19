# Render-work

```image
docs/reference/classes/Render-work.cwb
```

## Render

```code
(Render-work canvas child [gpu_child]) -> render

frames by children of the app's own, with jobs of the app's own: a
farm of that child on the nodes of this machine, a part of the frame
each, onto the canvas's pixels, which are shared, (canvas-shared); or
a farm of one of gpu_child, if the app has one, given the frame as the
GPU is to have it; or here, in the app's own task. The modes are :cpu,
:gpu and :here. What a frame is the app says, in properties:
:jobs_fn (lambda (kind count ready)) the jobs of a frame for a farm of
that kind, :cpu or :gpu, of count children, or with ready the jobs
that ask its children if they are ready, :nil if they need not be
asked; :here_fn (lambda (canvas)) the frame drawn here, not shown;
:reply_fn (lambda (msg)) an answer of a frame, what the app keeps of
it, and :none from it is a child that could not do the work where it
is; :cannot_fn (lambda (msg)) asked of an answer before it is counted,
:t if the child could not do its job where it is: the GPU's child has
no GPU after all, and the CPU has the frames, or a child of the CPU's
could not, and its job goes to another node, its own is left out
while the farm lasts. :herd is how many children the nodes have, as
(Jobs) takes it, and :away keeps them off this node. :send_back is
children that send the pixels of a part they could not reach, which
the :reply_fn puts on the canvas, so the nodes draw a frame with the
pixels not shared too. :launched_fn (lambda (kind msg)) is told of
each child as it starts, the first, or one in place of one that went.
With no canvas, :nil, the work is not a picture: nothing is shown, the
jobs are given to (:frame), and (:busy) says when all are answered
```

### :frame

```code
(. render :frame [work]) -> :nil | :t

a frame of the moment, its jobs asked of the :jobs_fn, or work, the
jobs themselves. :t if it is whole and shown, it was drawn here:
the mode is :here, the pixels are not shared, or the children are
not yet ready. :nil if it is out with a farm, (:busy) says when it
no longer is; no more is given till then
```

### :here

```code
(. render :here) -> :t

a frame drawn in this task, and shown
```

### :mail

```code
(. render :mail idx msg) -> :nil | :t

what came to mailbox idx of the render's. :t if a frame that was
out is now whole and shown, the app may give the next
```

