# One Farm for the CPU and the GPU

An app that draws a picture, or works out a lot of numbers, can do it on the
nodes of the CPU or on the GPU, and has a button to say which. There is one
way to do that, and this is it. Sections 1 to 3 are the model, section 4 the
kinds of job, and section 5 the library that has it all, which an app holds:
no app writes a farm of its own. Eleven apps are on it: Surface, Raymarch,
Eyes, Mandelbrot, Digits, Canvas, Bubbles, Opcodes, Mesh, Flight and the
Network Map.

What it stands on is in other documents, and they are worth having read:

* `task_farming.md`, the farm classes, and `lib/task/jobs.inc`, the queue of
  jobs on top of them.

* `shader_language.md`, the shaders and kernels the children run, and their
  back ends.

* `neural_networks.md`, for what Digits farms.

## 1. The one model

The work is cut into jobs. A job is a message. A farm of child tasks does the
jobs, each child drawing its part straight onto the app's pixels, and sends
back a short answer. The app counts the answers, and when the last is in the
frame is whole.

* **On the CPU it is a farm of many.** A child on each node of the machine,
  each given a part of the frame: a tile, a strip, a square.

* **On the GPU it is a farm of one.** One child, given the whole frame as its
  one job. It has the GPU work it out, a pixel a thread, and reads the result
  onto the same pixels.

Nothing else differs. It is the same `Jobs` object, the same job message, the
same answer, the same two mailboxes, the same timeout and the same restart of
a child that has gone. The button changes which child the farm is made of and
how many jobs a frame is cut into, and that is all it changes:

```vdu
(setq jobs (if (eql mode :gpu)
	(Jobs +shader_shade_child
		(elem-get select +select_task) (elem-get select +select_reply) 1 :t)
	(Jobs +shader_tile_child
		(elem-get select +select_task) (elem-get select +select_reply) (list +max_int 1 1) :t)))
```

That is from `apps/demos/raymarch/app.lisp`. The first is one child. The
second is a herd, a child for each node of this machine. Both are given `:t`
for `away`, of which more below.

Why only the one way:

* **The GPU is not a special place.** A child has the GPU of its own node,
  `lib/gpu/compute.inc`, not the GUI's. So the GPU's child can be on any node
  of the machine, as a CPU child can, and the app's own task never waits on a
  GPU.

* **An app is written once.** Its loop reads a task mailbox and a reply
  mailbox and does not know which kind of child is answering. A host with no
  GPU, or a shader the GPU can not take, is a child that answers "I could
  not", and the app makes the other farm.

* **What is true of a farm is true of both.** A child that dies is started
  again and its job given out again. A child that is slow is not waited for
  for ever. Neither was written twice.

## 2. The life of a farm

A farm lasts while its work goes on, and no longer.

### It is made when work starts

`(Jobs path task_mbox reply_mbox [size away])` starts the children at once.
`path` is the child's file. `size` says how many:

* A number is that many children, on any of the nodes. `1` is the GPU's.

* A list, `(herd_max [herd_init herd_growth])`, is a herd on the nodes of
  this machine only, those that can reach its shared memory. `(list +max_int
  1 1)` is a child a node.

With `away` the children are kept off the app's own node, if the machine has
another. A child's job is one long call of native code with no task switch in
it, and on the app's node that would hold up the app itself, and the GUI if
it is there too.

An `away` of `:here` is the other way about, the children are kept on the
app's own node. It is for a GPU child that draws triangles: its work is a
frame handed to the GPU and a wait that gives way, and on the node that has
the GUI it can draw where the frame is shown, see below.

An app keeps its farm in a name that is `:nil` when there is none, and makes
it when the first job is to go:

```vdu
(defun farm-start ()
	(unless jobs
		(setq jobs (Jobs ...))))
```

### Jobs go in, answers come out

`(. jobs :add (list job ...))` puts jobs on the queue, and every child with
nothing to do is given one. The app's loop passes on what comes to its two
mailboxes:

* `(. jobs :launched msg)`, a child has started, and is given a job.

* `(. jobs :answered msg)`, a job is done. It gives how many are still out,
  and that child its next job. At 0 the frame is whole. It gives `:nil` for
  an answer that is to be left alone, from a child that has since been
  started again.

A job starts with a `+job`, the key of its child and where the answer goes,
which the farm fills in. An answer starts with a `+job_reply`, that key. What
follows each is the app's own.

### A child that has gone, or is too long

A timer in the app's loop calls `(. jobs :refresh timeout)`. Any child that
has died, or has had its job longer than the timeout, is started again, and
its job goes back on the queue. The timeout is patience, not a rate: a tile
is quick and 5 seconds is plenty, the GPU's first frame waits for its kernel
to be built and is given 30. A child given its job again is given twice as
long, so a patience too short is a try or two, never a loop.

The GPU's first frame is waited for a second, `:gpu_warm`. If it is still
out then the GPU's shader is not ready, a driver may take 20 seconds to
build one the first time, on the Pi. The GPU's farm is set aside on
mailboxes of its own and goes on with that, and the frames are the CPU's,
the farm or here, as in CPU mode. When the GPU answers the frame it was
given it is the farm again, and the CPU's is let go of; one that says it
has no GPU leaves the frames the CPU's. `Render-shade` and `Render-tris` do
it, `Render-work`'s answers are the app's and it does not.

The other way, from the GPU to the CPU, the CPU's farm is made and its
children have to build their shaders and fetch the meshes, seconds, and
till then the frames were drawn here, in the app's own task, one node and
slow. `Render-tris` has the GPU's farm, if it has drawn, set aside on the
same mailboxes as above and go on drawing them till the CPU's children say
they are ready, then let go of, `:standbys`. Its frame is `:standby_out`,
not `:out`, which is the CPU's farm's alone: their word that they are ready
comes to the same mailbox as their answers, and is told from them by there
being none of theirs out. Back to the GPU before then and it is the farm
again at once, its child warm. Chris, 2026-11-24, of Flight: "That is much
better switching ! Seams allmost faster". A mailbox set aside is the same
mailbox: the farm and its children are not touched, only the slot of the
render's list it is read from.

A child goes of its own accord when it has had nothing to do for a while, 5
seconds in the library's children, `(job-child timeout fn)`. It says so as it
goes, and a job that was sent to it as it went is put back on the queue, not
lost.

Two more words a child can say, for the app to pass on:

* `(. jobs :cannot msg)`, this child can not do the work where it is. Its
  job goes back, its node is not asked again while the farm lasts, and
  another node gets the child.

* `(. jobs :failed msg)`, the job went wrong. It is not given out again, and
  its child is started again.

### It is let go of when work stops

```vdu
(defun farm-close ()
	(setq jobs (jobs-end jobs select +select_task +select_reply)))
```

`(jobs-end jobs select task_idx reply_idx)` tells every child to go, and
puts a new mailbox in each of the two places of `select` the farm had. It
gives `:nil`, for the name. An app calls it:

* when the other farm is to take over, the button was pressed, or the GPU's
  child said it could not;

* when the work has stopped, the picture stands still, the film is made, the
  training is paused;

* as it closes.

It is not kept over a pause. A child with nothing to do goes by itself, and
there is no telling which have, so a farm kept idle is a farm half gone. The
next work makes a new one.

### Nothing late comes in

A mailbox that has been let go of takes no mail. So after `jobs-end` an
answer to the old farm goes nowhere, and the app has nothing to look at to
see whether an answer is stale: if it arrived, it is of this farm.

The same is used within a farm's life. `(. jobs :reply_to new_mbox)` empties
the queue and has answers come to a new mailbox from now on, the children
left as they are. Mandelbrot does it when the palette is changed, or the view
sent home, under a picture that is still out: what was asked for is no longer
wanted, and is never heard.

### An answer drives the next job

The next frame is started when the last answer of this one comes in, in the
reply branch of the loop, not on the next tick of a timer:

```vdu
(when (= out 0)
	(. *canvas* :swap +swap_write)
	(start-frame))
```

A frame that takes 2 ms is followed at once by the next, and one that takes
200 ms is not asked for again while it is still out. The timer is for the
farm's patience, for showing what there is so far of a slow frame, and for
the app's own clock. It does not pace the work.

## 3. Where the pixels go

A child does not send its pixels back. It draws them where they are to be.

### The app's canvas is in shared memory

```vdu
(defq shared_canvas (canvas-shared +width +height 1))
...
(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1)))
```

`(canvas-shared width height scale)` is a canvas whose pixels are in memory
that every node of the machine can reach, `:nil` if the host has none.
`(canvas-key canvas)` is the key of them, 0 if they are the canvas's own. The
key goes in every job.

### A child attaches by the key

```vdu
(defq held (list))
...
(defq pixmap (pixmap-attach held width height canvas_key))
```

`(pixmap-attach held width height key)`, and `(canvas-attach held width
height scale key)` for a child that draws with the canvas's own calls, find
the pixels the once and keep them in `held`, a list of the child's. They give
`:nil` if the key is 0 or the pixels can not be reached, from another machine
say.

A CPU child then draws its part of the frame straight onto them, and its
answer is a few bytes: the job's key, and what it drew.

### The GPU's child reads its frame onto them

The GPU works the frame out into a buffer of its own, and the child reads
that back. Given the pixmap's block, the read goes straight into the shared
pixels, with no str made and no copy after:

```vdu
(defq block (pixmap-buf pixmap))
(gpu-read out_buf 0 (* n +int_size) block (* y row))
```

That is from `lib/gpu/shade_child.lisp`. So a GPU frame costs one copy, from
the GPU's memory to the pixels, and then the app shows them.

### Or the GPU draws where the frame is shown

A frame that is read back is copied from the GPU, and copied to it again to
be shown. On the node that has the GUI neither is needed: the GPU of that
node is the GUI's own, so a child there can draw straight into a texture the
GUI shows. The child of a frame of triangles does, `lib/gpu/tris.inc`, and
the child of a pixel shader, `lib/gpu/shade.inc`, when its tile is all of the
frame. The frame names the texture, `(canvas-target canvas)`, and a child
that is on the node the frame came from draws into it; one that is not, or
is refused, draws to a target of its own, or runs its kernel, and reads back.
It finds which for itself, and its answer says, so an app is written the one
way.

The child answers when the GPU is done with the frame, not when it was
handed over. So the answer that drives the next frame gives a slow GPU no
more than it can do, and the desktop, drawn by the same GPU, is not kept
waiting behind frames that have piled up.

A GPU can not be stopped part way through a draw, as native code can be at
a `(task-slice)`. So a frame of a pixel shader is given to it some rows at
a time, each waited for, and the desktop has its turn between: the place a
GPU frame can be let go of is the edge of a strip. The child times each
strip and sizes the next to about 12ms of the GPU. A fast GPU takes all of
a frame as the one draw; a Raspberry Pi 4 takes Surface's a dozen rows at a
time, and the mouse stays live, where a frame as one draw held the GPU, and
the pointer, for half a second. Nothing of a part frame is seen, the strips
go to the canvas that is not on show.

A strip leaves the rest of the texture as it was, so nothing is cleared
first, and a pixel the shader leaves clear has to be drawn clear: the pair
of such a frame is flat, `(shader-gpu-pair vertex pixel :nil :t)`, and its
pixel shader does not throw a clear pixel away as that of a pair with a
depth buffer does.

The texture drawn into is of a canvas that is not on show. When the frame is
whole the two canvases exchange their textures, `(. canvas :exchange that)`:
what is shown is never a frame part drawn, nor a texture nothing has been
drawn into yet.

### The app shows them

When the last answer is in, `(. *canvas* :swap +swap_write)`. An app whose
CPU frame is slow can swap on a timer as well, to show it filling in.

### When the pixels can not be shared

A child that could not reach the pixels sends them after its answer, and the
app puts them on the canvas. The library's tile job has it built in,
`(shader-tile-show canvas msg)`, and the app calls that on every answer
whether or not there are pixels with it. It is slower, and it is right: a
host with no shared memory still draws.

The strips of triangles have no such way back. A child that can not reach
the pixels says so in its answer, and the app draws the frames in its own
task from then on.

### Pictures a child reads from

A texture is shared the same way, the one copy of it on the machine however
many children read it. `(shader-strip-picture canvas)`, `lib/gpu/tris.inc`,
gives a loaded picture's pixels in shared memory, and `(shader-strip-texture
canvas)` names it for a job by its key and size.

## 4. The kinds of job in use

### A tile of a pixel shader

`lib/gpu/tile.inc` and `lib/gpu/shade.inc`. The job is a tile of the frame,
the path of a shader file, and the block of its inputs:

```vdu
(shader-tile file inputs x y x1 y1 width height shared [target])
```

The CPU's child, `+shader_tile_child`, has the shader as native code and
shades the tile. The GPU's, `+shader_shade_child`, has it as a kernel and
runs it over the tile's pixels, or, given a target on the node that has the
GUI, draws it there, section 3. It is the same job to both, and an app needs
no child of its own. `(Render-shade)` holds it: Surface, Raymarch and Eyes.

A CPU frame is cut into tiles of a few rows each, a GPU frame is the one
tile, all of it.

### A kernel of the app's own

Mandelbrot has two children of its own, `child.lisp` and `gpu.lisp`, because
its work is not a pixel shader: a kernel with a buffer, the palette, and on
the CPU a way of leaving out the inside of the set. The job is its own
structure, `+rect`, a square of the picture and where in the set it is, and
the answer says what was drawn. The model is the same, and its two children
are the place to look for how to write one, `apps/science/mandelbrot/`. The
app holds a `(Render-work)` of them.

### Strips of triangles

`lib/gpu/tris.inc`. The job is the rows of a strip of the frame and a list of
draws, each a mesh by its number, the inputs of the vertex and the pixel
shader, and its textures:

```vdu
(shader-strip vfile pfile ask shared width height y y1 cull draws [target fill_argb])
```

A job does not carry the meshes. A child that has not got one asks the app
for it, the once, at a mailbox the app gives, and the app answers with
`(shader-mesh-send msg verts)`. So a frame is a few small messages however
big the meshes are. Each child has a depth buffer of its own, for just its
rows, and fills its rows first with `fill_argb`, the color the frame is
cleared to, `:clear` of the render. A strip slow to answer is given to
another child, and its own may have drawn it already: filled first, a strip
drawn twice is the same, where a translucent thing was laid over itself.

A draw can have a pair of shader files of its own, for a scene whose
things are not all drawn alike, the last of a draw, `sfiles`. Both children
build each pair the once and keep it. A job with no rows is how an app has its
children fetch their meshes before the first frame, and it draws the frames
itself till they have answered.

The GPU's child is the same job to a farm of one on the app's node,
`+shader_tris_gpu_child`, the whole frame its strip. It has each pair, mesh
and picture on the GPU, and draws where the frame is shown or reads it back,
as above.

An app has none of this in it. It holds a `(Render-tris canvas vfile
pfile)`, `lib/gpu/render_farm.inc`, tells it its meshes, and gives it a
frame; Flight, Mesh and the Network Map do:

```vdu
(defq render (Render-tris *canvas* vfile pfile :t))
(. render :mesh 0 verts)
...
(bind '(idx msg) (. render :read select +select_timer))
(cond
	((not idx))
	((= idx +select_main)
		...
		(. render :mode (if gpu_button :gpu :cpu)))
	(:t ;the timer
		(unless (. render :busy)
			(. render :frame (list (list 0 vvals pvals (list picture)))))
		(. render :tick)
		(. *status* :set_text (. render :how))))
...
(. render :close)
```

`(:read)` reads the render's mail with the app's own and deals with it.
`(:frame)` is a frame, by the GPU, by the nodes, or in the app's own task
when neither can be, and `(:busy)` says while one is out. `(:mode)` is the
button, and gives back what the render can do. `(:how)` is a line of status.
An app whose scene is another library's gives the render that library's
meshes and its own way of drawing a frame here, `:mesh_fn` and `:here_fn`,
and its draws with the blocks already made, as Mesh does.

A window that is sized is told its new size many times a second. An app
does not make a canvas at each: it keeps the size that is wanted, makes a
canvas of it when the next frame is to be given, `(. render :canvas
canvas)`, and puts that on show when the frame of it is whole, the old one
staying till then. Mesh and the Network Map do.

### A batch of numbers

Digits trains a network. Each child of its CPU farm is given some of the
samples of a batch, and sums their gradients into its own part of a block.
Its GPU farm of one is given the whole batch, a step or a check, and runs
the model's kernels on it, `lib/nn/gpu.inc`. The data, the model's numbers
and the gradients are in shared memory, a `:buf` each by its key, as pixels
are for a picture, so a job is small and an answer is small.
`neural_networks.md` has it. Its farms are a `(Render-work)`'s with no
canvas, the work is not a picture.

### A picture by a farm, with no GPU side

Canvas, Bubbles and Opcodes draw by a farm of CPU children or in their own
task, to show the one against the other. They have the farm's life, the
shared canvas and the answer that drives the next frame, and no GPU child:
a `(Render-work)` with the one child each.

## 5. The library an app holds

`lib/gpu/render_farm.inc`. An app writes none of sections 2 and 3. It holds
a render, which has the farm and its life, the mailboxes, the way the pixels
go, and what is done when a child can not do its work. There are three
kinds, and what they share is the class `Render`:

* `(. render :read select before)` reads the render's mail with the app's
  own. It gives `(idx msg)`, the app's mailbox and its mail, or `:nil` for
  `idx` when the mail was the render's, and is dealt with. The render's
  mailboxes are read as if they were in `select` before the one of index
  `before`: an app names its first timer there, so a timer that is always
  due does not leave them unread.

* `(. render :busy)` is `:t` while a frame is out. No more is given till it
  is not.

* `(. render :mode [mode])` is how frames are drawn, `:gpu` or `:cpu`, and
  with a mode asks for it. It gives back what the render has, and the button
  is set from that: a node with no GPU stays on the CPU, and so does one
  whose child could not have the shader.

* `(. render :tick)`, from the app's timer, starts again a child that has
  gone or is too long, and lets the farm go when no frame has been given for
  a while.

* `(. render :canvas canvas)` is another canvas from the next frame, one of
  another size. `(. render :drop)` is the frame that is out not waited for,
  the view it was of has gone. `(. render :stop)` lets the farm go now, and
  `(. render :close)` is for the end.

* `(. render :how)` is a line of status: what drew the last frame, and how
  long it took.

### A pixel shader over a frame

`(Render-shade canvas file [lines])`. A frame is the inputs of the shader, a
list of `(name value)`. All of an app:

```vdu
(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/gpu/render_farm.inc")

(enums +event +event_user
	(enum mode))

(enums +select 0
	(enum main timer))

(defq +shader_file (cat *app_root* "my.shader") +width 600 +height 600
	+timer_rate (/ 1000000 60) select :nil render :nil given 0
	shared_canvas (canvas-shared +width +height 1))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Mine" (+sym_close) +event_close)
	(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body)) :connect +event_mode)
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1))))

(defun next-frame ()
	(. render :frame (frame-values (setq given (pii-time)))))

(defun mode-show ()
	(. *mode* :set_selected (if (eql (. render :mode) :gpu) 1 0)))

(defun main ()
	(setq select (task-mboxes +select_size) render (Render-shade *canvas* +shader_file))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	(mode-show)
	(next-frame)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(defq id :t)
	(while id
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;the render's mail: a frame that took a tick or more drives the next
				(and (not (. render :busy)) (>= (- (pii-time) given) +timer_rate) (next-frame)))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_mode)
						(. render :mode (if (= (. *mode* :get_selected) 1) :gpu :cpu))
						(mode-show))
					((. *window* :event msg))))
			(:t ;the timer, a frame a tick if the last is drawn
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(unless (. render :busy) (next-frame))
				(. render :tick))))
	(. render :close)
	(. *window* :close))
```

A frame of the nodes is shown when it is whole. `(def render :partial :t)`
shows it as far as it has got, twice a second, for a picture that stands
still while it fills in, as a frame of Raymarch's film does; of a picture
that moves that would be two frames mixed.

`(frame-values)` is the app's own, the inputs of its shader for this frame.
`apps/demos/surface/app.lisp` is the whole of one.

What to see in it:

* There is no farm in it, no child, and no mailbox but the app's own two.
  The app never touches the GPU, and does not ask if there is one.

* A frame is given from two places: the timer, a frame a tick, and the
  render's mail, when a frame that took longer than a tick is whole. An
  answer drives the next, nothing waits for a tick.

* The button asks, and is set from what the render says.

Two properties change how it draws. `(def render :direct :nil)` has every
frame's pixels on the canvas, read back, the same to the bit whatever drew
them, for an app that keeps them: Raymarch writes a film. `(def render :farm
:nil)` has a frame the GPU does not draw drawn in the app's own task, for a
small picture drawn now and then that is not worth a child a node: Eyes.
`(:frame)` then gives `:t`, the frame is whole and shown, where it gives
`:nil` for one that is out.

### Triangles

`(Render-tris canvas vfile pfile [cull])`, section 4 has its use: the meshes
told to it, a frame a list of draws. Flight is the whole of one, a draw a
pair of its own, Mesh one whose scene is another library's.

A draw is `(id vvals pvals [pictures [vfile pfile]])`, the last its own
pair if it is not drawn with the render's, its inputs packed by that pair's
shaders, by the GPU, the farm and here alike, each pair read the once,
`(:pair)`. A mesh drawn by another vertex shader is told with it, `(. render
:mesh id verts vfile)`, as its vertices are counted by that shader's attrs.
Flight's frame is the land by the render's pair, and a sky, two triangles
of corners by a pair of its own at the back of the depth, given after what
hides it, so it is worked out only where it is seen.

Draws one after another that have the very same lists of inputs, and pair,
as the one before are packed the once: a thing in many meshes, the tiles of
Flight's land, of which only those in sight are drawn, is given the one
`vvals` and the one `pvals` for all of them. A draw is not free, though, the
child gives the GPU each by itself: Flight's coarse land is the one mesh.

An app gives its next frame when the last comes back, `(:read)` with no
`idx`, if that took a tick of its timer or more, and leaves a faster one to
the tick: one that gives a frame only on a tick draws every other tick when
a frame takes a little over one.

### Children of the app's own

`(Render-work canvas child [gpu_child])`, when the library's children do not
fit. The farm, its life and its mail are the render's still; what a frame is
the app says, in properties:

```vdu
(setq render (Render-work *canvas* (cat *app_root* "child.lisp")))
(def render :jobs_fn (const frame-jobs) :here_fn (const frame-here)
	:reply_fn (const part-done))
```

* `:jobs_fn`, `(lambda (kind count ready))`, gives the jobs of a frame for a
  farm of that kind, `:cpu` or `:gpu`, of `count` children. With `ready` it
  gives the jobs that ask the children if they are ready, each a job of
  nothing, or `:nil` if they need not be asked. Till they have all answered
  the frames are drawn in the app's task.

* `:here_fn`, `(lambda (canvas))`, draws a frame in the app's own task. The
  render shows it.

* `:reply_fn`, `(lambda (msg))`, is given each answer of a frame, for what
  the app keeps of it.

* `:cannot_fn`, `(lambda (msg))`, is asked of an answer before it is
  counted, and says `:t` if the child could not do its job where it is. The
  GPU's child has no GPU after all, and the CPU has the frames from there;
  or a child of the CPU's could not, and its job goes to another node.

* `:launched_fn`, `(lambda (kind msg))`, is told of each child as it starts.

* `:herd` is how many children the nodes have, as `(Jobs)` takes it, `:away`
  keeps them off the app's node, `:send_back` is children that send the
  pixels they could not reach, `:patience` and `:gpu_patience` how long a
  child is waited for, and `:idle` how long with no frame before the farm is
  let go of, `:nil` for an app that does that itself, with `(:stop)`.

The modes are `:cpu`, `:gpu` if there is a `gpu_child`, and `:here`, the
app's own task. `(. render :frame)` gives `:t` for a frame drawn here, whole
and shown, and `:nil` for one that is out. Canvas is the least of one,
Mandelbrot one with a GPU child, a child that can not, and a view that
moves under a frame that is out, `(:drop)`.

Work that is not a picture has no canvas, `(Render-work :nil child
gpu_child)`, and gives its jobs themselves, `(. render :frame work)`:
nothing is shown, and `(:busy)` says when all are answered. Digits does.

A function given to a render runs inside the render's method, and names are
found where a function is called from. So it must not `(setq)` a name the
method has for itself, `out`, `jobs`, `gpu`, `fn`, `mode`, `farm`: it would
set the method's. Give it names of the app's own, or have it only keep the
answer, and look at it after `(:read)`, as Digits does.

### A child

A child of the app's own, when the library's two do not fit, is a file with a
`main` that is all `(job-child timeout fn)`:

```vdu
(import "gui/pixmap/lisp.inc")
(import "lib/task/jobs.inc")

;the pixels of the app's canvas, (pixmap-attach)
(defq held (list))

(defun work (msg mbox)
	;the job, done, and its answer, which starts with the job's key
	(defq pixmap (pixmap-attach held (getf msg +my_job_width) (getf msg +my_job_height)
			(getf msg +my_job_shared)))
	...
	(mail-send (getf msg +job_reply)
		(setf-> (str-alloc +my_reply_size)
			(+job_reply_key (getf msg +job_key))
			(+my_reply_drawn (if pixmap 1 0)))))

(defun main ()
	(job-child 5000000 work))
```

Its job and its answer are structures of the app's, that start where the
farm's end:

```vdu
(structure +my_job +job_size
	(uint x y x1 y1 width height)
	(long shared))

(structure +my_reply +job_reply_size
	(uint drawn))
```

`lib/gpu/tile_child.lisp` and `lib/gpu/shade_child.lisp` are the two to read,
a CPU child and a GPU child of the same job.

### The farm by hand

Sections 2 and 3 are what the library does, and what an app that can not use
it would have to do: a `Jobs` of each kind, `(jobs-end)` at every change of
farm, a new reply mailbox for what is no longer wanted, the next frame from
the answer, the canvas in shared memory. `lib/gpu/render_farm.inc` is the
place to read it done, each kind's `(:frame)` and `(:mail)`.
