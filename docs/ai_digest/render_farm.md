# One Farm for the CPU and the GPU

An app that draws a picture, or works out a lot of numbers, can do it on the
nodes of the CPU or on the GPU, and has a button to say which. There is one
way to write that, and this is it. Ten apps follow it, all or part of it,
and section 4 says which part: Surface, Raymarch, Mandelbrot, Digits, Canvas,
Bubbles, Opcodes, Planets, Mesh and Model.

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
to be built and is given 30.

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
GUI shows. The child of a frame of triangles does, `lib/gpu/tris.inc`. The
frame names the texture, `(canvas-target canvas)`, and a child that is on
the node the frame came from draws into it; one that is not, or is refused,
draws to a target of its own and reads back. It finds which for itself, and
its answer says, so an app is written the one way.

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
(shader-tile file inputs x y x1 y1 width height shared)
```

The CPU's child, `+shader_tile_child`, has the shader as native code and
shades the tile. The GPU's, `+shader_shade_child`, has it as a kernel and
runs it over the tile's pixels. It is the same job to both, and an app needs
no child of its own: Surface and Raymarch have none.

A CPU frame is cut into tiles of a few rows each, a GPU frame is the one
tile, all of it.

### A kernel of the app's own

Mandelbrot has two children of its own, `child.lisp` and `gpu.lisp`, because
its work is not a pixel shader: a kernel with a buffer, the palette, and on
the CPU a way of leaving out the inside of the set. The job is its own
structure, `+rect`, a square of the picture and where in the set it is, and
the answer says what was drawn. The model is the same, and its two children
are the place to look for how to write one, `apps/science/mandelbrot/`.

### Strips of triangles

`lib/gpu/tris.inc`. The job is the rows of a strip of the frame and a list of
draws, each a mesh by its number, the inputs of the vertex and the pixel
shader, and its textures:

```vdu
(shader-strip vfile pfile ask shared width height y y1 cull draws)
```

A job does not carry the meshes. A child that has not got one asks the app
for it, the once, at a mailbox the app gives, and the app answers with
`(shader-mesh-send msg verts)`. So a frame is a few small messages however
big the meshes are. Each child has a depth buffer of its own, for just its
rows.

A draw can have a pair of shader files of its own, for a scene whose
things are not all drawn alike. A job with no rows is how an app has its
children fetch their meshes before the first frame, and it draws the frames
itself till they have answered.

The GPU's child is the same job to a farm of one on the app's node,
`+shader_tris_gpu_child`, the whole frame its strip. It has each pair, mesh
and picture on the GPU, and draws where the frame is shown or reads it back,
as above.

An app has none of this in it. It holds a `(Render-tris canvas vfile
pfile)`, `lib/gpu/render_farm.inc`, tells it its meshes, and gives it a
frame; Planets, Model, Mesh and the Network Map do:

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
`neural_networks.md` has it.

### A picture by a farm, with no GPU side

Canvas, Bubbles and Opcodes draw by a farm of CPU children or in their own
task, to show the one against the other. They have the farm's life, the
shared canvas and the answer that drives the next frame, and no GPU child.

## 5. A worked skeleton

Cut down from `apps/demos/raymarch/app.lisp`, with the button of
`apps/science/mandelbrot/app.lisp`. Read those two for the whole of it.

What the app has:

```vdu
(import "lib/gpu/shader.inc")
(import "lib/gpu/compute.inc")
(import "lib/gpu/shade.inc")

(enums +event +event_user
	(enum mode))

(enums +select 0
	(enum main task reply timer))

(defq +shader_file (cat *app_root* "my.shader")
	+width 600 +height 600 +line_batch 8
	+timer_rate (/ 1000000 60) +slow_ticks 30 ticks 0
	;a tile is quick, the GPU's first frame waits for its kernel
	+retry_timeout (task-timeout 5) +gpu_timeout (task-timeout 30)
	program (shader-load +shader_file) select :nil jobs :nil
	;there is a GPU, and it is wanted
	has_gpu (> (gpu-format) 0) mode :cpu
	shared_canvas (canvas-shared +width +height 1))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Mine" (+sym_close) +event_close)
	(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body)) :connect +event_mode)
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1))))
```

The farm, made and let go of:

```vdu
(defun farm-close ()
	(setq jobs (jobs-end jobs select +select_task +select_reply)))

(defun farm-start ()
	(unless jobs
		(setq jobs (if (eql mode :gpu)
			(Jobs +shader_shade_child
				(elem-get select +select_task) (elem-get select +select_reply) 1 :t)
			(Jobs +shader_tile_child
				(elem-get select +select_task) (elem-get select +select_reply) (list +max_int 1 1) :t)))))
```

A frame, the one tile to the GPU's child or a tile to each of the CPU's:

```vdu
(defun start-frame ()
	(farm-start)
	(defq inputs (shader-pack program (frame-values)) key (canvas-key *canvas*))
	(. jobs :add (if (eql mode :gpu)
		(list (shader-tile +shader_file inputs 0 0 +width +height +width +height key))
		(map! (lambda () (defq y (* (!) +line_batch))
				(shader-tile +shader_file inputs 0 y +width (min +height (+ y +line_batch)) +width +height key))
			'() 0 (const (/ (+ +height +line_batch -1) +line_batch))))))
```

`(frame-values)` is the app's own, the inputs of its shader for this frame,
a list of `(name value)`.

The other farm takes over, from the button or because the GPU's child could
not:

```vdu
(defun to-mode (new_mode)
	(farm-close)
	(setq mode new_mode)
	(. *mode* :set_selected (if (eql mode :gpu) 1 0))
	(start-frame))
```

The loop:

```vdu
(defun main ()
	(setq select (task-mboxes +select_size) mode (if has_gpu :gpu :cpu))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *mode* :set_selected (if (eql mode :gpu) 1 0))
	(. *window* :open)
	(start-frame)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(defq id :t)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_mode)
						;the other farm, if the host has it
						(defq want (if (and has_gpu (= (. *mode* :get_selected) 1)) :gpu :cpu))
						(if (eql want mode) (. *mode* :set_selected (if (eql mode :gpu) 1 0))
							(to-mode want)))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a tile is shaded, one of an old farm never comes
				(when (defq out (. jobs :answered msg))
					(cond
						((nql mode :gpu)
							;a child that could not reach the canvas sends the pixels
							(shader-tile-show *canvas* msg)
							(when (= out 0)
								(. *canvas* :swap +swap_write)
								(start-frame)))
						((= (getf msg +shade_reply_drawn) 0)
							;no GPU after all, the CPU's farm takes over
							(setq has_gpu :nil)
							(to-mode :cpu))
						(:t ;the frame is on the pixels, the next is started
							(. *canvas* :swap +swap_write)
							(start-frame)))))
			(:t ;the timer, the patience of the farm
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(when (and jobs (= (setq ticks (% (inc ticks) +slow_ticks)) 0))
					(if (eql mode :cpu) (. *canvas* :swap +swap_write))
					(. jobs :refresh (if (eql mode :gpu) +gpu_timeout +retry_timeout))))))
	(farm-close)
	(. *window* :close))
```

What to see in it:

* There is one `jobs`, one task mailbox and one reply mailbox. The mode is
  looked at in three places only: which child, how the frame is cut, and what
  an answer of its kind says.

* `(start-frame)` is called from the reply branch. Nothing waits for a tick.

* `(farm-close)` before every change of farm, and at the end. After it, no
  answer of the old farm can arrive.

* The app never touches the GPU. `(gpu-format)` is asked once, to know if the
  button means anything.

* This app draws for ever. One that can stand still, a picture that is done
  till the view moves, lets its farm go when it has, as Mandelbrot does two
  seconds after its last picture, and `(farm-start)` makes the next.

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
