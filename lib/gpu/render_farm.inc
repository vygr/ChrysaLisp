;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Frames for an app, by the GPU or by the nodes.
;
; An app has a (Render-shade) of its canvas and a pixel shader file, and
; gives it a frame, the inputs of the shader. Or a (Render-tris) of its
; canvas and a pair of shader files, tells it its meshes, and gives it a
; frame, a list of what is drawn. Or a (Render-work) of its canvas and
; children of its own, and says what the jobs of a frame are.
; How the frame is made is this object's: by the GPU, a farm of one on
; the app's own node, which draws straight into the canvas's texture
; where the node has the GUI and reads the frame back where it has not;
; or by the CPU, a strip of the frame by each node of the machine; or
; here, in the app's own task, when neither can be. docs/ai_digest/
; render_farm.md is the model, lib/gpu/tris.inc the job and the children.
;
; The app reads its mail with (. render :read select before), which reads
; the render's own mailboxes with the app's and deals with what comes to
; them, and calls (. render :tick) from its timer.
;
; An app whose scene is another library's has it tell the render what it
; knows, as properties of the render: :mesh_fn, (lambda (id)) the vertices
; of a mesh, a str, asked the first time it is wanted; :here_fn, (lambda
; (canvas draws)) a frame drawn in this task, for when no farm can; and
; :clear, what a frame of the nodes is filled with first. Its draws are
; then as (shader-strip) takes them, the blocks already made.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./vp.inc")
(import "./compute.inc")
(import "./tris.inc")
(import "./shade.inc")

;module
(env-push)

(enums +rt_mbox 0
	(enum task reply ask))

;the patience of a farm: a strip may wait seconds for its node on a busy
;machine, the GPU's first frame waits for its pair to be built. And how
;long with no frame given before a farm is let go of, its work has
;stopped: less than the time a child waits for work before it goes
(defq +cpu_timeout (task-timeout 60) +gpu_timeout (task-timeout 30) +tick_rate 30
	+stale 3000000)

(defun mesh-of (id)
	; (mesh-of id) -> :nil | (verts tris packed)
	;a mesh by its number: its vertices, a doubles or the bytes of one,
	;the triangles of them taken three at a time, and its bytes if they
	;have been made. One the app has not told of is asked of its :mesh_fn
	(defq meshes (get :meshes this))
	(unless (defq m (. meshes :find id))
		(and (defq fn (get :mesh_fn this)) (defq verts (fn id))
			(. meshes :insert id (setq m (list verts
					(reduce! (# (push %0 (!))) '() (nums) 0 (/ (length verts) +long_size (get :stride this)))
					verts)))))
	m)

(defun farm-end ()
	;the farm let go of, its work has stopped
	(defq mboxes (get :mboxes this))
	(def this :jobs (jobs-end (get :jobs this) mboxes +rt_mbox_task +rt_mbox_reply)
		:kind :nil :ready :nil :out :nil))

(defun frame-forget ()
	;a frame that is out is not waited for, and the farm is kept: what its
	;children still answer goes to the mailbox it was asked for with, let
	;go of. But a farm of the CPU that has not yet said it is ready is let
	;go of, that word would be lost too
	(defq jobs (get :jobs this) mboxes (get :mboxes this))
	(cond
		((not jobs))
		((and (eql (get :kind this) :cpu) (not (get :ready this))) (farm-end))
		(:t (elem-set mboxes +rt_mbox_reply (mail-mbox))
			(. jobs :reply_to (elem-get mboxes +rt_mbox_reply))))
	(def this :out :nil))

(defun back ()
	; (back) -> canvas
	;the canvas the GPU draws a frame into, off the screen, the size the
	;app's is shown at: a frame is put on show when it is whole, the two
	;exchange their textures, (whole), so what is shown is never a frame
	;part drawn, nor a texture nothing has been drawn into yet
	(defq canvas (get :canvas this) pixmap (getf canvas +canvas_pixmap 0)
		scale (getf canvas +canvas_scale 0)
		size (list (/ (getf pixmap +pixmap_width 0) scale) (/ (getf pixmap +pixmap_height 0) scale)))
	(unless (and (get :back this) (eql size (get :back_size this)))
		(def this :back (Canvas (first size) (second size) 1) :back_size size))
	(get :back this))

(defun whole (how)
	;a frame is whole, and is shown as it was made: one the GPU drew is on
	;the back canvas, which gives the app's what it has
	(defq canvas (get :canvas this))
	(cond
		((not canvas))
		((= how +strip_texture) (. canvas :exchange (get :back this)))
		((. canvas :swap +swap_write)))
	(def this :out :nil :took (- (pii-time) (get :began this))
		:how (cond ((= how +strip_texture) :gpu) ((eql (get :kind this) :gpu) :gpu_copy) (:t :farm)))
	:t)

(defun strips (draws rows)
	;a job for each child of the CPU's farm, a strip of the frame, or with
	;no rows a job of nothing, which has a child fetch every mesh and
	;picture and say it is ready
	(defq jobs (get :jobs this) count (. jobs :size) pixmap (getf (get :canvas this) +canvas_pixmap 0)
		w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0))
	(. jobs :add (map! (lambda () (shader-strip (get :vfile this) (get :pfile this)
					(elem-get (get :mboxes this) +rt_mbox_ask) (canvas-key (get :canvas this)) w h
					(if rows (/ (* (!) h) count) 0) (if rows (/ (* (inc (!)) h) count) 0)
					(get :cull this) draws)) '() 0 count)))

(defclass Render (canvas) :nil
	; (Render canvas) -> render
	;what every kind of render has: the canvas, the mode, the farm there is
	;and its life, and the mail of it. A kind adds what a frame is, (:frame),
	;and what an answer of its children says, (:mail). It starts on the GPU
	;if this node has one
	(defq has_gpu (> (gpu-format) 0))
	(def this :canvas canvas
		:mboxes (list (mail-mbox) (mail-mbox) (mail-mbox))
		:mode (if has_gpu :gpu :cpu) :no_gpu (not has_gpu) :no_farm :nil
		;the farm there is, of which kind, :gpu or :cpu, its children have
		;said they are ready, a frame is out with it
		:jobs :nil :kind :nil :ready :nil :out :nil :back :nil :back_size :nil
		;what a frame of the nodes is filled with first, how long they are
		;waited for, what a part of a frame is called, and is a frame shown
		;as it fills
		:clear +argb_black :patience +cpu_timeout :part "strip" :partial :nil
		;how long the GPU's child is waited for, and how long with no frame
		;given before a farm is let go of, :nil never, the app does it
		:gpu_patience +gpu_timeout :idle +stale
		:began 0 :took 0 :last 0 :how :here :ticks 0)

	(defmethod :read (select before)
		; (. render :read select before) -> (idx msg)
		;the next mail of the app's mailboxes, select, or of the render's
		;own, which are read as if they were in select before the one of
		;that index: an app has its timers last, and puts them after these
		;too, so a timer that is always due does not leave them unread.
		;idx is the app's mailbox the mail came to, and msg the mail. :nil
		;for idx is mail of the render's, dealt with: if a frame was out,
		;(:busy) says whether it still is
		(defq mboxes (get :mboxes this)
			all (cat (slice select 0 before) mboxes (slice select before -1))
			idx (mail-select all) msg (mail-read (elem-get all idx)))
		(cond
			((< idx before) (list idx msg))
			((< idx (+ before (length mboxes)))
				(. this :mail (- idx before) msg)
				(list :nil msg))
			((list (- idx (length mboxes)) msg))))

	(defmethod :mode (&optional mode)
		; (. render :mode [mode]) -> :gpu | :cpu
		;how frames are drawn, and with a mode, :gpu or :cpu, how they are
		;to be from the next. The GPU is not to be had on a node with none,
		;or once its child has said it can not
		(when (and mode (nql mode (get :mode this)) (or (nql mode :gpu) (not (get :no_gpu this))))
			(farm-end)
			(def this :mode mode))
		(get :mode this))

	(defmethod :canvas (canvas)
		; (. render :canvas canvas) -> render
		;another canvas to draw on from the next frame, the app's was made
		;again, of another size say. A frame that is out for the old one
		;is not waited for. The farm is kept, its children find the pixels
		;and the texture a frame is for from the frame itself
		(frame-forget)
		(def this :canvas canvas :no_farm :nil :back :nil)
		this)

	(defmethod :busy ()
		; (. render :busy) -> :nil | :t
		;a frame is out with a farm, the next is not to be given yet
		(get :out this))

	(defmethod :drop ()
		; (. render :drop) -> render
		;a frame that is out is not waited for, the view it was of has
		;gone, and the next may be given at once. The farm is kept
		(frame-forget)
		this)

	(defmethod :tick ()
		; (. render :tick) -> render
		;called from the app's timer: now and then a child that has gone,
		;or been too long, is started again. And a farm that has had no
		;frame for a while is let go of, its work has stopped, the next
		;frame makes a new one
		(defq ticks (% (inc (get :ticks this)) +tick_rate) jobs (get :jobs this))
		(def this :ticks ticks)
		(and jobs (= ticks 0)
			(. jobs :refresh (if (eql (get :kind this) :gpu) (get :gpu_patience this) (get :patience this))))
		;a frame the nodes are still drawing is shown as far as it has got
		(and jobs (= ticks 0) (get :out this) (get :partial this) (eql (get :kind this) :cpu)
			(. (get :canvas this) :swap +swap_write))
		(and jobs (not (get :out this)) (defq idle (get :idle this))
			(> (- (pii-time) (get :last this)) idle)
			(farm-end))
		this)

	(defmethod :how ()
		; (. render :how) -> str
		;how the last frame was drawn, and how long it took, for a line
		;of status. A frame the GPU drew straight into the canvas's texture
		;has no time said: an app that draws them one after another says
		;its rate
		(defq tenths (/ (get :took this) 100) how (get :how this))
		(if (eql how :gpu) "the GPU"
			(cat (cond
					((eql how :gpu_copy) "the GPU, read back")
					((eql how :farm) (cat "native code, a " (get :part this) " each by "
							(str (if (defq jobs (get :jobs this)) (. jobs :size) 0)) " nodes"))
					(:t "native code, on this node"))
				", " (str (/ tenths 10)) "." (str (% tenths 10)) "ms a frame")))

	(defmethod :stop ()
		; (. render :stop) -> render
		;the farm let go of, its children told to go, and what it has out
		;not waited for: its work has stopped, or is no longer wanted. The
		;next frame makes a new one, and nothing late comes to it
		(farm-end)
		this)

	(defmethod :close ()
		; (. render :close) -> render
		;the farm let go of, the app is closing
		(farm-end)
		this))

(defclass Render-tris (canvas vfile pfile &optional cull) (Render canvas)
	; (Render-tris canvas vfile pfile [cull]) -> render
	;frames of triangles on a canvas, drawn with the vertex shader and
	;the pixel shader of those two files. With cull a triangle that faces
	;away is left out, :front those that face us. For the nodes to draw,
	;and for the GPU on a node with no GUI, the canvas is one whose pixels
	;are shared, (canvas-shared). It starts on the GPU if this node has
	;one
	(defq vertex (shader-load vfile))
	(def this :canvas canvas :vfile vfile :pfile pfile :cull cull
		:vertex vertex :pixel (shader-load pfile) :pipeline :nil
		;how many numbers a vertex is, of the attrs of the vertex shader
		:stride (reduce (# (+ %0 (shader-size (second %1)))) (shader-attrs vertex) 0)
		;the frame that is out, and the meshes, by their numbers
		:draws :nil :meshes (Fmap 31) :mesh_fn :nil :here_fn :nil)

	(defmethod :mesh (id verts)
		; (. render :mesh id verts) -> render
		;a mesh frames are drawn of, by a number of the app's, never given
		;to another while this render lasts: verts a doubles, the numbers
		;of a vertex one after another as the vertex shader has its attrs
		(. (get :meshes this) :insert id (list verts
				(reduce! (# (push %0 (!))) '() (nums) 0 (/ (length verts) (get :stride this))) :nil))
		this)

	(defmethod :forget ()
		; (. render :forget) -> render
		;no mesh is known any more, and the farm, whose children have
		;them, is let go of: for a new scene
		(farm-end)
		(def this :meshes (Fmap 31))
		this)

	(defmethod :here (draws)
		; (. render :here draws) -> :t
		;a frame by native code in this task, a thing after another into the
		;one depth buffer, and shown
		(defq canvas (get :canvas this) pixmap (getf canvas +canvas_pixmap 0)
			vertex (get :vertex this) pixel (get :pixel this))
		(cond
			((defq fn (get :here_fn this)) (fn canvas draws))
			(:t (defq depth (shader-vp-depth (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
				(unless (get :pipeline this)
					(def this :pipeline (shader-vp-pipeline vertex pixel)))
				(. canvas :fill (get :clear this))
				(each (lambda ((id vvals pvals &optional pictures))
						(and (list? vvals) (defq m (mesh-of id))
							(shader-vp-draw-tris (get :pipeline this) (first m) (second m) pixmap depth vvals
								;each picture under the name the pixel shader has it by
								(cat pvals (map (lambda ((name) picture) (list name (getf picture +canvas_pixmap 0)))
										(shader-textures pixel) (ifn pictures '())))
								(get :cull this))))
					draws)))
		(. canvas :swap +swap_write)
		(def this :took (- (pii-time) (get :began this)) :how :here :last (pii-time))
		:t)

	(defmethod :frame (draws)
		; (. render :frame draws) -> :nil | :t
		;a frame, a list of (id vvals pvals [pictures]): the number of a
		;mesh, the inputs of the vertex shader and of the pixel shader for
		;it, each a list of (name value), and the pictures the pixel shader
		;reads from, canvases, in the order it has them. Or each as
		;(shader-strip) takes it, (id vblock pblock [y y1 pictures]), the
		;blocks made, by an app with a :here_fn. :t if it is whole and
		;shown, it was drawn here. :nil if it is out with a farm, and
		;(:busy) says when it no longer is; no more is given till then
		(defq canvas (get :canvas this) pixmap (getf canvas +canvas_pixmap 0)
			vertex (get :vertex this) pixel (get :pixel this) key (canvas-key canvas)
			gpu (and (eql (get :mode this) :gpu) (not (get :no_gpu this)))
			;can the nodes draw it: the pixels and every picture shared,
			;and a node besides this one
			farm (and (not gpu) (not (get :no_farm this)) (/= key 0) (> (length (lisp-nodes :t)) 1)
				(not (some (lambda ((id vvals pvals &optional pictures &rest _))
						(and (list? vvals) (some (# (= (canvas-key %0) 0)) (ifn pictures '())))) draws))))
		(def this :draws draws :began (pii-time) :last (pii-time))
		;a farm of the other kind is let go of
		(and (get :jobs this) (nql (get :kind this) (cond (gpu :gpu) (farm :cpu))) (farm-end))
		(defq packed (if (or gpu farm)
				(map (lambda (draw)
						(bind '(id vvals pvals &optional pictures &rest _) draw)
						;one whose blocks are made is as a strip takes it
						(if (str? vvals) draw
							(list id (shader-pack vertex vvals) (shader-pack pixel pvals)
								0 (getf pixmap +pixmap_height 0)
								(map (# (ifn (shader-strip-texture %0) "")) (ifn pictures '()))))) draws)))
		(cond
			(gpu ;the one job, all of the frame, to a child on this node
				(unless (get :jobs this)
					(def this :kind :gpu :jobs (Jobs +shader_tris_gpu_child
							(elem-get (get :mboxes this) +rt_mbox_task) (elem-get (get :mboxes this) +rt_mbox_reply)
							1 :here)))
				(defq w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0))
				(. (get :jobs this) :add (list (shader-strip (get :vfile this) (get :pfile this)
						(elem-get (get :mboxes this) +rt_mbox_ask) key w h 0 h (get :cull this) packed
						(canvas-target (back)))))
				(def this :out :t)
				:nil)
			(farm
				(unless (get :jobs this)
					;a new farm, a child for each node but this one, and none
					;of them on this one, a strip is one long call and would
					;hold up the app. Till they say they are ready the frames
					;are drawn here
					(def this :kind :cpu :jobs (Jobs +shader_tris_child
							(elem-get (get :mboxes this) +rt_mbox_task) (elem-get (get :mboxes this) +rt_mbox_reply)
							(list 64 (max 1 (dec (length (lisp-nodes :t)))) 0) :t))
					(strips packed :nil))
				(cond
					((get :ready this)
						(. canvas :fill (get :clear this))
						(strips packed :t)
						(def this :out :t)
						:nil)
					((. this :here draws))))
			((. this :here draws))))

	(defmethod :mail (idx msg)
		; (. render :mail idx msg) -> :nil | :t
		;what came to mailbox idx of (:mboxes). :t if a frame that was out
		;is now whole and shown, the app may give the next
		(defq jobs (get :jobs this))
		(cond
			((= idx +rt_mbox_task)
				(if jobs (. jobs :launched msg))
				:nil)
			((= idx +rt_mbox_ask)
				;a child has not got a mesh. It waits for it, so it is
				;answered whatever: one that asks for a mesh that has gone,
				;a child of a farm let go of, is given any there is
				(defq m (mesh-of (getf msg +strip_ask_mesh)))
				(unless m (. (get :meshes this) :each (lambda (k v) (setq m v))))
				(when m
					(unless (third m) (elem-set m 2 (shader-verts-str (first m))))
					(shader-mesh-send msg (third m)))
				:nil)
			((and jobs (defq out (. jobs :answered msg)))
				(defq how (getf msg +strip_reply_drawn) gpu (eql (get :kind this) :gpu)
					was_out (get :out this) draws (get :draws this))
				(cond
					((and gpu (= how +strip_none))
						;there is no GPU for it after all, the CPU has the frames
						;from here, this one too
						(farm-end)
						(def this :no_gpu :t :mode :cpu)
						(and was_out (. this :frame draws)))
					((= how +strip_none)
						;a child that could not reach the pixels, the frames are
						;drawn here from now on, this one too
						(farm-end)
						(def this :no_farm :t)
						(and was_out (. this :here draws)))
					((/= out 0) :nil)
					(was_out (whole how))
					;the children of the CPU's farm are ready
					(:t (def this :ready :t) :nil))))))

(defclass Render-shade (canvas file &optional lines) (Render canvas)
	; (Render-shade canvas file [lines]) -> render
	;frames of a pixel shader, of that file, over all of a canvas. By the
	;GPU, a farm of one on this node, which draws the frame straight into
	;the canvas's texture where the node has the GUI, and reads it back onto
	;the canvas's pixels where it has not; or by the nodes, a tile of so
	;many lines of the frame each, 8 if not given, shown as it fills. The
	;canvas is one whose pixels are shared, (canvas-shared), for the nodes
	;to shade straight onto: a child that can not reach them sends its
	;pixels back
	;:direct is the GPU to draw into the texture where it can. An app that
	;wants every frame's pixels, to keep them, has it :nil: they are then
	;always on the canvas's pixels, read back, and the same to the bit
	;whatever node drew them
	;:partial is a frame of the nodes shown as far as it has got, now and
	;then, for a picture that stands still as it fills in; a picture that
	;moves is shown whole frames only, which is how it starts
	;:farm is the nodes to draw a frame the GPU does not. With it :nil such
	;a frame is drawn here, in the app's own task, by native code, (:here):
	;for a small picture drawn now and then, not worth a child a node
	(def this :file file :program (shader-load file) :lines (ifn lines 8) :vals :nil
		:direct :t :farm :t :native :nil :patience (task-timeout 5) :part "tile")

	(defmethod :here (vals)
		; (. render :here vals) -> :t
		;a frame by native code in this task, and shown
		(defq canvas (get :canvas this) pixmap (getf canvas +canvas_pixmap 0) program (get :program this)
			w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0))
		(unless (get :native this) (def this :native (shader-vp program)))
		;a row at a time, this task is on the node the GUI is on
		(shader-vp-draw-rows (get :native this) (shader-vp-frame program (get :native this) vals)
			pixmap 0 0 w h h :t)
		(. canvas :swap +swap_write)
		(def this :took (- (pii-time) (get :began this)) :how :here :last (pii-time))
		:t)

	(defmethod :frame (vals)
		; (. render :frame vals) -> :nil | :t
		;a frame, the inputs of the shader for it, a list of (name value).
		;:t if it is whole and shown, it was drawn here. :nil if it is out
		;with a farm, (:busy) says when it no longer is; no more is given
		;till then
		(defq canvas (get :canvas this) pixmap (getf canvas +canvas_pixmap 0)
			w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0)
			key (canvas-key canvas) file (get :file this) mboxes (get :mboxes this)
			inputs (shader-pack (get :program this) vals) lines (get :lines this)
			gpu (and (eql (get :mode this) :gpu) (not (get :no_gpu this))))
		(def this :vals vals :began (pii-time) :last (pii-time))
		;a farm of the other kind is let go of
		(and (get :jobs this) (nql (get :kind this) (cond (gpu :gpu) ((get :farm this) :cpu))) (farm-end))
		(cond
			((and (not gpu) (not (get :farm this))) (. this :here vals))
			(gpu ;the one tile, all of the frame, to a child on this node
				(unless (get :jobs this)
					(def this :kind :gpu :jobs (Jobs +shader_shade_child
							(elem-get mboxes +rt_mbox_task) (elem-get mboxes +rt_mbox_reply) 1 :here)))
				(. (get :jobs this) :add (list (shader-tile file inputs 0 0 w h w h key
						(if (get :direct this) (canvas-target (back)) 0))))
				(def this :out :t)
				:nil)
			(:t ;a tile each, the children kept off this node, a tile is one
				;long call of native code
				(unless (get :jobs this)
					(def this :kind :cpu :ready :t :jobs (Jobs +shader_tile_child
							(elem-get mboxes +rt_mbox_task) (elem-get mboxes +rt_mbox_reply)
							(list +max_int 1 1) :t)))
				(. (get :jobs this) :add (map! (lambda () (defq y (* (!) lines))
							(shader-tile file inputs 0 y w (min h (+ y lines)) w h key))
						'() 0 (/ (+ h lines -1) lines)))
				(def this :out :t)
				:nil)))

	(defmethod :mail (idx msg)
		; (. render :mail idx msg) -> :nil | :t
		;what came to mailbox idx of the render's. :t if a frame that was
		;out is now whole and shown, the app may give the next
		(defq jobs (get :jobs this))
		(cond
			((= idx +rt_mbox_task)
				(if jobs (. jobs :launched msg))
				:nil)
			((and (= idx +rt_mbox_reply) jobs (defq out (. jobs :answered msg)))
				(cond
					((nql (get :kind this) :gpu)
						;a child that could not reach the canvas sends the pixels
						(shader-tile-show (get :canvas this) msg)
						(and (= out 0) (whole +shade_pixels)))
					((= (defq how (getf msg +shade_reply_drawn)) +shade_none)
						;there is no GPU for it after all, the CPU has the frames
						;from here, this one too
						(farm-end)
						(def this :no_gpu :t :mode :cpu)
						(. this :frame (get :vals this)))
					((whole how)))))))

(defclass Render-work (canvas child &optional gpu_child) (Render canvas)
	; (Render-work canvas child [gpu_child]) -> render
	;frames by children of the app's own, with jobs of the app's own: a
	;farm of that child on the nodes of this machine, a part of the frame
	;each, onto the canvas's pixels, which are shared, (canvas-shared); or
	;a farm of one of gpu_child, if the app has one, given the frame as the
	;GPU is to have it; or here, in the app's own task. The modes are :cpu,
	;:gpu and :here. What a frame is the app says, in properties:
	;:jobs_fn (lambda (kind count ready)) the jobs of a frame for a farm of
	;that kind, :cpu or :gpu, of count children, or with ready the jobs
	;that ask its children if they are ready, :nil if they need not be
	;asked; :here_fn (lambda (canvas)) the frame drawn here, not shown;
	;:reply_fn (lambda (msg)) an answer of a frame, what the app keeps of
	;it, and :none from it is a child that could not do the work where it
	;is; :cannot_fn (lambda (msg)) asked of an answer before it is counted,
	;:t if the child could not do its job where it is: the GPU's child has
	;no GPU after all, and the CPU has the frames, or a child of the CPU's
	;could not, and its job goes to another node, its own is left out
	;while the farm lasts. :herd is how many children the nodes have, as
	;(Jobs) takes it, and :away keeps them off this node. :send_back is
	;children that send the pixels of a part they could not reach, which
	;the :reply_fn puts on the canvas, so the nodes draw a frame with the
	;pixels not shared too. :launched_fn (lambda (kind msg)) is told of
	;each child as it starts, the first, or one in place of one that went.
	;With no canvas, :nil, the work is not a picture: nothing is shown, the
	;jobs are given to (:frame), and (:busy) says when all are answered
	(def this :child child :gpu_child gpu_child :jobs_fn :nil :here_fn :nil :reply_fn :nil
		:cannot_fn :nil :launched_fn :nil :herd (list 64) :away :nil :send_back :nil :part "part")
	(unless gpu_child (def this :no_gpu :t :mode :cpu))

	(defmethod :here ()
		; (. render :here) -> :t
		;a frame drawn in this task, and shown
		(defq canvas (get :canvas this))
		((get :here_fn this) canvas)
		(. canvas :swap +swap_write)
		(def this :took (- (pii-time) (get :began this)) :how :here :last (pii-time))
		:t)

	(defmethod :frame (&optional work)
		; (. render :frame [work]) -> :nil | :t
		;a frame of the moment, its jobs asked of the :jobs_fn, or work, the
		;jobs themselves. :t if it is whole and shown, it was drawn here:
		;the mode is :here, the pixels are not shared, or the children are
		;not yet ready. :nil if it is out with a farm, (:busy) says when it
		;no longer is; no more is given till then
		(defq mode (get :mode this) mboxes (get :mboxes this) make (get :jobs_fn this)
			canvas (get :canvas this)
			gpu (and (eql mode :gpu) (not (get :no_gpu this)))
			farm (and (eql mode :cpu) (not (get :no_farm this))
				(or (not canvas) (get :send_back this) (/= (canvas-key canvas) 0))))
		(def this :began (pii-time) :last (pii-time))
		;a farm of the other kind is let go of
		(and (get :jobs this) (nql (get :kind this) (cond (gpu :gpu) (farm :cpu))) (farm-end))
		(cond
			((not (or gpu farm)) (. this :here))
			(:t (unless (get :jobs this)
					(def this :kind (if gpu :gpu :cpu) :jobs (if gpu
							(Jobs (get :gpu_child this) (elem-get mboxes +rt_mbox_task)
								(elem-get mboxes +rt_mbox_reply) 1 :t)
							(Jobs (get :child this) (elem-get mboxes +rt_mbox_task)
								(elem-get mboxes +rt_mbox_reply) (get :herd this) (get :away this))))
					;its children asked if they are ready, if the app has a job
					;for that: till they are the frames are drawn here
					(if (and make (defq ready (make (get :kind this) (. (get :jobs this) :size) :t)))
						(. (get :jobs this) :add ready)
						(def this :ready :t)))
				(cond
					((get :ready this)
						(. (get :jobs this) :add (ifn work (make (get :kind this) (. (get :jobs this) :size) :nil)))
						(def this :out :t)
						:nil)
					((. this :here))))))

	(defmethod :mail (idx msg)
		; (. render :mail idx msg) -> :nil | :t
		;what came to mailbox idx of the render's. :t if a frame that was
		;out is now whole and shown, the app may give the next
		(defq jobs (get :jobs this) gpu (eql (get :kind this) :gpu))
		(cond
			((= idx +rt_mbox_task)
				(when jobs
					(if (defq fn (get :launched_fn this)) (fn (get :kind this) msg))
					(. jobs :launched msg))
				:nil)
			((/= idx +rt_mbox_reply) :nil)
			((not jobs) :nil)
			((and (not gpu) (defq fn (get :cannot_fn this)) (fn msg))
				;its node can not do it, another does the job
				(. jobs :cannot msg)
				:nil)
			((or (and gpu (defq fn (get :cannot_fn this)) (fn msg))
					(and (defq out (. jobs :answered msg)) (get :out this)
						(eql (if (defq fn (get :reply_fn this)) (fn msg)) :none)))
				;a child that can not do the work where it is: no GPU for it
				;after all, or pixels it can not reach. The frame is drawn the
				;next way down, and those after it
				(farm-end)
				(if gpu (def this :no_gpu :t :mode :cpu) (def this :no_farm :t))
				(. this :frame))
			((not out) :nil)
			((not (get :out this))
				;its children have said they are ready
				(if (= out 0) (def this :ready :t))
				:nil)
			((= out 0) (whole +shade_pixels)))))

;module
(export-classes '(Render Render-tris Render-shade Render-work))
(env-pop)
