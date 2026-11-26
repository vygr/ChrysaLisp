;the GPU's child of lib/gpu/tris.inc, a farm of one. It has the GPU of its
;node draw a frame of triangles with a vertex shader and a pixel shader,
;and finds for itself how the frame gets to its app

(import "gui/pixmap/lisp.inc")
(import "./render.inc")
(import "./tris.inc")

;each pair of shaders it has been asked for, on the GPU, by its two files
;and the cull, kept for as long as the child is. One the GPU could not
;have is kept as that
(defq pairs (Fmap 11))

(defun pair-of (vfile pfile cull)
	; (pair-of vfile pfile cull) -> :nil | pair
	(unless (defq pair (. pairs :find (defq files (cat vfile "\n" pfile "\n" (str cull)))))
		(. pairs :insert files (setq pair (ifn (shader-gpu-pair (shader-load vfile) (shader-load pfile)
				(elem-get '(:nil :t :front) cull)) :none))))
	(if (nql pair :none) pair))

;the meshes it has been sent, by their numbers, each on the GPU, when
;each was last drawn, and when they were last looked over. One not drawn
;for a while is let go of, an app whose meshes come and go has them asked
;for again if it draws them again
(defq meshes (Fmap 31) drawn_at (Fmap 31) swept 0 +mesh_idle 10000000 +sweep_every 2000000)

(defun sweep ()
	;every mesh not drawn for +mesh_idle let go of
	(when (> (- (defq now (pii-time)) swept) +sweep_every)
		(setq swept now)
		(defq old (list))
		(. drawn_at :each (lambda (id at) (if (> (- now at) +mesh_idle) (push old id))))
		(each (lambda (id)
				(if (nql (defq m (. meshes :find id)) :none) (gpu-mesh-free m))
				(. meshes :erase id) (. drawn_at :erase id))
			old)))

;the pictures it has been named, each by what it was named as, a copy on
;the GPU, kept for as long as the child is. One that could not be found
;is kept as that
(defq pictures (Fmap 11))

;for a frame that is read back: a target of this node's GPU, the size of
;the last frame, and the pixels of the app's canvas, in shared memory,
;(pixmap-attach)
(defq target :nil target_size :nil held (list))

(defun mesh (id ask)
	;a mesh, on the GPU, its vertices asked for of the app if this is the
	;first it has been heard of. :nil if the GPU could not have it
	(unless (defq m (. meshes :find id))
		(defq mbox (mail-mbox))
		(mail-send ask (setf-> (str-alloc +strip_ask_size)
				(+strip_ask_reply mbox) (+strip_ask_mesh id)))
		(. meshes :insert id (setq m (ifn (gpu-mesh (mail-read mbox)) :none))))
	(. drawn_at :insert id (pii-time))
	(if (nql m :none) m))

(defun picture (named)
	;a picture, on the GPU, found the first time it is named: it is in
	;shared memory, named by its key, its width and its height, the app's.
	;:nil if it could not be found there, or the GPU could not have it
	(unless (defq p (. pictures :find named))
		(bind '(key w h) (map (const str-as-num) (split named " ")))
		(. pictures :insert named (setq p (ifn (and (/= key 0) (defq pixmap (pixmap-shared w h key))
				(gpu-picture (pixmap-buf pixmap) w h)) :none))))
	(if (nql p :none) p))

(defun drawn (handle frame gui main)
	; (drawn handle frame gui main) -> :nil | :gone | ticket
	;the frame given to the GPU, tried again while a pair is being built
	;or the GUI's texture is not yet to be drawn into. :gone if the child
	;is told to go meanwhile, it stops waiting
	(defq out 0)
	(while (eql 0 (setq out (gpu-draw handle frame gui)))
		(if (mail-poll (list main)) (setq out :gone) (task-sleep 500)))
	out)

(defun strip (msg main)
	;the frame drawn, and how it was said. A strip is all of the frame to
	;this child, whatever rows it names, but one of no rows: an app asks
	;for that to have its child ready, the pair made and every mesh and
	;picture on the GPU, before the first frame. If the child is told to go
	;while it waits for the GPU it says nothing, the frame is no longer its
	;own
	(bind '(key reply ask pixels_key handle width height y y1 cull vfile_length pfile_length draws)
		(getf-> msg +job_key +job_reply +strip_ask +strip_shared +strip_target +strip_width +strip_height
			+strip_y +strip_y1 +strip_cull +strip_vfile_length +strip_pfile_length +strip_draws))
	(defq at (+ +strip_files vfile_length pfile_length) how +strip_none gone :nil
		;the pair of the strip, what a draw with none of its own is drawn with
		pair (pair-of (slice msg +strip_files (+ +strip_files vfile_length))
			(slice msg (+ +strip_files vfile_length) at) cull))
	;each draw, its pair, its mesh and its pictures on the GPU. One whose
	;pair or mesh is not there is left out, and so is one with a picture
	;that is not
	(defq frame (list))
	(times draws
		(bind '(id vlen plen tlen slen) (getf-> (slice msg at (+ at +strip_draw_size))
				+strip_draw_mesh +strip_draw_vblock_length +strip_draw_pblock_length
				+strip_draw_tfiles_length +strip_draw_sfiles_length))
		(defq blocks (+ at +strip_draw_blocks) tfiles (+ blocks vlen plen) sfiles (+ tfiles tlen)
			m (mesh id ask) named (slice msg tfiles sfiles)
			had (if (empty? named) (list) (map (const picture) (split named "\n")))
			with (if (= slen 0) pair
				(apply (const pair-of) (push (split (slice msg sfiles (+ sfiles slen)) "\n") cull))))
		(and with m (not (some (# (not %0)) had))
			(push frame (list with m (slice msg blocks (+ blocks vlen))
					(slice msg (+ blocks vlen) tfiles) had)))
		(setq at (+ sfiles slen)))
	(when (and pair (> y1 y))
		(setq frame (gpu-draw-frame frame))
		;straight into the texture of the app's canvas, if this is the node
		;that has it: the GPU of this node is then the GUI's own
		(when (and (/= handle 0) (eql (slice ask +long_size -1) (task-nodeid)))
			(defq out (drawn handle frame :t main))
			(cond
				((eql out :gone) (setq gone :t))
				;said when the GPU is done with it, so an app that gives its
				;next frame on this answer gives the GPU no more than it can do
				(out (gpu-take (gpu-wait out))
					(setq how +strip_texture))))
		;or to a target of this node's, read back onto the app's pixels
		(when (and (= how +strip_none) (not gone)
				(defq pixmap (pixmap-attach held width height pixels_key)))
			(unless (and target (eql target_size (list width height)))
				(if target (gpu-target-free target))
				(setq target (gpu-target width height) target_size (list width height)))
			(when target
				(defq ticket (drawn target frame :nil main))
				(cond
					((eql ticket :gone) (setq gone :t))
					(ticket (gpu-take-pixels ticket (* width height +int_size) (pixmap-buf pixmap) 0)
						(setq how +strip_pixels))))))
	(sweep)
	;a strip of no rows is answered as drawn if there is a GPU to draw the
	;next with
	(and (= y1 y) pair (setq how +strip_pixels))
	(unless gone
		(mail-send reply (setf-> (str-alloc +strip_reply_size)
				(+job_reply_key key) (+strip_reply_y y) (+strip_reply_drawn how)))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox) (strip msg mbox)))
	(. meshes :each (lambda (k m) (if (nql m :none) (gpu-mesh-free m))))
	(. pictures :each (lambda (k p) (if (nql p :none) (gpu-picture-free p))))
	(if target (gpu-target-free target))
	(. pairs :each (lambda (k p) (if (nql p :none) (gpu-pair-free p)))))
