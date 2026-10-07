;the child of lib/gpu/tris.inc, it draws strips of a frame of triangles
;with a vertex shader and a pixel shader, as native code

(import "gui/pixmap/lisp.inc")
(import "./vp.inc")
(import "./tris.inc")

(enums +select 0
	(enum main timeout))

;the pair of shaders last asked for, as native code. They are assembled
;by the first child on this machine to ask for them
(defq shader_files "" pipeline :nil vertex :nil pixel :nil)

;the meshes it has been sent, by their numbers, and the triangles of a
;mesh of that many vertices, which are just the vertices in order
(defq meshes (Fmap 31) orders (Fmap 11))

;the pixels of the app's canvas, in shared memory, and the key they were
;found by
(defq shared_key 0 shared :nil)

(defun attach (key width height)
	;the app's pixels, found again if the key has changed. :nil if this
	;node can not reach them
	(unless (= key shared_key)
		(setq shared_key key shared (and (/= key 0) (pixmap-shared width height key))))
	shared)

(defun mesh (id ask)
	;the vertices of a mesh, asked for of the app if this is the first it
	;has been heard of
	(unless (defq verts (. meshes :find id))
		(defq mbox (mail-mbox))
		(mail-send ask (setf-> (str-alloc +strip_ask_size)
			(+strip_ask_reply mbox) (+strip_ask_mesh id)))
		(. meshes :insert id (setq verts (mail-read mbox))))
	verts)

(defun order (count)
	;the triangles of count vertices taken three at a time
	(unless (defq tris (. orders :find count))
		(. orders :insert count (setq tris (apply (const nums) (range 0 count)))))
	tris)

(defun strip (msg)
	;draw the strip, and say it is done. Every mesh of it is got, even if
	;the strip has no rows, an app asks for such a strip to have its
	;children ready before the first frame
	(bind '(key reply ask pixels_key width height y y1 cull vfile_length pfile_length draws)
		(getf-> msg +job_key +job_reply +strip_ask +strip_shared +strip_width +strip_height
			+strip_y +strip_y1 +strip_cull +strip_vfile_length +strip_pfile_length +strip_draws))
	(defq at (+ +strip_files vfile_length pfile_length)
		files (slice msg +strip_files at))
	(unless (eql files shader_files)
		(setq shader_files files
			vertex (shader-load (slice files 0 vfile_length))
			pixel (shader-load (slice files vfile_length -1))
			pipeline (shader-vp-pipeline vertex pixel)))
	(defq pixmap (attach pixels_key width height)
		depth (if (and pixmap (> y1 y)) (shader-vp-depth width (- y1 y)))
		attrs (elem-get (third pipeline) 2))
	(times draws
		(bind '(id vlen plen dy dy1) (getf-> (slice msg at (+ at +strip_draw_size))
			+strip_draw_mesh +strip_draw_vblock_length +strip_draw_pblock_length
			+strip_draw_y +strip_draw_y1))
		;a mesh that is on none of the rows of this strip is not drawn,
		;and not asked for till it is. A strip of no rows asks for them all
		(defq blocks (+ at +strip_draw_blocks)
			verts (if (or (= y1 y) (and (< dy y1) (> dy1 y))) (mesh id ask)))
		(if (and depth verts)
			(shader-vp-draw-tris pipeline verts (order (/ (length verts) 8 attrs)) pixmap depth
				(shader-unpack vertex (slice msg blocks (+ blocks vlen)))
				(shader-unpack pixel (slice msg (+ blocks vlen) (+ blocks vlen plen)))
				(elem-get '(:nil :t :front) cull) 3 0 y width y1 y))
		(setq at (+ blocks vlen plen)))
	(mail-send reply (setf-> (str-alloc +strip_reply_size)
		(+job_reply_key key) (+strip_reply_y y) (+strip_reply_drawn (if pixmap 1 0)))))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and draw the strip
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(strip msg)))))
