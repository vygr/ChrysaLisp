;the child of lib/gpu/tris.inc, it draws strips of a frame of triangles
;with a vertex shader and a pixel shader, as native code

(import "gui/pixmap/lisp.inc")
(import "./vp.inc")
(import "./tris.inc")

;the pair of shaders last asked for, as native code. They are assembled
;by the first child on this machine to ask for them
(defq shader_files "" pipeline :nil vertex :nil pixel :nil)

;the meshes it has been sent, by their numbers, and the triangles of a
;mesh of that many vertices, which are just the vertices in order
(defq meshes (Fmap 31) orders (Fmap 11))

;the pictures it has been named, each by what it was named as, kept for
;as long as the child is. One that could not be found is kept as that
(defq pictures (Fmap 11))

(defun picture (named)
	;the pixmap of a picture, found the first time it is named. It is in
	;shared memory, named by its key, its width and its height, and it
	;is the app's, there is the one copy of it on the machine
	(unless (defq pixmap (. pictures :find named))
		(bind '(key w h) (map (const str-as-num) (split named " ")))
		(. pictures :insert named (setq pixmap (ifn (and (/= key 0) (pixmap-shared w h key)) :none))))
	(if (nql pixmap :none) pixmap))

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

(defun strip (msg main)
	;draw the strip, and say it is done. Every mesh of it is got, even if
	;the strip has no rows, an app asks for such a strip to have its
	;children ready before the first frame. A child that was too long over
	;its strip has been told to go, its strip given to another: it stops
	;as it sees that, before its next mesh, and says nothing, so it does
	;not draw into a frame that is no longer its own. A child has one strip
	;at a time, so all that can come to its mailbox meanwhile is that
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
		depth (and pixmap (> y1 y) (shader-vp-depth width (- y1 y)))
		attrs (elem-get (third pipeline) 2))
	(defq gone :nil)
	(times draws
		(setq gone (or gone (mail-poll (list main))))
		(bind '(id vlen plen dy dy1 tlen) (getf-> (slice msg at (+ at +strip_draw_size))
			+strip_draw_mesh +strip_draw_vblock_length +strip_draw_pblock_length
			+strip_draw_y +strip_draw_y1 +strip_draw_tfiles_length))
		;a mesh that is on none of the rows of this strip is not drawn,
		;and not asked for till it is. A strip of no rows asks for them all
		(defq blocks (+ at +strip_draw_blocks)
			verts (if (or (= y1 y) (and (< dy y1) (> dy1 y))) (mesh id ask)))
		(and depth verts (not gone)
			(shader-vp-draw-tris pipeline verts (order (/ (length verts) 8 attrs)) pixmap depth
				(shader-unpack vertex (slice msg blocks (+ blocks vlen)))
				;the inputs of the pixel shader, and each of its textures
				;under its name, the picture it was named as. One that is
				;not named, or can not be found, is no picture
				(cat (shader-unpack pixel (slice msg (+ blocks vlen) (+ blocks vlen plen)))
					(map (lambda ((name) named) (list name (picture named)))
						(shader-textures pixel)
						(split (slice msg (+ blocks vlen plen) (+ blocks vlen plen tlen)) "\n")))
				(elem-get '(:nil :t :front) cull) 3 0 y width y1 y))
		(setq at (+ blocks vlen plen tlen)))
	(unless gone
		(mail-send reply (setf-> (str-alloc +strip_reply_size)
			(+job_reply_key key) (+strip_reply_y y) (+strip_reply_drawn (if pixmap 1 0))))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox) (strip msg mbox))))
