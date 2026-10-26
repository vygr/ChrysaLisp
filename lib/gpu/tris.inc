;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Triangles drawn with no GPU, a strip of the frame at a time, by
; the nodes.
;
; The app has a Jobs object, lib/task/jobs.inc, of the child here, and
; gives it a strip of the frame as a job, (shader-strip). The child has
; the two shaders as native code, the VP back end, and draws the strip
; straight onto the app's pixels, which are in shared memory that it
; can reach, (canvas-shared), with a depth buffer of its own for just
; those rows.
;
; A job says what is drawn, a list of meshes, each by a number, with
; the inputs of the two shaders for it. It does not carry the meshes.
; A child that has not got one asks the app for it, the once, at a
; mailbox the app gives, and the app answers with (shader-mesh-send).
; So a frame is a few small messages, however big the meshes are.
;
; Nor does it carry the pictures a pixel shader reads from, its
; textures. Each is in shared memory, as the pixels of the frame are,
; and is named by its key and its size, (shader-strip-texture). A child
; finds it by them, the once. So there is the one copy of a picture on
; the machine, however many nodes draw with it, Chris: "If we have
; shared memory pixmaps, why do we need the texture on all children ?"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././task/jobs.inc")
(import "./shader.inc")
;shader-strip-texture takes a canvas, its field is needed as it is defined:
;a child that had only gui/pixmap/lisp.inc had none, an error that was
;passed over
(import "gui/canvas/lisp.inc")

(defq +shader_tris_child "lib/gpu/tris_child.lisp")

;a strip for a child to draw. Where it asks for a mesh, the key of the
;shared pixels, the size of the frame, the rows, the cull, 0, 1 or 2,
;then the paths of the vertex and pixel shader files, then the draws
(structure +strip +job_size
	(netid ask)
	(long shared)
	(uint width height y y1 cull vfile_length pfile_length draws)
	(offset files))

;a draw, the number of the mesh, the rows of the frame it may be on, then
;the inputs blocks of the two shaders, then the textures of the pixel
;shader, in the order it has them, a line each, its key, its width and
;its height. A child whose strip is none of those rows does nothing for
;it, it does not even place its vertices
(structure +strip_draw 0
	(long mesh)
	(uint vblock_length pblock_length y y1 tfiles_length)
	(offset blocks))

;its answer, the first row, and 1 if it was drawn, 0 if the child could
;not reach the pixels
(structure +strip_reply +job_reply_size
	(uint y drawn))

;a child asks for a mesh, where to send it, and its number
(structure +strip_ask 0
	(netid reply)
	(long mesh))

(defun shader-strip (vfile pfile ask shared width height y y1 cull draws)
	; (shader-strip vfile pfile ask shared width height y y1 cull draws) -> job
	;the job for rows y to y1 of a frame of that width and height. ask
	;is the mailbox a child asks for a mesh at, shared the key of the
	;pixels, (canvas-key). draws is a list of (mesh vblock pblock [y y1
	;tfiles]), the number of a mesh, the blocks of (shader-pack) for the
	;two shaders, the rows of the frame the mesh may be on, if the app
	;knows, all of them if not, and a list of the textures of the pixel
	;shader, if it has any, in the order it has them, each of
	;(shader-strip-texture).
	(apply (const cat) (cat (list (setf-> (str-alloc +strip_size)
			(+strip_ask ask) (+strip_shared shared)
			(+strip_width width) (+strip_height height) (+strip_y y) (+strip_y1 y1)
			(+strip_cull (cond ((not cull) 0) ((eql cull :front) 2) (1)))
			(+strip_vfile_length (length vfile)) (+strip_pfile_length (length pfile))
			(+strip_draws (length draws))) vfile pfile)
		(map (lambda ((mesh vblock pblock &optional dy dy1 tfiles))
			(defq tfiles (join (ifn tfiles '()) (ascii-char 10)))
			(cat (setf-> (str-alloc +strip_draw_size)
				(+strip_draw_mesh mesh)
				(+strip_draw_vblock_length (length vblock))
				(+strip_draw_pblock_length (length pblock))
				(+strip_draw_y (max 0 (ifn dy 0)))
				(+strip_draw_y1 (max 0 (ifn dy1 height)))
				(+strip_draw_tfiles_length (length tfiles))) vblock pblock tfiles)) draws))))

(defun shader-strip-texture (canvas)
	; (shader-strip-texture canvas) -> :nil | str
	;a picture as a strip names it, for a canvas whose pixels are in
	;shared memory, (canvas-shared). :nil if they are not, the children
	;of a farm can not reach them
	(and (defq pixmap (getf canvas +canvas_pixmap 0)) (/= (defq key (pixmap-key pixmap)) 0)
		(str key " " (getf pixmap +pixmap_width 0) " " (getf pixmap +pixmap_height 0))))

(defun shader-mesh-send (msg verts)
	; (shader-mesh-send msg verts)
	;answer a child that asked for a mesh, msg is what came to the ask
	;mailbox, verts the vertices of the mesh as a str, (shader-verts-str)
	(mail-send (getf msg +strip_ask_reply) verts))
