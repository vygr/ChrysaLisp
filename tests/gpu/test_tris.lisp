(report-header "GPU: triangles, a frame drawn in strips by a farm of children")

(import "gui/lisp.inc")
(import "lib/gpu/cpu.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/tris.inc")
(import "lib/math/mesh.inc")
(import "lib/math/matrix.inc")

(enums +tf 0 (enum task reply ask timer))

(defun tf-corners (mesh)
	;a mesh as the shaders want it, a vertex of its own for each corner of
	;each face, with the normal of the face
	(defq verts (. mesh :get_verts) norms (. mesh :get_norms) out (list (reals)))
	(each (lambda ((i0 i1 i2 in))
		(defq n (slice norms (* in 3) (* (inc in) 3)))
		(each (# (push out (slice verts (* %0 4) (* (inc %0) 4)) n)) (list i0 i1 i2)))
		(partition (. mesh :get_tris) 4))
	(apply (const cat) out))

(defun tf-pixels (pixmap size)
	(defq stream (memory-stream))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(slice (read-blk stream 1000000) 0 (* size size 4)))

(defq tf_vfile "lib/gpu/shaders/mesh_vertex.shader" tf_pfile "lib/gpu/shaders/mesh_lit.shader"
	tf_vertex (shader-load tf_vfile) tf_pixel (shader-load tf_pfile)
	tf_size 96 tf_select (list (mail-mbox) (mail-mbox) (mail-mbox) (mail-mbox))
	tf_canvas (canvas-shared tf_size tf_size 1))

(cond
	((not tf_canvas) (test-skip "triangles by a farm" "this host has no shared memory"))
	(:t (defq tf_pixmap (getf tf_canvas +canvas_pixmap 0)
			tf_meshes (list (tf-corners (Mesh-sphere +real_1/2 8)) (tf-corners (Mesh-torus +real_1 +real_1/3 8)))
			tf_packed (map (const shader-verts-str) tf_meshes)
			tf_lens (Mat4x4-frustum (n2r -0.5) (n2r 0.5) (n2r 0.5) (n2r -0.5) (n2r 1) (n2r 6))
			tf_vvals (map (# (list (list 'model %0) (list 'lens tf_lens)))
				(list (mat4x4-mul (Mat4x4-translate (n2r 0.3) (n2r 0) (n2r -2.2)) (Mat4x4-rotx (n2r 0.4)))
					(mat4x4-mul (Mat4x4-translate (n2r -0.2) (n2r 0.1) (n2r -3)) (Mat4x4-rotx (n2r 1.1)))))
			tf_pvals (map (# (list (list 'color %0))) '((1.0 0.2 0.2 1.0) (0.2 1.0 0.3 0.5)))
			tf_draws (map (# (list (!) (shader-pack tf_vertex %0) (shader-pack tf_pixel %1))) tf_vvals tf_pvals)
			;three children, whatever the nodes, so the frame is three strips
			tf_jobs (Jobs +shader_tris_child (elem-get tf_select +tf_task) (elem-get tf_select +tf_reply) '(3 3 0))
			tf_asked 0)
		(defun tf-frame (strips)
			;a frame, till every strip is answered, or far too long has gone by
			(defq out 1 drawn :t)
			(. tf_canvas :fill 0)
			(. tf_jobs :add (map (# (shader-strip tf_vfile tf_pfile (elem-get tf_select +tf_ask)
				(canvas-key tf_canvas) tf_size tf_size (/ (* %0 tf_size) strips) (/ (* (inc %0) tf_size) strips)
				:t tf_draws)) (range 0 strips)))
			(mail-timeout (elem-get tf_select +tf_timer) (task-timeout 30) 0)
			(while (> out 0)
				(defq msg (mail-read (elem-get tf_select (defq idx (mail-select tf_select)))))
				(case idx
					(+tf_task (. tf_jobs :launched msg))
					(+tf_reply (when (defq o (. tf_jobs :answered msg))
						(if (= (getf msg +strip_reply_drawn) 0) (setq drawn :nil))
						(setq out o)))
					(+tf_ask (setq tf_asked (inc tf_asked))
						(shader-mesh-send msg (elem-get tf_packed (getf msg +strip_ask_mesh))))
					(:t (setq out 0 drawn :nil))))
			(mail-timeout (elem-get tf_select +tf_timer) 0 0)
			drawn)
		(assert-eq "three children" 3 (. tf_jobs :size))
		(assert-true "a frame of three strips is drawn" (tf-frame 3))
		(defq tf_farmed (tf-pixels tf_pixmap tf_size))
		;a child that drew asked for both meshes. A quick child may have
		;drawn two of the strips before another was up, so it is two for
		;each child that drew, and never more than two for each child
		(assert-true "a child that drew asked for each mesh, the once"
			(and (even? tf_asked) (<= 2 tf_asked 6)))
		;the same frame by one task
		(defq tf_alone (Canvas tf_size tf_size 1) tf_alone_pixmap (getf tf_alone +canvas_pixmap 0)
			tf_depth (shader-vp-depth tf_size tf_size)
			tf_pipeline (shader-vp-pipeline tf_vertex tf_pixel))
		;the inputs of a strip travel as the blocks a GPU takes, 32 bit
		;floats, so the one task is given them from the blocks as well
		(each (lambda (mesh (id vblock pblock))
			(shader-vp-draw-tris tf_pipeline mesh (apply (const nums) (range 0 (/ (length mesh) 7)))
				tf_alone_pixmap tf_depth (shader-unpack tf_vertex vblock) (shader-unpack tf_pixel pblock) :t))
			tf_meshes tf_draws)
		(assert-eq "the farm's frame is the frame one task draws" (tf-pixels tf_alone_pixmap tf_size) tf_farmed)
		(assert-true "and it is a picture"
			(> (length (filter (# (/= (get-uint tf_farmed %0) 0)) (range 0 (length tf_farmed) 4))) 2000))
		(assert-true "a second frame, of seven strips" (tf-frame 7))
		(assert-eq "is the same frame" tf_farmed (tf-pixels tf_pixmap tf_size))
		(assert-true "and no child asked for a mesh again" (and (even? tf_asked) (<= tf_asked 6)))
		;a draw that says which rows its mesh is on. A strip that is none
		;of them does nothing for it, so with the wrong rows the mesh is
		;not drawn, and with the right ones the frame is the frame
		(defq tf_whole tf_draws
			tf_draws (list (cat (first tf_whole) (list 0 5)) (cat (second tf_whole) (list 0 tf_size))))
		(assert-true "a frame, the first mesh said to be on the top rows only" (tf-frame 3))
		(assert-true "is not the whole frame" (not (eql tf_farmed (tf-pixels tf_pixmap tf_size))))
		(setq tf_draws (map (# (cat %0 (list 0 tf_size))) tf_whole))
		(assert-true "a frame, both said to be on every row" (tf-frame 3))
		(assert-eq "is the frame" tf_farmed (tf-pixels tf_pixmap tf_size))
		(setq tf_draws tf_whole)
		(assert-true "a strip of no rows is answered, it is how children are made ready" (tf-frame 1))
		(. tf_jobs :close)))
