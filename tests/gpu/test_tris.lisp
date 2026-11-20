(report-header "GPU: triangles, a frame drawn in strips by a farm of children")

(import "gui/lisp.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/compute.inc")
(import "lib/gpu/tris.inc")
(import "lib/math/mesh.inc")
(import "lib/math/matrix.inc")

(enums +tf 0 (enum task reply ask timer))

(defun tf-corners (mesh)
	;a mesh as the shaders want it, a vertex of its own for each corner of
	;each face, with the normal of the face
	(defq verts (. mesh :get_verts) norms (. mesh :get_norms) out (list (doubles)))
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
			tf_meshes (list (tf-corners (Mesh-sphere +double_1/2 8)) (tf-corners (Mesh-torus +double_1 +double_1/3 8)))
			tf_packed (map (const shader-verts-str) tf_meshes)
			tf_lens (mat4x4-frustum (n2d -0.5) (n2d 0.5) (n2d 0.5) (n2d -0.5) (n2d 1) (n2d 6))
			tf_vvals (map (# (list (list 'model %0) (list 'lens tf_lens)))
				(list (mat4x4-mul (mat4x4-translate (n2d 0.3) (n2d 0) (n2d -2.2)) (mat4x4-rotx (n2d 0.4)))
					(mat4x4-mul (mat4x4-translate (n2d -0.2) (n2d 0.1) (n2d -3)) (mat4x4-rotx (n2d 1.1)))))
			tf_pvals (map (# (list (list 'color %0))) '((1.0 0.2 0.2 1.0) (0.2 1.0 0.3 0.5)))
			tf_draws (map (# (list (!) (shader-pack tf_vertex %0) (shader-pack tf_pixel %1))) tf_vvals tf_pvals)
			;three children, whatever the nodes, so the frame is three strips
			tf_jobs (Jobs +shader_tris_child (elem-get tf_select +tf_task) (elem-get tf_select +tf_reply) '(3 3 0))
			tf_asked 0 tf_launched 0 tf_said (list))
		(defun tf-frame (strips)
			;a frame, till every strip is answered, however long that takes.
			;A child with nothing to do for 5s goes, and a strip given to it as
			;it went is lost, so the farm is looked at each second, as an app does.
			;A child is only slow, never wrong: on a busy machine a strip may
			;wait seconds for its node, so one is given to another after 10s,
			;and given again it is given twice as long
			(defq out 1 drawn :t start (pii-time))
			(. tf_canvas :fill 0)
			(. tf_jobs :add (map! (# (shader-strip tf_vfile tf_pfile (elem-get tf_select +tf_ask)
							(canvas-key tf_canvas) tf_size tf_size (/ (* (!) tf_size) strips) (/ (* (inc (!)) tf_size) strips)
							:t tf_draws)) '() 0 strips))
			(mail-timeout (elem-get tf_select +tf_timer) 1000000 0)
			(while (> out 0)
				(defq msg (mail-read (elem-get tf_select (defq idx (mail-select tf_select)))))
				(case idx
					(+tf_task (setq tf_launched (inc tf_launched)) (. tf_jobs :launched msg))
					(+tf_reply (when (defq o (. tf_jobs :answered msg))
							(if (= (getf msg +strip_reply_drawn) 0) (setq drawn :nil))
							(setq out o)))
					(+tf_ask (setq tf_asked (inc tf_asked))
						(shader-mesh-send msg (elem-get tf_packed (getf msg +strip_ask_mesh))))
					(:t (. tf_jobs :refresh 10000000)
						(mail-timeout (elem-get tf_select +tf_timer) 1000000 0))))
			(mail-timeout (elem-get tf_select +tf_timer) 0 0)
			;what is said of a frame if a check of it fails
			(push tf_said (str strips " strips " (/ (- (pii-time) start) 1000) "ms, meshes asked "
					tf_asked ", most tries " (. tf_jobs :tries) ", children " (. tf_jobs :size)))
			drawn)
		(defq tf_failed *test_failures*)
		(assert-eq "three children" 3 (. tf_jobs :size))
		(assert-true "a frame of three strips is drawn" (tf-frame 3))
		(defq tf_farmed (tf-pixels tf_pixmap tf_size))
		;a child that drew asked for both meshes. A quick child may have
		;drawn two of the strips before another was up, so it is two for
		;each child that drew, and never more than two for each child started
		(assert-true "a child that drew asked for each mesh, the once"
			(and (even? tf_asked) (<= 2 tf_asked (* 2 tf_launched))))
		;the same frame by one task
		(defq tf_alone (Canvas tf_size tf_size 1) tf_alone_pixmap (getf tf_alone +canvas_pixmap 0)
			tf_depth (shader-vp-depth tf_size tf_size)
			tf_pipeline (shader-vp-pipeline tf_vertex tf_pixel))
		;the inputs of a strip travel as the blocks a GPU takes, 32 bit
		;floats, so the one task is given them from the blocks as well
		(each (lambda (mesh (id vblock pblock))
				(shader-vp-draw-tris tf_pipeline mesh (reduce! (# (push %0 (!))) '() (nums) 0 (/ (length mesh) 7))
					tf_alone_pixmap tf_depth (shader-unpack tf_vertex vblock) (shader-unpack tf_pixel pblock) :t))
			tf_meshes tf_draws)
		(assert-eq "the farm's frame is the frame one task draws" (tf-pixels tf_alone_pixmap tf_size) tf_farmed)
		(assert-true "and it is a picture"
			(> (reduce! (# (if (/= (get-uint tf_farmed (<< (!) 2)) 0) (inc %0) %0)) '() 0 0 (>> (length tf_farmed) 2)) 2000))
		(assert-true "a second frame, of seven strips" (tf-frame 7))
		(assert-eq "is the same frame" tf_farmed (tf-pixels tf_pixmap tf_size))
		;the children are kept while there is work, but one with none for 5s
		;goes: on a busy machine the frame drawn by one task above took longer,
		;and new children were started, each asking for the meshes as its own
		(assert-true "and no child asked for a mesh again" (and (even? tf_asked) (<= tf_asked (* 2 tf_launched))))
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
		;a draw with a pair of shaders of its own, not the strip's: the frame
		;one task draws with the two pairs, each thing with its own
		(defq tf_shiny '("lib/gpu/shaders/shiny_vertex.shader" "lib/gpu/shaders/shiny_lit.shader")
			tf_shiny_vertex (shader-load (first tf_shiny)) tf_shiny_pixel (shader-load (second tf_shiny))
			tf_withs (list (list tf_vertex tf_pixel tf_pipeline)
				(list tf_shiny_vertex tf_shiny_pixel (shader-vp-pipeline tf_shiny_vertex tf_shiny_pixel)))
			tf_two (Canvas tf_size tf_size 1) tf_two_pixmap (getf tf_two +canvas_pixmap 0)
			tf_two_depth (shader-vp-depth tf_size tf_size))
		(setq tf_draws (list (first tf_whole) (cat (second tf_whole) (list 0 tf_size '() tf_shiny))))
		(assert-true "a frame, the second thing with a pair of its own" (tf-frame 3))
		(defq tf_own (tf-pixels tf_pixmap tf_size))
		(each (lambda (mesh (id vblock pblock &rest _) (vertex pixel pipeline))
				(shader-vp-draw-tris pipeline mesh (reduce! (# (push %0 (!))) '() (nums) 0 (/ (length mesh) 7))
					tf_two_pixmap tf_two_depth (shader-unpack vertex vblock) (shader-unpack pixel pblock) :t))
			tf_meshes tf_draws tf_withs)
		(assert-eq "is the frame one task draws with the two pairs" (tf-pixels tf_two_pixmap tf_size) tf_own)
		(assert-true "and not the frame of the one pair" (not (eql tf_own tf_farmed)))
		(setq tf_draws tf_whole)
		(assert-true "a strip of no rows is answered, it is how children are made ready" (tf-frame 1))
		(. tf_jobs :close)
		;the GPU's child of the same job, a farm of one on this node, all
		;of the frame its strip. This node shows nothing, so it reads the
		;frame back onto the pixels. What it draws is the CPU's frame but
		;at the edges of things, where the GPU has 4 samples a pixel and
		;its own rule for which pixels a triangle has
		(cond
			((= (gpu-format) 0) (test-skip "triangles by the GPU's child" "this node has no GPU"))
			(:t ;new mailboxes, nothing late of the farm before comes in
				(elem-set tf_select +tf_task (mail-mbox))
				(elem-set tf_select +tf_reply (mail-mbox))
				(setq tf_jobs (Jobs +shader_tris_gpu_child (elem-get tf_select +tf_task)
					(elem-get tf_select +tf_reply) 1 :here))
				(assert-true "a frame by the GPU's child is drawn" (tf-frame 1))
				(defq tf_gpu (tf-pixels tf_pixmap tf_size) tf_lit 0 tf_far 0)
				(each! (lambda () (defq a (get-uint tf_farmed (<< (!) 2)) b (get-uint tf_gpu (<< (!) 2)))
						(if (/= a 0) (++ tf_lit))
						(if (> (reduce (# (max %0 (abs (- (logand (>> a %1) 255) (logand (>> b %1) 255)))))
								'(0 8 16 24) 0) 16) (++ tf_far)))
					'() 0 (>> (length tf_farmed) 2))
				(push tf_said (str "the GPU's frame, " tf_far " pixels far from the CPU's of " tf_lit " lit"))
				(assert-true "and is the frame the nodes draw, but at the edges of things" (< tf_far (/ tf_lit 6)))
				(assert-true "a second frame of it" (tf-frame 1))
				(assert-eq "is the same frame" tf_gpu (tf-pixels tf_pixmap tf_size))
				;and a thing with a pair of its own is drawn with that, by the GPU too
				(setq tf_draws (list (first tf_whole) (cat (second tf_whole) (list 0 tf_size '() tf_shiny))))
				(assert-true "a frame of it with a pair of a thing's own" (tf-frame 1))
				(assert-true "is not the frame of the one pair" (not (eql tf_gpu (tf-pixels tf_pixmap tf_size))))
				(setq tf_draws tf_whole)
				(. tf_jobs :close)))
		;a picture for the children to read, one whose pixels are its own is
		;had again in shared memory, one that is there already is itself
		(defq tf_picture (shader-strip-picture tf_alone))
		(assert-true "a picture's pixels are put in shared memory" (/= (canvas-key tf_picture) 0))
		(assert-eq "and are the pixels it had" (tf-pixels tf_alone_pixmap tf_size)
			(tf-pixels (getf tf_picture +canvas_pixmap 0) tf_size))
		(assert-true "it is named for a strip" (str? (shader-strip-texture tf_picture)))
		(assert-true "one in shared memory is itself" (eql (shader-strip-picture tf_canvas) tf_canvas))
		(if (> *test_failures* tf_failed) (each (# (print "test_tris: " %0)) tf_said))))
