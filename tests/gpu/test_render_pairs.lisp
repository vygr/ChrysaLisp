(report-header "GPU: a render of triangles, each draw by a pair of shaders of its own if it names one")

(import "gui/lisp.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/math/mesh.inc")
(import "lib/math/matrix.inc")

(defun rp-corners (mesh)
	;a mesh as the mesh shaders want it, a vertex of its own for each
	;corner of each face, with the normal of the face, 7 numbers
	(defq verts (. mesh :get_verts) norms (. mesh :get_norms) out (list (doubles)))
	(each (lambda ((i0 i1 i2 in))
			(defq n (slice norms (* in 3) (* (inc in) 3)))
			(each (# (push out (slice verts (* %0 4) (* (inc %0) 4)) n)) (list i0 i1 i2)))
		(partition (. mesh :get_tris) 4))
	(apply (const cat) out))

(defun rp-rgb (pixels x y size)
	;the red, green and blue of a pixel of them
	(defq argb (get-uint pixels (* 4 (+ (* y size) x))))
	(list (logand (>> argb 16) 0xff) (logand (>> argb 8) 0xff) (logand argb 0xff)))

(defun rp-pixels (canvas size)
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(slice (read-blk stream 1000000) 0 (* size size 4)))

(defq rp_size 96 rp_vfile "lib/gpu/shaders/mesh_vertex.shader" rp_pfile "lib/gpu/shaders/mesh_lit.shader"
	rp_shiny '("lib/gpu/shaders/shiny_vertex.shader" "lib/gpu/shaders/shiny_lit.shader")
	rp_flat '("lib/gpu/shaders/frame_vertex.shader" "tests/gpu/test_flat.shader")
	rp_lens (mat4x4-frustum (n2d -0.5) (n2d 0.5) (n2d 0.5) (n2d -0.5) (n2d 1) (n2d 6))
	;a ball by the render's own pair, a ring that shines by a pair of its
	;own, and in a corner a square of one colour, two triangles of corners
	;of 2 numbers each, by a pair whose vertex is not the render's
	rp_draws (list
		(list 0 (list (list 'model (mat4x4-mul (mat4x4-translate (n2d 0.3) (n2d 0) (n2d -2.2)) (mat4x4-rotx (n2d 0.4))))
				(list 'lens rp_lens)) '((color (1.0 0.2 0.2 1.0))))
		(list 1 (list (list 'model (mat4x4-mul (mat4x4-translate (n2d -0.3) (n2d 0.1) (n2d -3)) (mat4x4-rotx (n2d 1.1))))
				(list 'lens rp_lens)) '((color (0.2 0.8 1.0 1.0))) '() rp_shiny)
		(list 2 '() '((tint (1.0 0.9 0.1))) '() rp_flat))
	rp_meshes (list (rp-corners (Mesh-sphere +double_1/2 8)) (rp-corners (Mesh-torus +double_1/3 (n2d 0.12) 8))
		(doubles (n2d 0.5) (n2d 0.5) (n2d 0.9) (n2d 0.5) (n2d 0.9) (n2d 0.9)
			(n2d 0.5) (n2d 0.5) (n2d 0.9) (n2d 0.9) (n2d 0.5) (n2d 0.9))))

(defun rp-render (canvas)
	;a render of the mesh pair, the three meshes told to it, the square's by
	;the vertex shader it is drawn with
	(defq render (Render-tris canvas rp_vfile rp_pfile))
	(def render :clear +argb_black)
	(. render :mesh 0 (first rp_meshes))
	(. render :mesh 1 (second rp_meshes))
	(. render :mesh 2 (third rp_meshes) (first rp_flat))
	render)

(defq rp_seen (list))

(defun rp-until (render hows)
	;frames, as an app gives them, till one is drawn one of those ways,
	;however long that takes: the farm draws the frames here till its
	;children are ready, and the GPU too till it has its shaders. Each way
	;a frame was drawn meanwhile is kept, rp_seen
	(defq select (list (mail-mbox)) t0 (pii-time))
	(clear rp_seen)
	(until (or (find (get :how render) hows) (> (- (pii-time) t0) 120000000))
		(mail-timeout (first select) 20000 0)
		(bind '(idx msg) (. render :read select 0))
		(when idx
			(unless (. render :busy)
				(if (. render :frame rp_draws) (push rp_seen (get :how render))))
			(. render :tick))
		(unless idx (unless (find (get :how render) rp_seen) (push rp_seen (get :how render)))))
	(find (get :how render) hows))

(defq rp_canvas (canvas-shared rp_size rp_size 1))
(cond
	((not rp_canvas) (test-skip "a render's draws by pairs of their own" "this host has no shared memory"))
	(:t ;in this task, the one pipeline of each pair
		(defq rp_here (rp-render rp_canvas))
		(def rp_here :no_farm :t)
		(. rp_here :mode :cpu)
		(assert-eq "a frame in this task is drawn at once" :t (. rp_here :frame rp_draws))
		(defq rp_here_pixels (rp-pixels rp_canvas rp_size))
		(bind '(r g b) (rp-rgb rp_here_pixels 80 13 rp_size))
		(assert-true "the square is drawn, by the pair it names, of the colour given it"
			(and (> r 240) (> g 210) (< b 40)))
		(bind '(r g b) (rp-rgb rp_here_pixels 62 48 rp_size))
		(assert-true "the ball by the render's pair, red" (and (> r 100) (> r (* 2 g))))
		(. rp_here :close)
		;the same draws by the render's pair alone: the ring is drawn as the
		;ball is, and the square, of corners, is not a mesh it can draw
		(defq rp_one (Canvas rp_size rp_size 1) rp_one_render (rp-render rp_one))
		(def rp_one_render :no_farm :t)
		(. rp_one_render :mode :cpu)
		(. rp_one_render :frame (list (first rp_draws) (slice (second rp_draws) 0 3) (third rp_draws)))
		(assert-true "with no pair of its own the ring is drawn otherwise"
			(not (eql (rp-pixels rp_one rp_size) rp_here_pixels)))
		(. rp_one_render :close)
		;by the farm, a strip each node, each draw by its own pair there too
		(defq rp_farm_canvas (canvas-shared rp_size rp_size 1) rp_farm (rp-render rp_farm_canvas))
		(. rp_farm :mode :cpu)
		(cond
			((<= (length (lisp-nodes :t)) 1) (test-skip "the farm's frame" "one node, no farm"))
			((rp-until rp_farm '(:farm))
				(assert-eq "the farm's frame is the frame drawn in this task"
					rp_here_pixels (rp-pixels rp_farm_canvas rp_size)))
			(:t (assert-eq "the farm draws a frame" :farm (get :how rp_farm))))
		(. rp_farm :close)
		;by the GPU, which is not the CPU's to the pixel, at the edges of things
		(defq rp_gpu_canvas (canvas-shared rp_size rp_size 1) rp_gpu (rp-render rp_gpu_canvas))
		(cond
			((= (gpu-format) 0) (test-skip "the GPU's frame" "this host has no GPU"))
			((not (rp-until rp_gpu '(:gpu :gpu_copy)))
				(assert-eq "the GPU draws a frame" :gpu (get :how rp_gpu)))
			((eql (get :how rp_gpu) :gpu)
				(test-skip "the GPU's frame to the pixel" "drawn straight into the texture, on the GUI's node"))
			(:t ;a pixel is far from the CPU's if a colour is more than 16 from it,
				;the GPU's floats are not the VP's to the last bit, as test_tris; its
				;alpha is not the CPU's, the frame is shown opaque
				(defq rp_gpu_pixels (rp-pixels rp_gpu_canvas rp_size) rp_lit 0 rp_far 0)
				(each! (lambda () (defq a (get-uint rp_here_pixels (<< (!) 2)) b (get-uint rp_gpu_pixels (<< (!) 2)))
						(if (/= (logand a 0xffffff) 0) (++ rp_lit))
						(if (> (reduce (# (max %0 (abs (- (logand (>> a %1) 255) (logand (>> b %1) 255))))) '(0 8 16) 0) 16)
							(++ rp_far)))
					'() 0 (* rp_size rp_size))
				(assert-true "the GPU's frame is the CPU's but at the edges of things" (< rp_far (/ rp_lit 6)))
				(bind '(r g b) (rp-rgb rp_gpu_pixels 80 13 rp_size))
				(assert-true "and its square of the colour given it" (and (> r 240) (> g 210) (< b 40)))))
		;to the CPU from the GPU that draws: the GPU goes on drawing the
		;frames till the CPU's children are ready, none is drawn here
		(cond
			((or (= (gpu-format) 0) (<= (length (lisp-nodes :t)) 1))
				(test-skip "the GPU stands in for the CPU's farm" "no GPU, or one node, no farm"))
			((not (find (get :how rp_gpu) '(:gpu :gpu_copy)))
				(test-skip "the GPU stands in for the CPU's farm" "the GPU did not draw"))
			(:t (. rp_gpu :mode :cpu)
				(assert-eq "switched to the CPU, its farm draws in the end" 0 (rp-until rp_gpu '(:farm)))
				(assert-true "and no frame was drawn here meanwhile, the GPU stood in"
					(not (find :here rp_seen)))
				(assert-eq "and the GPU stands in no more" :nil (get :standby rp_gpu))))
		(. rp_gpu :close)))
