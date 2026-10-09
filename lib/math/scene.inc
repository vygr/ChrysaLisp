;;;;;;;;;;;;;
; scene class
;;;;;;;;;;;;;

(import "./mesh.inc")
(import "././gpu/vp.inc")

;the two shaders the faces of a scene are drawn with
(defq +scene_vertex_file "lib/gpu/shaders/mesh_vertex.shader"
	+scene_pixel_file "lib/gpu/shaders/mesh_lit.shader")

;module
(env-push)

(defq +num_poly_buckets 256)

(defun circle (r)
	; (circle r) -> path
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* (n2f r) 4.0)) 0.25))
	(memoize r (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path)) 13))

(defun lighting (col at)
	; (lighting col at)
	;very basic attenuation and diffuse
	(defq alpha (first col) col (rest col))
	(bind '(r g b) (vector-min (vector-add
		;attenuation
		(vector-scale col (* (n2f at) 255.0) (const (cat +fixeds_tmp3)))
		;diffuse
		(vector-scale col 64.0 +fixeds_tmp3))
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (n2i (* alpha 255.0)) 24) (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun lighting-at3 (col at sp)
	; (lighting-at3 col at sp)
	;very basic attenuation, diffuse and specular
	(defq alpha (first col) col (rest col))
	(bind '(r g b) (vector-min (vector-add (vector-add
		;attenuation
		(vector-scale col (* (n2f at) (const (/ 255.0 3.0))) (const (cat +fixeds_tmp3)))
		;diffuse
		(vector-scale col 64.0 (const (cat +fixeds_tmp3))) (const (cat +fixeds_tmp3)))
		;specular
		(vector-scale (const (fixeds 255.0 255.0 255.0)) (n2f sp) (const (cat +fixeds_tmp3))) +fixeds_tmp3)
		;clamp
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (n2i (* alpha 255.0)) 24) (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun render-object-verts (size mat4x4_proj object)
	(defq sp (* +real_1/2 (n2r (dec size))) col (. object :get_color))
	(each (lambda ((x y z w))
			(defq rw (recip w) z (* z rw))
			(when (<= +real_-1 z +real_1)
				;y is up, as the faces are drawn
				(defq x (* x rw) y (* y rw) at (recip (+ z +real_2)) r (* (const (n2r 0.02)) sp rw)
					sx (* (+ x +real_1) sp) sy (* (- +real_1 y) sp))
				(push (elem-get poly_buckets (n2i (* (+ z +real_1) (const (n2r (/ +num_poly_buckets 2))))))
					(list (circle r) (lighting col at) (n2f sx) (n2f sy)))))
		(partition (mat4x4-vec4-mul mat4x4_proj (. object :get_verts)) +vec4_size)))

(defun scene-pipeline (&optional files)
	;the two shaders a scene is drawn with, assembled the first time. Or
	;those of an object that has two of its own, their files
	(memoize (if files (sym (cat (first files) (second files))) :scene_pipeline)
		(shader-vp-pipeline
			(shader-load (if files (first files) +scene_vertex_file))
			(shader-load (if files (second files) +scene_pixel_file))) 3))

(defun scene-order (count)
	;the triangles of count vertices taken three at a time
	(memoize count (apply (const nums) (range 0 count)) 11))

(defun mesh-corners (mesh)
	; (mesh-corners mesh) -> str
	;a mesh as the shaders want it. A face is lit flat, so a vertex that
	;faces share is a vertex of its own for each of them, where it is and
	;then the normal of that face, 7 numbers. The triangles are then just
	;the vertices in the order they come. It is the bytes of the numbers,
	;as they go in a message to a child that draws.
	(defq verts (. mesh :get_verts) norms (. mesh :get_norms) out (list (reals)))
	(each (lambda ((i0 i1 i2 in))
		(defq n (slice norms (* in +vec3_size) (* (inc in) +vec3_size)))
		(each (# (push out (slice verts (* %0 +vec4_size) (* (inc %0) +vec4_size)) n))
			(list i0 i1 i2))) (partition (. mesh :get_tris) +tri_size))
	(shader-verts-str (apply (const cat) out)))

(defun mesh-corners-smooth (mesh)
	; (mesh-corners-smooth mesh) -> str
	;a mesh as the shaders want it, lit smooth. The normal at a vertex is
	;that of all the faces that share it, taken together, so a face shades
	;from corner to corner and a ball of faces is round
	(defq verts (. mesh :get_verts) norms (. mesh :get_norms)
		tris (partition (. mesh :get_tris) +tri_size) out (list (reals))
		sums (map (lambda (&) (reals +real_0 +real_0 +real_0))
			(range 0 (/ (length verts) +vec4_size))))
	(each (lambda ((i0 i1 i2 in))
		(defq n (slice norms (* in +vec3_size) (* (inc in) +vec3_size)))
		(each (# (nums-add (elem-get sums %0) n (elem-get sums %0))) (list i0 i1 i2))) tris)
	(each (lambda (n)
		(defq d (nums-dot n n))
		(if (> d +real_0) (nums-scale n (recip (sqrt d)) n))) sums)
	(each (lambda ((i0 i1 i2 in))
		(each (# (push out (slice verts (* %0 +vec4_size) (* (inc %0) +vec4_size)) (elem-get sums %0)))
			(list i0 i1 i2))) tris)
	(shader-verts-str (apply (const cat) out)))

(defun mesh-ball (mesh)
	; (mesh-ball mesh) -> (centre radius)
	;a ball that the mesh is inside, the middle of its box and how far
	;the furthest vertex is from there
	(defq verts (partition (. mesh :get_verts) +vec4_size)
		lo (reduce (const nums-min) verts) hi (reduce (const nums-max) verts)
		centre (nums-scale (nums-add lo hi) +real_1/2) far +real_0)
	(each (# (defq d (nums-sub %0 centre)) (setq far (max far (nums-dot d d)))) verts)
	(list (cat (slice centre 0 3) (reals +real_1)) (sqrt far)))

(defun ball-rows (centre radius mat4x4_obj mat4x4_frust height)
	; (ball-rows centre radius mat4x4_obj mat4x4_frust height) -> (y y1)
	;the rows of a frame of that height a ball may be on, as it is seen.
	;All of them if it is at the eye or behind it. The ball is as big as
	;the most the matrix of its object stretches anything
	(defq eye (mat4x4-vec4-mul mat4x4_obj centre)
		stretch (sqrt (max
			(nums-dot (defq c0 (reals (elem-get mat4x4_obj 0) (elem-get mat4x4_obj 4) (elem-get mat4x4_obj 8))) c0)
			(nums-dot (defq c1 (reals (elem-get mat4x4_obj 1) (elem-get mat4x4_obj 5) (elem-get mat4x4_obj 9))) c1)
			(nums-dot (defq c2 (reals (elem-get mat4x4_obj 2) (elem-get mat4x4_obj 6) (elem-get mat4x4_obj 10))) c2)))
		r (* radius stretch) clip (mat4x4-vec4-mul mat4x4_frust eye) w (- (elem-get clip 3) r))
	(cond
		((<= w +real_0) (list 0 height))
		(:t (defq mid (/ (elem-get clip 1) (elem-get clip 3))
				;how big it is on the screen, taken at its nearest point
				reach (/ (* r (abs (elem-get mat4x4_frust 5))) w)
				half (* (n2r height) +real_1/2))
			(list (max 0 (- (n2i (* (- +real_1 (+ mid reach)) half)) 2))
				(min height (+ (n2i (* (- +real_1 (- mid reach)) half)) 3))))))

(defclass Scene-node (&optional name) :nil
	; (Scene-node [name]) -> scene_node
	(def this
		:children (list) :parent :nil
		:rot_x +real_0 :rot_y +real_0 :rot_z +real_0
		:trans_x +real_0 :trans_y +real_0 :trans_z +real_0
		:scale_x +real_1 :scale_y +real_1 :scale_z +real_1
		:matrix (Mat4x4-unity) :imatrix (Mat4x4-unity)
		:dirty :nil :name (ifn name ""))

	(defmethod :get_parent ()
		; (. scene_node :get_parent) -> scene_node | :nil
		(obj-ref (get :parent this)))

	(defmethod :set_parent (parent)
		; (. scene_node :set_parent parent) -> scene_node
		(set this :parent (weak-ref parent))
		this)

	(defmethod :children ()
		; (. scene_node :children) -> children
		(get :children this))

	(defmethod :add_node (child)
		; (. scene_node :add_node child) -> scene_node
		(if (. child :get_parent) (. child :sub_node))
		(push (get :children this) child)
		(. child :set_parent this)
		this)

	(defmethod :sub_node ()
		; (. scene_node :sub_node) -> scene_node
		(when (defq parent (. child :get_parent))
			(. child :set_parent :nil)
			(defq children (get :children parent) idx (find child children))
			(set parent :children (erase children idx (inc idx))))
		this)

	(defmethod :set_rotation (x y z)
		; (. scene_node :set_rotation x y z) -> scene_node
		(set this :rot_x x :rot_y y :rot_z z :dirty :t)
		this)

	(defmethod :set_translation (x y z)
		; (. scene_node :set_translation x y z) -> scene_node
		(set this :trans_x x :trans_y y :trans_z z :dirty :t)
		this)

	(defmethod :set_scale (x y z)
		; (. scene_node :set_scale x y z) -> scene_node
		(set this :scale_x x :scale_y y :scale_z z :dirty :t)
		this)

	(defmethod :get_matrix ()
		; (. scene_node :get_matrix) -> matrix
		(when (get :dirty this)
			(raise :rot_x :rot_y :rot_z
				:trans_x :trans_y :trans_z
				:scale_x :scale_y :scale_z)
			(set this :dirty :nil :matrix
				(mat4x4-mul (Mat4x4-translate trans_x trans_y trans_z)
				(mat4x4-mul (Mat4x4-rotz rot_z)
				(mat4x4-mul (Mat4x4-roty rot_y)
				(mat4x4-mul (Mat4x4-rotx rot_x)
				(Mat4x4-scale scale_x scale_y scale_z)))))))
		(get :matrix this))

	(defmethod :walk_nodes (fi fo)
		; (. scene_node :walk_nodes fnc_in fnc_out) -> scene_node
		;if fnc_in returns :nil, it'll step down into that nodes children.
		;fnc_out is always called to balance calls to fnc_in.
		(defq stack (list this (get :children this) 0))
		(while (defq idx (pop stack) children (pop stack) this (pop stack))
			(cond
				((and (= idx 0) (callback fi (penv) this children))
					;not stepping in
					(callback fo (penv) this children))
				((< idx (length children))
					;step into child node
					(push stack this children (inc idx)
						(defq this (elem-get children idx)) (get :children this) 0))
				(:t ;step out
					(callback fo (penv) this children))))
		this)

	(defmethod :find_nodes (name)
		; (. scene_node :find_nodes name) -> nodes
		(defq nodes (list))
		(. this :walk_nodes
			(lambda (object children)
				(if (starts-with name (get :name object))
					(push nodes object))
				:nil)
			(lambda (object children)))
		nodes)
	)

(defclass Scene-object (mesh color &optional name) (Scene-node name)
	; (Scene-object mesh color [name]) -> scene_node
	(def this :mesh mesh :color color)

	(defmethod :get_mesh ()
		; (. scene_node :get_mesh) -> mesh
		(get :mesh this))

	(defmethod :set_mesh (mesh)
		; (. scene_node :set_mesh mesh) -> scene_node
		(lower :mesh) this)

	(defmethod :get_verts ()
		; (. scene_node :get_verts) -> verts
		(. (get :mesh this) :get_verts))

	(defmethod :get_norms ()
		; (. scene_node :get_norms) -> norms
		(. (get :mesh this) :get_norms))

	(defmethod :get_tris ()
		; (. scene_node :get_tris) -> tris
		(. (get :mesh this) :get_tris))

	(defmethod :get_color ()
		; (. scene_node :get_color) -> color
		(get :color this))
	)

(defclass Scene (&optional name) (Scene-node name)
	; (Scene [name]) -> scene_node
	(def this :poly_buckets (lists +num_poly_buckets) :meshes (list))

	(defmethod :draws (left right top bottom near far height)
		; (. scene :draws left right top bottom near far height) -> ((mesh vblock pblock y y1) ...)
		;what is drawn for a frame of the faces, as it is seen now, a frame
		;of that many rows. For each object that has a mesh, the number of
		;the mesh, the inputs of the vertex shader and of the pixel shader
		;for it, as the blocks they travel in, and the rows it may be on.
		;Objects that are see through come last, the furthest first.
		;It is what a child that draws a strip is sent, and what is drawn
		;from here, so the two are the same to the bit.
		;An object with :smooth is lit smooth, a normal a vertex. One with
		;:shaders, the files of a vertex and a pixel shader, is drawn with
		;those, they have the inputs of the scene's own, and its draw has
		;the files on the end, for what draws it to choose them by.
		(defq mat4x4_frust (Mat4x4-frustum left right top bottom near far)
			matrix_stack (list (Mat4x4-unity)) draws (list) thin (list) pipeline (scene-pipeline))
		(raise :meshes)
		(. this :walk_nodes
			(lambda (object children)
				(defq mat4x4_obj (mat4x4-mul (last matrix_stack) (. object :get_matrix)))
				(push matrix_stack mat4x4_obj)
				(when (def? :mesh object)
					;the mesh is made into what the shaders want, and given
					;its number, the first time it is seen
					(unless (eql (get :corners_of object) (defq mesh (. object :get_mesh)))
						(def object :corners_of mesh :corners_id (length meshes)
							:ball (mesh-ball mesh))
						(push meshes (if (def? :smooth object) (mesh-corners-smooth mesh) (mesh-corners mesh))))
					(bind '(centre radius) (get :ball object))
					;its color, then how much of it there is, as the pixel
					;shader has it
					(defq col (. object :get_color) alpha (first col)
						files (if (def? :shaders object) (get :shaders object))
						with (if files (scene-pipeline files) pipeline)
						draw (cat (list (get :corners_id object)
							(shader-pack (first with)
								(list (list 'model mat4x4_obj) (list 'lens mat4x4_frust)))
							(shader-pack (second with)
								(list (list 'color (cat (rest col) (slice col 0 1))))))
							(ball-rows centre radius mat4x4_obj mat4x4_frust height)
							(if files (list files) '())))
					;what is see through is drawn after all that is not, the
					;furthest first, by how far off its middle is
					(if (< alpha 1.0)
						(push thin (list (elem-get (mat4x4-vec4-mul mat4x4_obj centre) 2) draw))
						(push draws draw)))
				:nil)
			(lambda (object children)
				(pop matrix_stack)))
		(each (# (push draws (second %0)))
			(sort thin (# (cond ((< (first %0) (first %1)) -1) ((> (first %0) (first %1)) 1) (0)))))
		draws)

	(defmethod :mesh (id)
		; (. scene :mesh id) -> str
		;the mesh of that number, as the shaders want it, for a child that
		;has asked for it
		(elem-get (get :meshes this) id))

	(defmethod :draw (canvas draws)
		; (. scene :draw canvas draws) -> scene
		;the faces of a frame, drawn here, by the shaders as native code,
		;with a depth buffer. The canvas is not swapped
		(defq pixmap (getf canvas +canvas_pixmap 0) pipeline (scene-pipeline)
			depth (shader-vp-depth (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
		(raise :meshes)
		(. canvas :fill 0)
		(each (lambda ((id vblock pblock &optional y y1 files))
			(defq verts (elem-get meshes id) with (if files (scene-pipeline files) pipeline))
			(shader-vp-draw-tris with verts (scene-order (/ (length verts) 56)) pixmap depth
				(shader-unpack (first with) vblock) (shader-unpack (second with) pblock) :t)
			(task-slice)) draws)
		this)

	(defmethod :render (canvas size left right top bottom near far mode)
		; (. scene :render canvas size left right top bottom near far mode) -> scene
		;with mode the faces of the meshes are drawn, by a vertex shader and
		;a pixel shader, with a depth buffer. Without it their vertices are,
		;as dots
		(cond
			(mode (. this :draw canvas (. this :draws left right top bottom near far size)))
			(:t (. canvas :fill 0)
				(defq mat4x4_frust (Mat4x4-frustum left right top bottom near far)
					matrix_stack (list (Mat4x4-unity)))
				(raise :poly_buckets)
				(. this :walk_nodes
					(lambda (object children)
						(defq mat4x4_obj (mat4x4-mul (last matrix_stack) (. object :get_matrix)))
						(push matrix_stack mat4x4_obj)
						(when (def? :mesh object)
							(render-object-verts size (mat4x4-mul mat4x4_frust mat4x4_obj) object)
							(task-slice))
						:nil)
					(lambda (object children)
						(pop matrix_stack)))
				(reach (lambda (path_list)
						(while (defq p (pop path_list))
							(bind '(p col sx sy) p)
							(. canvas :set_color col)
							(. canvas :fpoly sx sy +winding_odd_even (list p)))
						(task-slice))
					poly_buckets)))
		(. canvas :swap +swap_write)
		this)
	)

;module
(export-classes '(Scene-node Scene-object Scene))
(env-pop)
