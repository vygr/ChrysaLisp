;;;;;;;;;;;;;
; scene class
;;;;;;;;;;;;;

(import "lib/math/mesh.inc")

;module
(env-push)

(defq +num_poly_buckets 50)

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq cache (const-quoted (fmap 13)) r (* (floor (* (n2f r) 4.0)) 0.25))
	(unless (defq p (. cache :find r))
		(. cache :insert r (defq p
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path)))))
	p)

(defun lighting (col at)
	;very basic attenuation and diffuse
	(defq alpha (elem-get 0 col) col (slice 1 -1 col))
	(bind '(r g b) (vec-min (vec-add
		;attenuation
		(vec-scale col (* (n2f at) 255.0) (const (cat +fixeds_tmp3)))
		;difuse
		(vec-scale col 64.0 +fixeds_tmp3))
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (n2i (* alpha 255.0)) 24) (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun lighting-at3 (col at sp)
	;very basic attenuation, diffuse and specular
	(defq alpha (elem-get 0 col) col (slice 1 -1 col))
	(bind '(r g b) (vec-min (vec-add (vec-add
		;attenuation
		(vec-scale col (* (n2f at) (const (/ 255.0 3.0))) (const (cat +fixeds_tmp3)))
		;difuse
		(vec-scale col 64.0 (const (cat +fixeds_tmp3))) (const (cat +fixeds_tmp3)))
		;specular
		(vec-scale (const (fixeds 255.0 255.0 255.0)) (n2f sp) (const (cat +fixeds_tmp3))) +fixeds_tmp3)
		;clamp
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (n2i (* alpha 255.0)) 24) (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun render-object-verts (size mat4x4_proj object)
	(defq sp (* +real_1/2 (n2r (dec size)))
		proj_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) (. object :get_verts))
		col (. object :get_color))
	(each (lambda ((x y z w))
			(defq rw (recip w) z (* z rw))
			(when (<= +real_-1 z +real_1)
				(defq x (* x rw) y (* y rw) at (recip (+ z +real_2)) r (* (const (n2r 0.02)) sp rw)
					sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
				(push (elem-get (n2i (* (+ z +real_1) (const (n2r (/ +num_poly_buckets 2))))) poly_buckets)
					(list (circle r) (lighting col at) (n2f sx) (n2f sy)))))
		proj_verts))

(defun render-object-tris (size imat4x4_obj mat4x4_proj object)
	(defq verts (. object :get_verts) norms (. object :get_norms) col (. object :get_color)
		cam_pos (mat4x4-vec4-mul imat4x4_obj (const (Vec4-r +real_0 +real_0 +real_0 +real_1)))
		light_vec (mat4x4-vec3-mul imat4x4_obj (const (Vec3-r (n2r 0.55) (n2r 0.55) (n2r -0.55))))
		proj_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) verts)
		screen_verts (cap (length verts) (list))
		screen_zs (cap (length verts) (list))
		ats (cap (length verts) (list))
		sp (* +real_1/2 (n2r (dec size))))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(push screen_zs z)
			(push screen_verts (path (n2f sx) (n2f sy)))
			(push ats at))
		proj_verts)
	(each (lambda ((i0 i1 i2 in))
			(when (and (> (vec-dot (defq n (elem-get in norms)) (vec-sub (elem-get i0 verts) cam_pos)) +real_0)
					(<= +real_-1 (elem-get i1 screen_zs) +real_1)
					(<= +real_-1 (elem-get i2 screen_zs) +real_1)
					(<= +real_-1 (defq z0 (elem-get i0 screen_zs)) +real_1))
				(defq sp (max +real_0 (vec-dot n light_vec))
					sp (* sp sp sp sp sp sp)
					at (+ (elem-get i0 ats) (elem-get i1 ats) (elem-get i2 ats))
					bucket (n2i (* (+ z0 +real_1) (const (n2r (/ +num_poly_buckets 2))))))
				(push (elem-get bucket poly_buckets) (list
					(cat (elem-get i0 screen_verts) (elem-get i1 screen_verts) (elem-get i2 screen_verts))
					(lighting-at3 col at sp)))))
		(. object :get_tris)))

(defclass Scene-node (&optional name) nil
	; (Scene-node &optional name) -> scene_node
	(def this
		:children (list) :parent nil
		:rot_x +real_0 :rot_y +real_0 :rot_z +real_0
		:trans_x +real_0 :trans_y +real_0 :trans_z +real_0
		:scale_x +real_1 :scale_y +real_1 :scale_z +real_1
		:matrix (Mat4x4-unity) :imatrix (Mat4x4-unity)
		:dirty nil :name (opt name ""))

	(defmethod :get_parent (this)
		; (. scene_node :get_parent) -> scene_node | nil
		(obj-ref (get :parent this)))

	(defmethod :set_parent (this parent)
		; (. scene_node :set_parent parent) -> scene_node
		(set this :parent (weak-ref parent))
		this)

	(defmethod :children (this)
		; (. scene_node :children) -> children
		(get :children this))

	(defmethod :add_node (this child)
		; (. scene_node :add_node child) -> scene_node
		(if (. child :get_parent) (. child :sub_node))
		(push (get :children this) child)
		(. child :set_parent this)
		this)

	(defmethod :sub_node (this)
		; (. scene_node :sub_node) -> scene_node
		(when (defq parent (. child :get_parent))
			(. child :set_parent nil)
			(defq children (get :children parent) idx (find child children))
			(set parent :children (erase children idx (inc idx))))
		this)

	(defmethod :set_rotation (this x y z)
		; (. scene_node :set_rotation x y z) -> scene_node
		(set this :rot_x x :rot_y y :rot_z z :dirty t)
		this)

	(defmethod :set_translation (this x y z)
		; (. scene_node :set_translation x y z) -> scene_node
		(set this :trans_x x :trans_y y :trans_z z :dirty t)
		this)

	(defmethod :set_scale (this x y z)
		; (. scene_node :set_scale x y z) -> scene_node
		(set this :scale_x x :scale_y y :scale_z z :dirty t)
		this)

	(defmethod :get_matrix (this)
		; (. scene_node :get_matrix) -> matrix
		(when (get :dirty this)
			(raise :rot_x :rot_y :rot_z
				:trans_x :trans_y :trans_z
				:scale_x :scale_y :scale_z)
			(set this :dirty nil :matrix
				(mat4x4-mul (Mat4x4-translate trans_x trans_y trans_z)
				(mat4x4-mul (Mat4x4-rotz rot_z)
				(mat4x4-mul (Mat4x4-roty rot_y)
				(mat4x4-mul (Mat4x4-rotx rot_x)
				(Mat4x4-scale scale_x scale_y scale_z)))))))
		(get :matrix this))

	(defmethod :walk_nodes (_this _fi _fo)
		; (. scene_node :walk_nodes fnc_in fnc_out) -> scene_node
		;if fnc_in returns nil, it will step down into that nodes children.
		;fnc_out is allways called to balence calls to fnc_in.
		(defq _stack (list _this (get :children _this) 0))
		(while (defq _idx (pop _stack) _children (pop _stack) _this (pop _stack))
			(cond
				((and (= _idx 0) (_fi _this _children))
					;not stepping in
					(_fo _this _children))
				((< _idx (length _children))
					;step into child node
					(push _stack _this _children (inc _idx)
						(defq _this (elem-get _idx _children)) (get :children _this) 0))
				(t  ;step out
					(_fo _this _children))))
		this)

	(defmethod :find_nodes (this name)
		; (. scene_node :find_nodes name) -> nodes
		(defq nodes (list))
		(. this :walk_nodes
			(lambda (object children)
				(if (starts-with name (get :name object))
					(push nodes object))
				nil)
			(lambda (object children)))
		nodes)
	)

(defclass Scene-object (mesh color &optional name) (Scene-node name)
	; (Scene-object mesh color &optional name) -> scene_node
	(def this :mesh mesh :color color)

	(defmethod :get_mesh (this)
		; (. scene_node :get_mesh) -> mesh
		(get :mesh this))

	(defmethod :set_mesh (this mesh)
		; (. scene_node :set_mesh mesh) -> scene_node
		(lower :mesh) this)

	(defmethod :get_verts (this)
		; (. scene_node :get_verts) -> verts
		(. (get :mesh this) :get_verts))

	(defmethod :get_norms (this)
		; (. scene_node :get_norms) -> norms
		(. (get :mesh this) :get_norms))

	(defmethod :get_tris (this)
		; (. scene_node :get_tris) -> tris
		(. (get :mesh this) :get_tris))

	(defmethod :get_color (this)
		; (. scene_node :get_color) -> color
		(get :color this))
	)

(defclass Scene (&optional name) (Scene-node name)
	; (Scene &optional name) -> scene_node
	(def this :poly_buckets (defq poly_buckets (cap +num_poly_buckets (list))))
	(times +num_poly_buckets (push poly_buckets (list)))

	(defmethod :render (this canvas size left right top bottom near far mode)
		; (. scene_node :render canvas size left right top bottom near far) -> scene_node
		(. canvas :fill 0)
		(defq mat4x4_frust (Mat4x4-frustum left right top bottom near far)
			matrix_stack (list (Mat4x4-unity)))
		(raise :poly_buckets)
		(. scene :walk_nodes
			(lambda (object children)
				(defq mat4x4_obj (mat4x4-mul (elem-get -2 matrix_stack) (. object :get_matrix))
					imat4x4_obj (mat4x4-invert mat4x4_obj))
				(push matrix_stack mat4x4_obj)
				(when (def? :mesh object)
					(defq mat4x4_proj (mat4x4-mul mat4x4_frust mat4x4_obj))
					(if mode
						(render-object-tris size imat4x4_obj mat4x4_proj object)
						(render-object-verts size mat4x4_proj object))
					(task-slice))
				nil)
			(lambda (object children)
				(pop matrix_stack)))
		(each-rev (lambda (path_list)
				(while (defq p (pop path_list))
					(bind '(p col &optional sx sy) p)
					(. canvas :set_color col)
					(if sx
						(. canvas :fpoly sx sy +winding_odd_even (list p))
						(. canvas :ftri p)))
				(task-slice))
			poly_buckets)
		(. canvas :swap)
		this)
	)

;module
(export-classes
	Scene-node Scene-object Scene)
(env-pop)
