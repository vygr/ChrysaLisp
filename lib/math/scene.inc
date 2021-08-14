;;;;;;;;;;;;;
; scene class
;;;;;;;;;;;;;

(import "lib/math/mesh.inc")

;module
(env-push)

(enums +poly 0
	(enum poly col pos))

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* (r2f r) 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))))

(defun lighting (alpha col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vec-min (vec-add (vec-scale col (* (r2f at) 255.0) +fixeds_tmp3)
		(const (fixeds 32.0 32.0 32.0)) +fixeds_tmp3)
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (f2i (* alpha 255.0)) 24) (<< (f2i r) 16) (<< (f2i g) 8) (f2i b)))

(defun lighting-at3 (alpha col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vec-min (vec-add (vec-scale col (* (r2f at) (const (/ 255.0 3.0))) +fixeds_tmp3)
		(const (fixeds 32.0 32.0 32.0)) +fixeds_tmp3)
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (f2i (* alpha 255.0)) 24) (<< (f2i r) 16) (<< (f2i g) 8) (f2i b)))

(defun render-object-verts (size mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec size)))
		proj_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) (. object :get_verts)))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				r (* (const (f2r 0.02)) sp w) sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(push poly_list (list
				(circle r)
				(lighting 1.0 (const (fixeds 1.0 1.0 1.0)) at)
				(list (r2f sx) (r2f sy) w))))
		proj_verts))

(defun render-object-tris (size mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec size)))
		obj_verts (map (# (mat4x4-vec4-mul mat4x4_obj %0)) (. object :get_verts))
		obj_norms (map (# (mat4x4-vec3-mul mat4x4_obj %0)) (. object :get_norms))
		proj_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) (. object :get_verts))
		screen_verts (cap (length obj_verts) (list))
		ats (cap (length obj_verts) (list)))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(push screen_verts (path (r2f sx) (r2f sy)))
			(push ats at))
		proj_verts)
	(each (lambda ((i0 i1 i2 in))
			(defq v0 (elem i0 obj_verts) v1 (elem i1 obj_verts)
				v2 (elem i2 obj_verts) n (elem in obj_norms)
				at (+ (elem i0 ats) (elem i1 ats) (elem i2 ats)))
			(if (> (vec-dot n v0) +real_0)
				(push poly_list (list
					(list (cat (elem i0 screen_verts) (elem i1 screen_verts) (elem i2 screen_verts)))
					(lighting-at3 1.0 (. object :get_color) at)
					(list 0.0 0.0 (elem +vec4_w (elem i0 proj_verts)))))))
		(elem +mesh_tris (. object :get_mesh))))

(defclass Scene-node (&optional name) nil
	; (Scene-node &optional name) -> scene_node
	(def this
		:children (list) :parent nil
		:rot_x +real_0 :rot_y +real_0 :rot_z +real_0
		:trans_x +real_0 :trans_y +real_0 :trans_z +real_0
		:scale_x +real_1 :scale_y +real_1 :scale_z +real_1
		:matrix (Mat4x4-unity) :dirty nil :name (opt name ""))

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
					(mat4x4-mul (mat4x4-mul (mat4x4-mul
						(Mat4x4-rotx rot_x) (Mat4x4-roty rot_y)) (Mat4x4-rotz rot_z))
							(Mat4x4-scale scale_x scale_y scale_z)))))
		(get :matrix this))

	(defmethod :walk_nodes (_this _fi _fo)
		; (. scene_node :walk_nodes fnc_in fnc_out)
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
						(defq _this (elem _idx _children)) (get :children _this) 0))
				(t  ;step out
					(_fo _this _children)))))
	)

(defclass Scene-object (mesh color &optional name) (Scene-node name)
	; (Scene-object mesh color &optional name) -> scene_node
	(def this :mesh mesh :color color)

	(defmethod :get_mesh (this)
		; (. scene_node :get_mesh) -> mesh
		(get :mesh this))

	(defmethod :get_verts (this)
		; (. scene_node :get_verts) -> verts
		(elem +mesh_verts (get :mesh this)))

	(defmethod :get_norms (this)
		; (. scene_node :get_norms) -> norms
		(elem +mesh_norms (get :mesh this)))

	(defmethod :get_tris (this)
		; (. scene_node :get_tris) -> tris
		(elem +mesh_tris (get :mesh this)))

	(defmethod :get_color (this)
		; (. scene_node :get_color) -> color
		(get :color this))
	)

(defclass Scene (&optional name) (Scene-node name)
	; (Scene &optional name) -> scene_node

	(defmethod :render (this canvas size left right top bottom near far mode)
		; (. scene_node :render canvas size left right top bottom near far) -> tris
		(. canvas :fill 0)
		(defq mat4x4_frust (Mat4x4-frustum left right top bottom near far)
			matrix_stack (list (Mat4x4-unity)) poly_list (list))
		(. scene :walk_nodes
			(lambda (object children)
				(push matrix_stack (defq mat4x4_obj (mat4x4-mul
					(elem -2 matrix_stack) (. object :get_matrix))))
				(when (def? :mesh object)
					(defq mat4x4_proj (mat4x4-mul mat4x4_frust mat4x4_obj))
					(if mode
						(render-object-tris size mat4x4_obj mat4x4_proj object)
						(render-object-verts size mat4x4_obj mat4x4_proj object)))
				nil)
			(lambda (object children)
				(pop matrix_stack)))
		(each (lambda ((poly col (sx sy sz)))
				(.-> canvas (:set_color col) (:fpoly sx sy +winding_odd_even poly)))
			(sort (# (if (< (elem +vec3_z (elem +poly_pos %0))
							(elem +vec3_z (elem +poly_pos %1))) 1 -1)) poly_list))
		(. canvas :swap))
	)

;module
(export (penv)
	*class_Scene-node* Scene-node Scene-node?
	*class_Scene-object* Scene-object Scene-object?
	*class_Scene* Scene Scene?)
(env-pop)