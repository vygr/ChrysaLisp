;;;;;;;;;;;;;
; scene class
;;;;;;;;;;;;;

(import "lib/math/mesh.inc")

;module
(env-push)

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* (r2f r) 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))))

(defun fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly (r2f x) (r2f y) +winding_odd_even _)))

(defun fpoly-zero (canvas col _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly 0.0 0.0 +winding_odd_even _)))

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

(defun sort-verts (verts)
	(sort (lambda (v1 v2)
		(if (<= (elem +vec4_w v1) (elem +vec4_w v2)) 1 -1)) verts))

(defun sorted-tris (tris prog_verts)
	(sorted (lambda (t1 t2)
		(if (<= (elem +vec4_w (elem (elem +tri_i_v0 t1) prog_verts))
				(elem +vec4_w (elem (elem +tri_i_v0 t2) prog_verts)))
			1 -1)) tris))

(defun render-object-verts (canvas size mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec size)))
		prog_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) (. object :get_verts)))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				r (* (const (f2r 0.02)) sp w) sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(fpoly canvas (lighting 1.0 (const (fixeds 1.0 1.0 1.0)) at) sx sy (circle r)))
		(sort-verts prog_verts)))

(defun render-object-tris (canvas size mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec size)))
		obj_verts (map (# (mat4x4-vec4-mul mat4x4_obj %0)) (. object :get_verts))
		obj_norms (map (# (mat4x4-vec3-mul mat4x4_obj %0)) (. object :get_norms))
		prog_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0)) (. object :get_verts))
		screen_verts (cap (length obj_verts) (list))
		ats (cap (length obj_verts) (list)))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(push screen_verts (path (r2f sx) (r2f sy)))
			(push ats at))
		prog_verts)
	(each (lambda ((i0 i1 i2 in))
			(defq v0 (elem i0 obj_verts) v1 (elem i1 obj_verts)
				v2 (elem i2 obj_verts) n (elem in obj_norms)
				at (+ (elem i0 ats) (elem i1 ats) (elem i2 ats)))
			(if (> (vec-dot n v0) +real_0)
				(fpoly-zero canvas (lighting-at3 1.0 (. object :get_color) at)
					(list (cat (elem i0 screen_verts) (elem i1 screen_verts) (elem i2 screen_verts))))))
		(sorted-tris (elem +mesh_tris (. object :get_mesh)) prog_verts)))

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
		(. main_widget :fill 0)
		(defq mat4x4_frust (Mat4x4-frustum left right top bottom near far)
			matrix_stack (list (Mat4x4-unity)))
		(. scene :walk_nodes
			(lambda (object children)
				(push matrix_stack (defq mat4x4_obj (mat4x4-mul
					(elem -2 matrix_stack) (. object :get_matrix))))
				(when (def? :mesh object)
					(defq mat4x4_proj (mat4x4-mul mat4x4_frust mat4x4_obj))
					(if mode
						(render-object-tris canvas size mat4x4_obj mat4x4_proj object)
						(render-object-verts canvas size mat4x4_obj mat4x4_proj object)))
				nil)
			(lambda (object children)
				(pop matrix_stack)))
		(. main_widget :swap))
	)

;module
(export (penv)
	*class_Scene-node* Scene-node Scene-node?
	*class_Scene-object* Scene-object Scene-object?
	*class_Scene* Scene Scene?)
(env-pop)
