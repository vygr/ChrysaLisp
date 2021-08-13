;;;;;;;;;;;;;
; scene class
;;;;;;;;;;;;;

(import "lib/math/mesh.inc")

;module
(env-push)

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
			(raise :rot_x :rot_y :rot_z :trans_x :trans_y :trans_z :scale_x :scale_y :scale_z)
			(set this :dirty nil :matrix
				(mat4x4-mul (Mat4x4-translate trans_x trans_y trans_z)
					(mat4x4-mul (mat4x4-mul (mat4x4-mul
						(Mat4x4-rotx rot_x) (Mat4x4-roty rot_y)) (Mat4x4-rotz rot_z))
							(Mat4x4-scale scale_x scale_y scale_z)))))
		(get :matrix this))

	(defmethod :walk_nodes (_this _fi _fo)
		; (. scene_node :walk_nodes fnc_node fnc_in fnc_out)
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

;module
(export (penv)
	*class_Scene-node* Scene-node Scene-node?
	*class_Scene-object* Scene-object Scene-object?)
(env-pop)
