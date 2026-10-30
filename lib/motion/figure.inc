;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/figure.inc - a node of a scene of vector shapes
;
; A figure is a flat thing, an outline with a fill and a stroke, or nothing
; at all and only a group of the figures under it. Each is placed in the
; space of its parent, x y z, turned on all three axes and scaled, about a
; pivot of its own. Its attributes are properties, what changes them is its
; components, lib/motion/comps.inc, and anything can read or set them
; between ticks with (get) and (def). A kept flattening is dropped when the
; outline or the stroke's width is, by (. figure :set_outline) and :set_width.
;
; It lives on its own turn, as a Sprite does in the Onslaught game. A figure
; told to go, (:kill), goes at the end of the tick. Things it fires, a
; component ending, a collision, can add components and figures as they like.
;
; The tree has the method names of Scene-node, lib/math/scene.inc, the same
; thing in 3D with meshes on it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/matrix.inc")
(import "gui/canvas/struct.inc")
(import "./morph.inc")

;each figure's own number, to know a pair of them by
(defq *figure_ids* 0)

(defclass Figure (&optional name) :nil
	; (Figure [name]) -> figure
	(def this
		:children (list) :parent :nil :name (ifn name "") :id (++ *figure_ids*)
		:comps (list) :dead :nil :on_death :nil
		;where it is
		:x 0.0 :y 0.0 :z 0.0 :rx 0.0 :ry 0.0 :rz 0.0 :sx 1.0 :sy 1.0 :px 0.0 :py 0.0
		;how it looks. A colour of 0 is none. trim is how much of its stroke
		;is drawn, along each of its lines
		:alpha 1.0 :fill 0 :stroke 0 :width 0.0 :trim 1.0 :rule +winding_odd_even
		:join +join_round :cap1 +cap_round :cap2 +cap_round :outline (list) :flat :nil
		;what it is, what it looks out for, and what is fired when it does
		:kinds 0 :meets 0 :on_enter :nil :on_stay :nil :on_leave :nil
		;worked out by the rostrum, each tick
		:world :nil :world_alpha 1.0)

	(defmethod :get_parent ()
		; (. figure :get_parent) -> figure | :nil
		(obj-ref (get :parent this)))

	(defmethod :set_parent (parent)
		; (. figure :set_parent parent) -> figure
		(def this :parent (weak-ref parent))
		this)

	(defmethod :children ()
		; (. figure :children) -> children
		(get :children this))

	(defmethod :add_node (child)
		; (. figure :add_node child) -> figure
		(if (. child :get_parent) (. child :sub_node))
		(push (get :children this) child)
		(. child :set_parent this)
		this)

	(defmethod :sub_node ()
		; (. figure :sub_node) -> figure
		;this one taken out of its parent
		(when (defq parent (. this :get_parent))
			(. this :set_parent :nil)
			(defq children (get :children parent) idx (find this children))
			(if idx (def parent :children (erase children idx (inc idx)))))
		this)

	(defmethod :root ()
		; (. figure :root) -> figure
		;the top of the tree it is in, the rostrum if it is on one
		(defq node this)
		(while (defq up (. node :get_parent)) (setq node up))
		node)

	(defmethod :walk_nodes (fi fo)
		; (. figure :walk_nodes fnc_in fnc_out) -> figure
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
		; (. figure :find_nodes name) -> nodes
		(defq nodes (list))
		(. this :walk_nodes
			(lambda (object children)
				(if (starts-with name (get :name object))
					(push nodes object))
				:nil)
			(lambda (object children)))
		nodes)

	(defmethod :add (&rest comps)
		; (. figure :add comp ...) -> figure
		;components, run from the next time this figure has its turn
		(each (# (push (get :comps this) %0)) comps)
		this)

	(defmethod :set_outline (outline)
		; (. figure :set_outline outline) -> figure
		(def this :outline outline :flat :nil)
		this)

	(defmethod :set_width (width)
		; (. figure :set_width width) -> figure
		(def this :width width :flat :nil)
		this)

	(defmethod :set_trim (trim)
		; (. figure :set_trim trim) -> figure
		(def this :trim trim :flat :nil)
		this)

	(defmethod :kill ()
		; (. figure :kill) -> figure
		;it goes at the end of this tick, and all under it
		(def this :dead :t)
		this)

	(defmethod :flat ()
		; (. figure :flat) -> (fills strokes box)
		;the polygons it is drawn with, in its own space, and the box they
		;are all in, :nil if there is nothing. Worked out once and kept
		(unless (defq flat (get :flat this))
			(raise :outline :width :trim :join :cap1 :cap2 :fill :stroke)
			(defq fills (list) strokes (list) radius (* 0.5 width))
			(if (/= fill 0)
				(each (lambda ((closed p)) (if (>= (length p) 6) (push fills p))) outline))
			(and (/= stroke 0) (> radius 0.0)
				(each (lambda ((closed p))
						(when (>= (length p) 4)
							(cond
								((< trim 1.0)
									(push strokes (path-stroke-polyline
										(morph-trim p closed (max trim 0.0)) radius join cap1 cap2)))
								(closed
									(bind '(p1 p2) (path-stroke-polygon p radius join))
									(push strokes p1 p2))
								(:t (push strokes (path-stroke-polyline p radius join cap1 cap2))))))
					outline))
			(setq flat (list fills strokes
				(outline-box (map (# (list :t %0)) (cat fills strokes)))))
			(def this :flat flat))
		flat)

	(defmethod :local ()
		; (. figure :local) -> matrix
		;where it is in the space of its parent
		(raise :x :y :z :rx :ry :rz :sx :sy :px :py)
		(Mat3x4-place x y z rx ry rz sx sy px py))

	(defmethod :update (dt)
		; (. figure :update dt) -> figure
		;its components have their turn, dt seconds since the last. Those
		;that say they are over go, then what they fire is fired, so a
		;figure's list is not changed while it is being gone through
		(defq comps (get :comps this))
		(when (nempty? comps)
			(defq over :nil)
			(def this :comps (list))
			(each (lambda (comp)
					(if (and (not (get :dead this)) (. comp :update this dt))
						(push (get :comps this) comp)
						(push (or over (setq over (list))) comp)))
				comps)
			(when over
				(each (lambda (comp) (. comp :over this)) over)))
		this)
	)
