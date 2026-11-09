;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/figure.inc - a node of a scene of vector shapes
;
; A figure is a flat thing, an outline with a fill and a stroke, or nothing
; at all and only a group of the figures under it. Each is placed in the
; space of its parent, x y z, turned on all three axes and scaled, about a
; pivot of its own. Its attributes are properties, all :fig_ so that they can't
; clash with what an app keeps on it, what changes them is its components,
; lib/motion/comps.inc, and anything can read or set them
; between ticks with (get) and (def). What is worked out from them is kept
; until they change: its matrix, by (. figure :set_place) once it has been on a
; tick, its flattening by (. figure :set_outline), :set_width and :set_trim.
;
; It lives on its own turn, as a Sprite does in the Onslaught game. A figure
; told to go, (:kill), goes at the end of the tick. Things it fires, a
; component ending, a collision, can add components and figures as they like.
;
; The tree has the method names of Scene-node, lib/math/scene.inc, the same
; thing in 3D with meshes on it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././math/matrix.inc")
(import "gui/canvas/struct.inc")
(import "./morph.inc")
(import "././collections/tree_node.inc")

;a component's turn, its env pushed so what it keeps are plain variables
(defmacro comp-turn (comp figure dt)
	; (comp-turn comp figure dt) -> :t | :nil
	(defq going (gensym))
	`(progn
		(defq ,going :nil)
		(env-push ,comp)
		(setq ,going ((get :update ,comp) ,figure ,dt))
		(env-pop)
		,going))

(defun comp-over (comp figure)
	; (comp-over comp figure)
	;what it was told to do when it is over
	(if (defq then (get 'then comp)) (then figure comp)))

;each figure's own number, to know a pair of them by
(defq *figure_ids* 0)

(defclass Figure (&optional name) (Tree-node name)
	; (Figure [name]) -> figure
	;a node of a tree of figures, Tree-node, and how it looks and moves
	(def this
		:fig_id (++ *figure_ids*)
		:fig_comps (list) :fig_dead :nil :fig_on_death :nil
		;where it is
		:fig_x 0.0 :fig_y 0.0 :fig_z 0.0 :fig_rx 0.0 :fig_ry 0.0 :fig_rz 0.0 :fig_sx 1.0 :fig_sy 1.0 :fig_px 0.0 :fig_py 0.0
		;how it looks. A colour of 0 is none. trim is how much of its stroke
		;is drawn, along each of its lines
		:fig_alpha 1.0 :fig_fill 0 :fig_stroke 0 :fig_width 0.0 :fig_trim 1.0 :fig_rule +winding_odd_even
		:fig_join +join_round :fig_cap1 +cap_round :fig_cap2 +cap_round :fig_outline (list) :fig_flat :nil
		;what it is, what it looks out for, and what is fired when it does
		:fig_kinds 0 :fig_meets 0 :fig_on_enter :nil :fig_on_stay :nil :fig_on_leave :nil
		;worked out by the rostrum, each tick
		:fig_world :nil :fig_world_alpha 1.0 :fig_world_by :nil :fig_local :nil)

	(defmethod :add (&rest comps)
		; (. figure :add comp ...) -> figure
		;components, run from the next time this figure has its turn
		(each (# (push (get :fig_comps this) %0)) comps)
		this)

	(defmethod :set_place (&rest key_vals)
		; (. figure :set_place key val ...) -> figure
		;any of :fig_x :fig_y :fig_z :fig_rx :fig_ry :fig_rz :fig_sx :fig_sy
		;:fig_px :fig_py. The matrix kept is dropped, a (def) would not be seen
		(each (lambda ((key val)) (def this key val)) (partition key_vals 2))
		(def this :fig_local :nil)
		this)

	(defmethod :set_outline (outline)
		; (. figure :set_outline outline) -> figure
		(def this :fig_outline outline :fig_flat :nil)
		this)

	(defmethod :set_width (width)
		; (. figure :set_width width) -> figure
		(def this :fig_width width :fig_flat :nil)
		this)

	(defmethod :set_trim (trim)
		; (. figure :set_trim trim) -> figure
		(def this :fig_trim trim :fig_flat :nil)
		this)

	(defmethod :kill ()
		; (. figure :kill) -> figure
		;it goes at the end of this tick, and all under it
		(def this :fig_dead :t)
		this)

	(defmethod :flat ()
		; (. figure :flat) -> (fills strokes box)
		;the polygons it is drawn with, in its own space, and the box they
		;are all in, :nil if there is nothing, (outline-flat). Worked out
		;once and kept
		(unless (defq flat (get :fig_flat this))
			(raise :fig_outline :fig_width :fig_trim :fig_join :fig_cap1 :fig_cap2 :fig_fill :fig_stroke)
			(def this :fig_flat (setq flat (outline-flat fig_outline fig_fill fig_stroke (* 0.5 fig_width)
						fig_join fig_cap1 fig_cap2 fig_trim))))
		flat)

	(defmethod :local ()
		; (. figure :local) -> matrix
		;where it is in the space of its parent. Worked out once and kept
		(unless (defq local (get :fig_local this))
			(raise :fig_x :fig_y :fig_z :fig_rx :fig_ry :fig_rz :fig_sx :fig_sy :fig_px :fig_py)
			(setq local (mat3x4-place fig_x fig_y fig_z fig_rx fig_ry fig_rz fig_sx fig_sy fig_px fig_py))
			(def this :fig_local local))
		local)

	(defmethod :update (dt)
		; (. figure :update dt) -> figure
		;its components have their turn, dt seconds since the last. Those
		;that say they are over go, then what they fire is fired, so a
		;figure's list is not changed while it is being gone through
		(defq comps (get :fig_comps this))
		(when (nempty? comps)
			(defq over :nil)
			(def this :fig_comps (list))
			(each (lambda (comp)
					(if (and (not (get :fig_dead this)) (comp-turn comp this dt))
						(push (get :fig_comps this) comp)
						(push (or over (setq over (list))) comp)))
				comps)
			(when over
				(each (lambda (comp) (comp-over comp this)) over)))
		this))
