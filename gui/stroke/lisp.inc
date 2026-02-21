;;;;;;;;;;;;;;;;;;;;;;;
; stroke capture widget
;;;;;;;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(env-push)

(defun snap (v s)
	(if (= s 0) v (* (/ (+ v (/ s 2)) s) s)))

;multiple potential strokes, each having a state !
;we could be multi touch here !

(defclass Stroke () (View)
	(def (. this :set_flags +view_flag_solid +view_flag_solid)
		:strokes (list) :states (list) :color 0 :snap_x 0 :snap_y 0)

	(defgetmethod :strokes)
	(defgetmethod :states)

	(defmethod :clear ()
		; (. stroke :clear) -> stroke
		;clear out finished strokes
		(raise :strokes :states)
		(defq out_strokes (list) out_states (list))
		(each (# (when (= (abs %1) 1)
			(push out_strokes %0)
			(push out_states %1))) strokes states)
		(lower (:strokes out_strokes :states out_states))
		this)

	(defmethod :set_snap (snap_x snap_y)
		; (. stroke :set_snap x y) -> stroke
		(lower :snap_x :snap_y)
		this)

	(defmethod :mouse_down (event)
		; (. stroke :mouse_down event) -> stroke
		(raise :strokes :states :snap_x :snap_y)
		(push strokes (nums
			(snap (getf event +ev_msg_mouse_rx) snap_x)
			(snap (getf event +ev_msg_mouse_ry) snap_y)))
		(push states (if (bits? (getf event +ev_msg_mouse_buttons) 2) -1 1))
		(lower :strokes :states)
		(. this :emit))

	(defmethod :mouse_up (event)
		; (. stroke :mouse_up event) -> stroke
		(raise :strokes :states :snap_x :snap_y)
		(push (last strokes) (nums
			(snap (getf event +ev_msg_mouse_rx) snap_x)
			(snap (getf event +ev_msg_mouse_ry) snap_y)))
		(elem-set states -2 (* (last states) 2))
		(. this :emit))

	(defmethod :mouse_move (event)
		; (. stroke :mouse_move event) -> stroke
		(raise :strokes :snap_x :snap_y)
		(defq p (nums
			(snap (getf event +ev_msg_mouse_rx) snap_x)
			(snap (getf event +ev_msg_mouse_ry) snap_y)))
		(when (nql p (slice (last strokes) -3 -1))
			(push (last strokes) p)
			(. this :emit))
		this)
	)

(export-classes '(Stroke))
(env-pop)
