;;;;;;;;;;;;;;;;;;;;;;;
; stroke capture widget
;;;;;;;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Stroke () (View)
	(def (. this :set_flags +view_flag_solid +view_flag_solid)
		:strokes (list) :states (list) :color 0)

	(defgetmethod strokes)
	(defgetmethod states)

	(defmethod :clear ()
		; (. stroke :clear) -> stroke
		(raise :strokes :states)
		(defq out_strokes (list) out_states (list))
		(each (# (unless %1
				(push out_strokes %0)
				(push out_states %1))) strokes states)
		(lower (:strokes out_strokes :states out_states))
		this)

	(defmethod :mouse_down (event)
		; (. stroke :mouse_down event) -> stroke
		(raise :strokes :states)
		(push strokes (array
			(getf event +ev_msg_mouse_rx)
			(getf event +ev_msg_mouse_ry)))
		(push states :nil)
		(. this :emit))

	(defmethod :mouse_up (event)
		; (. stroke :mouse_up event) -> stroke
		(raise :strokes :states)
		(push (last strokes)
			(getf event +ev_msg_mouse_rx)
			(getf event +ev_msg_mouse_ry))
		(elem-set -2 states :t)
		(. this :emit))

	(defmethod :mouse_move (event)
		; (. stroke :mouse_move event) -> stroke
		(raise :strokes)
		(push (last strokes)
			(getf event +ev_msg_mouse_rx)
			(getf event +ev_msg_mouse_ry))
		(. this :emit))
	)
