;;;;;;;;;;;;;;;
; button widget
;;;;;;;;;;;;;;;

(import "gui/label/lisp.inc")

(defclass Button () (Label)
	; (Button) -> button
	(def this :state 1)

	(defmethod :draw (this)
		; (. button :draw) -> button
		(bind '(w h) (. this :get_size))
		(. this :ctx_panel (get :color this) 1 (* (get :border this) (get :state this)) 0 0 w h))

	(defmethod :layout (this)
		; (. button :layout) -> button
		(defq flow (get :label_flow this) state (get :state this)
			border (get :border this) pos border)
		;contents offset or not
		(if (/= 1 state)
			(setq pos (* pos 2)))
		(bind '(w h) (. this :get_size))
		(.-> flow (:change pos pos (- w (* border 2)) (- h (* border 2))) :layout)
		(. this :set_flags +view_flag_opaque +view_flag_opaque))

	(defmethod :mouse_down (this event)
		; (. button :mouse_down event) -> button
		(def this :state -1)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_up (this event)
		; (. button :mouse_up event) -> button
		(when (/= (get :state this) 1)
			(def this :state 1)
			(.-> this :layout :dirty_all :emit))
		this)

	(defmethod :mouse_move (this event)
		; (. button :mouse_move event) -> button
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) -1 1))
		(when (/= state (get :state this))
			(def this :state state)
			(.-> this :layout :dirty_all))
		this)
	)
