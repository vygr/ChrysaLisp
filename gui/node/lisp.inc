;;;;;;;;;;;;;
; node widget
;;;;;;;;;;;;;

(import "././text/lisp.inc")

(defclass Node () (Text)
	; (Node) -> node
	(def this :state 1)

	(defmethod :draw ()
		; (. text :draw) -> text
		(bind '(w h) (. this :get_size))
		(.super (.-> this (:ctx_set_color (get :color this))
			(:ctx_filled_box 0 0 w h)) :draw))

	(defmethod :layout ()
		; (. node :layout) -> node
		(if (= (>> (get :color this) 24) 0xff)
			(. this :set_flags +view_flag_opaque +view_flag_opaque)
			this))

	(defmethod :mouse_down (event)
		; (. node :mouse_down event) -> node
		(def this :state -1)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_up (event)
		; (. node :mouse_up event) -> node
		(when (/= (get :state this) 1)
			(def this :state 1)
			(.-> this :layout :dirty_all :emit))
		this)

	(defmethod :mouse_move (event)
		; (. node :mouse_move event) -> node
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			state (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) -1 1))
		(when (/= state (get :state this))
			(def this :state state)
			(.-> this :layout :dirty_all))
		this)
	)
