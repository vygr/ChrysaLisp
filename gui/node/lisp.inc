;;;;;;;;;;;;;
; node widget
;;;;;;;;;;;;;

(import "././text/lisp.inc")
(import "././canvas/lisp.inc")

(defclass Node () (Text)
	; (Node) -> node
	(def this :state 1)
	(undef this :color)

	(defmethod :draw ()
		; (. node :draw) -> node
		(bind '(w h) (. this :get_size))
		(if (defq paper (def? :highlight_color this))
			(if (def? :color this) (setq paper (canvas-darker paper)))
			(setq paper (get :color this)))
		(.super (.-> this (:ctx_set_color paper)
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
