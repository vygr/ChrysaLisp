;;;;;;;;;;;;;
; node widget
;;;;;;;;;;;;;

(import "././text/lisp.inc")

(defclass Node () (Text)
	; (Node) -> node
	(undef this :color)

	(defmethod :mouse_down (event)
		; (. node :mouse_down event) -> node
		(raise :state)
		(setq state (logior +text_state_pressed state))
		(lower :state)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_up (event)
		; (. node :mouse_up event) -> node
		(raise :state)
		(when (/= (logand state +text_state_pressed) 0)
			(setq state (logand (lognot +text_state_pressed) state))
			(lower :state)
			(.-> this :layout :dirty_all :emit))
		this)

	(defmethod :mouse_move (event)
		; (. node :mouse_move event) -> node
		(raise :state)
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			s (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) +text_state_pressed 0))
		(when (/= s (logand +text_state_pressed state))
			(lower (:state (logior s (logand (lognot +text_state_pressed) state))))
			(.-> this :layout :dirty_all))
		this)
	)
