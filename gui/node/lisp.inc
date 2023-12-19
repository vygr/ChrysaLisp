;;;;;;;;;;;;;
; node widget
;;;;;;;;;;;;;

(import "././text/lisp.inc")
(import "././canvas/lisp.inc")

;node flags
(bits +node_state 0
	(bit pressed selected highlight))

(defclass Node () (Text)
	; (Node) -> node
	(def this :state 0)
	(undef this :color)

	(defmethod :draw ()
		; (. node :draw) -> node
		(raise :state :color)
		(bind '(w h) (. this :get_size))
		(if (/= (logand state +node_state_highlight) 0)
			(setq color *env_highlight_col*))
		(if (/= (logand state +node_state_selected) 0)
			(setq color (canvas-darker color)))
		(.super (.-> this (:ctx_set_color color)
			(:ctx_filled_box 0 0 w h)) :draw))

	(defmethod :mouse_down (event)
		; (. node :mouse_down event) -> node
		(raise :state)
		(setq state (logior +node_state_pressed state))
		(lower :state)
		(.-> this :layout :dirty_all))

	(defmethod :mouse_up (event)
		; (. node :mouse_up event) -> node
		(raise :state)
		(when (/= (logand state +node_state_pressed) 0)
			(setq state (logand (lognot +node_state_pressed) state))
			(lower :state)
			(.-> this :layout :dirty_all :emit))
		this)

	(defmethod :mouse_move (event)
		; (. node :mouse_move event) -> node
		(raise :state)
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry)
			s (if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) +node_state_pressed 0))
		(when (/= s (logand +node_state_pressed state))
			(lower (:state (logior s (logand (lognot +node_state_pressed) state))))
			(.-> this :layout :dirty_all))
		this)
	)
