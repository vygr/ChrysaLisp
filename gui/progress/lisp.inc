;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Progress () (View)
	; (Progress) -> progress

	(defmethod :draw ()
		; (. progress :draw) -> progress

		(bind '(w h) (. this :get_size))
		(defq col (get :paper this) gap (/ (* (get :value this) (- w 4)) (get :maximum this)))
		;a trough as a slider has, and what is done the color, a pixel clear
		;of the edge
		(.-> this (:ctx_trough col 0 0 w h)
			(:ctx_set_color col) (:ctx_filled_box 2 2 gap (- h 4))))

	(defmethod :constraint ()
		; (. progress :constraint) -> (width height)
		(list (ifn (def? :min_width this) 256) (ifn (def? :min_height this) 10)))
	)
