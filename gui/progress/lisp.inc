;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Progress () (View)
	; (Progress) -> progress

	(defmethod :draw ()
		; (. progress :draw) -> progress

		;as a slider is, a trough, a shade of the color with a thin edge a
		;shade darker
		(bind '(w h) (. this :get_size))
		(defq col (get :color this) trough (canvas-darker col)
			gap (/ (* (get :value this) (- w 4)) (get :maximum this)))
		(.-> this (:ctx_set_color (canvas-darker trough)) (:ctx_box 0 0 w h)
			(:ctx_set_color trough) (:ctx_filled_box 1 1 (- w 2) (- h 2))
			;what is done, the color, a pixel clear of the edge
			(:ctx_set_color col) (:ctx_filled_box 2 2 gap (- h 4))))

	(defmethod :constraint ()
		; (. progress :constraint) -> (width height)
		(list (ifn (def? :min_width this) 256) (ifn (def? :min_height this) 10)))
	)
