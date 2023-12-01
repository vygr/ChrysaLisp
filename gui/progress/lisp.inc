;;;;;;;;;;;;;;;;;
; progress widget
;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Progress () (View)
	; (Progress) -> progress

	(defmethod :draw ()
		; (. progress :draw) -> progress

		;border
		(bind '(w h) (. this :get_size))
		(.-> this (:ctx_set_color (defq col (get :color this))) (:ctx_box 0 0 w h))
		;middle left
		(defq gap (/ (* (get :value this) (- w 2)) (get :maximum this)))
		(.-> this (:ctx_set_color (setq col (canvas-darker col))) (:ctx_filled_box 1 1 gap (- h 2)))
		;middle right
		(.-> this (:ctx_set_color (canvas-darker col)) (:ctx_filled_box (inc gap) 1 (- w 2 gap) (- h 2))))

	(defmethod :pref_size ()
		; (. slider :pref_size) -> (width height)
		(defq mw (def? :min_width this) mh (def? :min_height this))
		(setd mw 256 mh 10)
		(list mw mh))
	)
