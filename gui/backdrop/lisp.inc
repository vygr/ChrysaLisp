;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Backdrop () (View)
	; (Backdrop)-> backdrop

	(defmethod :draw (this)
		; (. backdrop :draw) -> backdrop
		(bind '(w h) (. this :get_size))
		(.-> this
			(:ctx_set_color (get :color this))
			(:ctx_filled_box 0 0 w h)
			(:ctx_set_color (get :ink_color this)))
		(cond
			((= (defq style (get :style this)) 0)
				;grid
				(defq x (- (% (>> w 1) 32) 32) y 0)
				(while (< x w)
					(. this :ctx_filled_box x y 1 h)
					(setq x (+ x 32)))
				(setq y (- (% (>> h 1) 32) 32) x 0)
				(while (< y h)
					(. this :ctx_filled_box x y w 1)
					(setq y (+ y 32))))
			((= style 1)
				;plain
				)
			((= style 2)
				;axis
				(.-> this
					(:ctx_filled_box (>> w 1) 0 1 h)
					(:ctx_filled_box 0 (>> h 1) w 1))
				(defq x (- (% (>> w 1) 32) 32) y (- (>> h 1) 16))
				(while (< x w)
					(. this :ctx_filled_box x y 1 32)
					(setq x (+ x 32)))
				(setq y (- (% (>> h 1) 32) 32) x (- (>> w 1) 16))
				(while (< y h)
					(. this :ctx_filled_box x y 32 1)
					(setq y (+ y 32))))
			((= style 3)
				;lines
				(defq y 0)
				(while (< y h)
					(. this :ctx_filled_box 0 y w 1)
					(setq y (+ y 32)))))
		this)
	)
