;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Backdrop () (View)
	; (Backdrop) -> backdrop
	(def this :style :nil :spacing 32)

	(defmethod :draw (this)
		; (. backdrop :draw) -> backdrop
		(bind '(w h) (. this :get_size))
		(raise :spacing)
		(.-> this
			(:ctx_set_color (get :color this))
			(:ctx_filled_box 0 0 w h)
			(:ctx_set_color (get :ink_color this)))
		(case (get :style this)
			(:grid
				(defq x (- (% (>> w 1) spacing) spacing) y 0)
				(while (< x w)
					(. this :ctx_filled_box x y 1 h)
					(setq x (+ x spacing)))
				(setq y (- (% (>> h 1) spacing) spacing) x 0)
				(while (< y h)
					(. this :ctx_filled_box x y w 1)
					(setq y (+ y spacing))))
			(:axis
				(.-> this
					(:ctx_filled_box (>> w 1) 0 1 h)
					(:ctx_filled_box 0 (>> h 1) w 1))
				(defq x (- (% (>> w 1) spacing) spacing)
					y (- (>> h 1) (/ spacing 2)))
				(while (< x w)
					(. this :ctx_filled_box x y 1 spacing)
					(setq x (+ x spacing)))
				(setq y (- (% (>> h 1) spacing) spacing)
					x (- (>> w 1) (/ spacing 2)))
				(while (< y h)
					(. this :ctx_filled_box x y spacing 1)
					(setq y (+ y spacing))))
			(:lines
				(defq y 0)
				(while (< y h)
					(. this :ctx_filled_box 0 y w 1)
					(setq y (+ y spacing)))))
		this)
	)
