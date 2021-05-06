;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Backdrop () (View)
	; (Backdrop) -> backdrop
	(def this :style nil :spaceing 32)

	(defmethod :draw (this)
		; (. backdrop :draw) -> backdrop
		(bind '(w h) (. this :get_size))
		(defq spaceing (get :spaceing this))
		(.-> this
			(:ctx_set_color (get :color this))
			(:ctx_filled_box 0 0 w h)
			(:ctx_set_color (get :ink_color this)))
		(case (get :style this)
			(:grid
				(defq x (- (% (>> w 1) spaceing) spaceing) y 0)
				(while (< x w)
					(. this :ctx_filled_box x y 1 h)
					(setq x (+ x spaceing)))
				(setq y (- (% (>> h 1) spaceing) spaceing) x 0)
				(while (< y h)
					(. this :ctx_filled_box x y w 1)
					(setq y (+ y spaceing))))
			(:axis
				(.-> this
					(:ctx_filled_box (>> w 1) 0 1 h)
					(:ctx_filled_box 0 (>> h 1) w 1))
				(defq x (- (% (>> w 1) spaceing) spaceing) y (- (>> h 1) (/ spaceing 2)))
				(while (< x w)
					(. this :ctx_filled_box x y 1 spaceing)
					(setq x (+ x spaceing)))
				(setq y (- (% (>> h 1) spaceing) spaceing) x (- (>> w 1) (/ spaceing 2)))
				(while (< y h)
					(. this :ctx_filled_box x y spaceing 1)
					(setq y (+ y spaceing))))
			(:lines
				(defq y 0)
				(while (< y h)
					(. this :ctx_filled_box 0 y w 1)
					(setq y (+ y spaceing)))))
		this)
	)
