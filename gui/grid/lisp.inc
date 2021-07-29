;;;;;;;;;;;;;
; grid widget
;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Grid () (View)
	; (Grid) -> grid

	(defmethod :pref_size (this)
		; (. grid :pref_size) -> (width height)
		(defq pw 0 ph 0)
		(each (lambda (child)
			(bind '(w h) (. child :pref_size))
			(setq pw (max w pw) ph (max h ph))) (. this :children))
		(list (* (get :grid_width this) pw) (* (get :grid_height this) ph)))

	(defmethod :layout (this)
		; (. grid :layout) -> grid
		(bind '(w h) (. this :get_size))
		(raise :grid_width :grid_height
			(w (<< w +fp_shift) h (<< h +fp_shift)))
		(each (lambda (child)
			(defq row (/ _ grid_width) col (% _ grid_width)
				x (>> (/ (* col w) grid_width) +fp_shift)
				y (>> (/ (* row h) grid_height) +fp_shift)
				x1 (>> (/ (* (inc col) w) grid_width) +fp_shift)
				y1 (>> (/ (* (inc row) h) grid_height) +fp_shift))
			(. child :change x y (- x1 x) (- y1 y))) (. this :children))
		this)
	)
