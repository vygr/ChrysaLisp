;;;;;;;;;;;;;
; grid widget
;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

(defclass Grid () (View)
	; (Grid) -> grid

	(defmethod :pref_size (this)
		; (. grid :pref_size) -> (width height)
		(raise :grid_width :grid_height
			(pw 0 ph 0 children (. this :children)))
		(if (= grid_width 0)
			(setq grid_width (inc (/ (dec (length children)) grid_height))))
		(if (= grid_height 0)
			(setq grid_height (inc (/ (dec (length children)) grid_width))))
		(each (lambda (child)
			(bind '(w h) (. child :pref_size))
			(setq pw (max w pw) ph (max h ph))) children)
		(list (* grid_width pw) (* grid_height ph)))

	(defmethod :layout (this)
		; (. grid :layout) -> grid
		(bind '(w h) (. this :get_size))
		(raise :grid_width :grid_height
			(w (<< w +fp_shift) h (<< h +fp_shift) children (. this :children)))
		(if (= grid_width 0)
			(setq grid_width (inc (/ (dec (length children)) grid_height))))
		(if (= grid_height 0)
			(setq grid_height (inc (/ (dec (length children)) grid_width))))
		(each (lambda (child)
			(defq row (/ _ grid_width) col (% _ grid_width)
				x (>> (/ (* col w) grid_width) +fp_shift)
				y (>> (/ (* row h) grid_height) +fp_shift)
				x1 (>> (/ (* (inc col) w) grid_width) +fp_shift)
				y1 (>> (/ (* (inc row) h) grid_height) +fp_shift))
			(. child :change x y (- x1 x) (- y1 y))) children)
		this)
	)
