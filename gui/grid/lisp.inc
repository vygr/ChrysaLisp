;;;;;;;;;;;;;
; grid widget
;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Grid () (View)
	; (Grid) -> grid
	(def this :grid_width 0 :grid_height 0)

	(defmethod :constraint ()
		; (. grid :constraint) -> (width height)
		(raise :grid_width :grid_height
			(pw 0 ph 0 children (. this :children)))
		(if (= grid_width 0)
			(setq grid_width (inc (/ (dec (length children)) grid_height))))
		(if (= grid_height 0)
			(setq grid_height (inc (/ (dec (length children)) grid_width))))
		(each (lambda (child)
			(bind '(w h) (. child :get_constraint))
			(setq pw (max w pw) ph (max h ph))) children)
		(list (* grid_width pw) (* grid_height ph)))

	(defmethod :layout ()
		; (. grid :layout) -> grid
		(bind '(w h) (. this :get_size))
		(raise :grid_width :grid_height
			(w (<< w +fp_shift) h (<< h +fp_shift) children (. this :children)))
		(if (= grid_width 0)
			(setq grid_width (inc (/ (dec (length children)) grid_height))))
		(if (= grid_height 0)
			(setq grid_height (inc (/ (dec (length children)) grid_width))))
		(each (lambda (child)
			(defq row (/ (!) grid_width) col (% (!) grid_width)
				x (>> (/ (* col w) grid_width) +fp_shift)
				y (>> (/ (* row h) grid_height) +fp_shift)
				x1 (>> (/ (* (inc col) w) grid_width) +fp_shift)
				y1 (>> (/ (* (inc row) h) grid_height) +fp_shift))
			(. child :set_bounds x y (- x1 x) (- y1 y))) children)
		this)
	)
