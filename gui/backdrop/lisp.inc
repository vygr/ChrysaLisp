;;;;;;;;;;;;;;;;;
; backdrop widget
;;;;;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defun backdrop-lines (origin gap extent)
	; (backdrop-lines origin gap extent) -> (pos ...)
	;where the lines of a ruling fall across an extent of pixels, 0 up to
	;it: one is at origin, and the rest are gap apart each way from it.
	;origin and gap can be whole numbers or have parts, a line is on the
	;pixel nearest where it falls. None if they are too close to tell apart
	(defq out (list) gap (n2f gap) origin (n2f origin))
	(when (>= gap 2.0)
		;the first that is not before 0
		(defq at (- origin (* gap (floor (/ origin gap)))))
		(if (>= at gap) (setq at (- at gap)))
		(while (< (defq pos (n2i (floor (+ at 0.5)))) extent)
			(push out pos)
			(setq at (+ at gap))))
	out)

(defclass Backdrop () (View)
	; (Backdrop) -> backdrop
	;a view of one colour, :paper, with lines on it by a :style, :grid
	;:lines :axis or none, :spacing apart, in :ink.
	;
	;That much is all most want. More, each only if it is given:
	;
	;	:origin_x :origin_y	where a line of the grid, or the cross of
	;			the axis, is, in pixels of the view. With none a grid
	;			and an axis are about the middle, lines from the top.
	;			What shows something moved about says here by how far
	;	:rules	((spacing color) ...), more rulings than the one, each
	;			drawn over the last: a coarse one and finer between
	;			it, each as bright as its colour says. A spacing can
	;			have parts of a pixel, and a ruling too fine to see
	;			is left out. In place of :spacing and :ink
	;	:turned	:t and the lines of :lines go up and down
	(def this :style :nil :spacing 32)

	(defmethod :draw ()
		; (. backdrop :draw) -> backdrop
		(bind '(w h) (. this :get_size))
		(defq style (get :style this) spacing (get :spacing this)
			ox (def? :origin_x this) oy (def? :origin_y this))
		(.-> this
			(:ctx_set_color (get :paper this))
			(:ctx_filled_box 0 0 w h)
			(:ctx_set_color (get :ink this)))
		(case style
			((:grid :lines)
				(defq grid (eql style :grid) turned (def? :turned this))
				(each (lambda ((gap col))
					(. this :ctx_set_color col)
					(if (or grid turned)
						(each (# (. this :ctx_filled_box %0 0 1 h))
							(backdrop-lines (ifn ox (if grid (>> w 1) 0)) gap w)))
					(if (or grid (not turned))
						(each (# (. this :ctx_filled_box 0 %0 w 1))
							(backdrop-lines (ifn oy (if grid (>> h 1) 0)) gap h))))
					(ifn (def? :rules this) (list (list spacing (get :ink this))))))
			(:axis
				(defq cx (n2i (ifn ox (>> w 1))) cy (n2i (ifn oy (>> h 1))) half (/ (n2i spacing) 2))
				(.-> this
					(:ctx_filled_box cx 0 1 h)
					(:ctx_filled_box 0 cy w 1))
				(each (# (. this :ctx_filled_box %0 (- cy half) 1 (n2i spacing))) (backdrop-lines cx spacing w))
				(each (# (. this :ctx_filled_box (- cx half) %0 (n2i spacing) 1)) (backdrop-lines cy spacing h))))
		this)
	)
