;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/swatches.inc - the inks to fill a pen with, a menu on the board
;
; Twelve hues in five shades and a row of greys, how see through, and none.
; A panel, lib/cwb/panel.inc: a tap of any device is for the pen the button
; that tapped holds, and it stays till its cross is tapped. As many as are
; wanted can be open.
;
; A tap is the board's to do, (. board :pick event what val), with the
; event of the tap as the device was when it went down, and what was
; tapped, :ink, :alpha or :none, and its value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./panel.inc")

;a cell, how many across, and how see through an ink can be
(defq +swatch_cell 20.0 +swatch_cols 12
	+swatch_alphas ''(0xff 0xc0 0x80 0x40))

(defun swatch-colors ()
	; (swatch-colors) -> (argb ...)
	;row by row: light to dark, a hue each 30 degrees across, then grey
	(defq out (list))
	(each (lambda ((mix to))
			(each (lambda (h)
					(defq rgb (map (lambda (n)
						(defq k (+ n (* (n2f h) 0.5)) k (- k (* 6.0 (floor (/ k 6.0))))
							c (- 1.0 (max 0.0 (min k (- 4.0 k) 1.0))))
						(n2i (* 255.0 (+ (* c (- 1.0 mix)) (* to mix)))))
						'(5.0 3.0 1.0)))
					(push out (+ 0xff000000 (<< (first rgb) 16) (<< (second rgb) 8) (third rgb))))
				(range 0 +swatch_cols)))
		'((0.6 1.0) (0.3 1.0) (0.0 0.0) (0.3 0.0) (0.6 0.0)))
	(each (# (defq v (/ (* 255 %0) (dec +swatch_cols))) (push out (+ 0xff000000 (<< v 16) (<< v 8) v)))
		(range 0 +swatch_cols))
	out)

(defun swatch-tip (what val &optional long)
	; (swatch-tip what val [long]) -> :nil | str
	;what a cell is, in words, short as the toolbars' tips are; a colour
	;has none, as an ink of the bar has none
	(case what
		(:alpha (cat (if long "opacity " "") (str (/ (+ (* val 100) 127) 255)) "%"))
		(:none (if long "none"))
		(:close (if long "close"))))

(defclass Swatches (board x y) (Panel board x y)
	; (Swatches board x y) -> swatches
	;about a point of the document

	(defmethod :layout ()
		; (. swatches :layout) -> (cells w h)
		;the inks, row by row, then how see through, two cells each, and none
		;at the end of that row
		(defq colors (swatch-colors) n (/ (length colors) +swatch_cols)
			w (* +swatch_cell (n2f +swatch_cols)) cells (list))
		(each (lambda (col)
				(defq cx (* +swatch_cell (n2f (% (!) +swatch_cols))) cy (* +swatch_cell (n2f (/ (!) +swatch_cols))))
				(push cells (list :ink col cx cy (+ cx +swatch_cell) (+ cy +swatch_cell))))
			colors)
		(defq cy (+ (* +swatch_cell (n2f n)) +panel_pad) cy1 (+ cy +swatch_cell))
		(each (lambda (alpha)
				(defq cx (* 2.0 +swatch_cell (n2f (!))))
				(push cells (list :alpha alpha cx cy (+ cx (* 2.0 +swatch_cell)) cy1)))
			+swatch_alphas)
		(push cells (list :none 0 (- w +swatch_cell) cy w cy1))
		(list cells w cy1))

	(defmethod :tap (cell event)
		; (. swatches :tap cell event) -> swatches
		(. (get :board this) :pick event (elem-get cell +cell_what) (elem-get cell +cell_val))
		this)

	(defmethod :tip (cell)
		; (. swatches :tip cell) -> :nil | str
		(swatch-tip (elem-get cell +cell_what) (elem-get cell +cell_val) :t))

	(defmethod :cell_items (cell state)
		; (. swatches :cell_items cell state) -> shapes
		(bind '(what val x0 y0 x1 y1) cell)
		(defq items (list))
		(case what
			(:ink (push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0)) :fill val :stroke 0)))
			(:alpha
				;black that see through over a chequer, and how much in words
				(push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0)) :fill 0xffffffff :stroke 0))
				(each (lambda (i)
						(defq cx (+ x0 1.0 (* 4.5 (n2f (% i 8)))) cy (+ y0 1.0 (* 4.5 (n2f (/ i 8)))))
						(if (= (% (+ (% i 8) (/ i 8)) 2) 1)
							(push items (cwb-shape (cwb-d-rect cx cy (min (+ cx 4.5) (- x1 1.0)) (min (+ cy 4.5) (- y1 1.0)))
								:fill 0xffb0b0b0 :stroke 0))))
					(range 0 32))
				(push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0))
					:fill (<< val 24) :stroke 0))
				(defq ink (if (> val 0x60) 0xffffffff 0xff000000)
					text (cwb-text (swatch-tip what val) 0.0 0.0 :fill ink :font_size 11.0))
				(when (defq box (cwb-bounds (list text)))
					(bind '(bx by bx1 by1) box)
					(push items (cwb-text (swatch-tip what val)
						(- (* 0.5 (+ x0 x1)) (* 0.5 (+ bx bx1))) (- (* 0.5 (+ y0 y1)) (* 0.5 (+ by by1)))
						:fill ink :font_size 11.0))))
			;none, white struck through
			(:none (push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0)) :fill 0xffffffff :stroke 0)
				(cwb-shape (cwb-d "M" (+ x0 3.0) (- y1 3.0) "L" (- x1 3.0) (+ y0 3.0)) :stroke 0xffe03131 :width 2.0))))
		items)
	)

(defun swatches-on (board)
	; (swatches-on board) -> (swatches ...)
	;those open on the board
	(filter (const Swatches?) (. (. board :get_stage) :get_actors)))
