;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/swatches.inc - the colours of a custom ink, a menu on the board
;
; Chris: "an auxiliary pallet selector menu ? that just give finer control
; of colours", "it's a menu in the whiteboard, just like another ring menu".
; Twelve hues in five shades and a row of greys, and for a brush how see
; through it is, for the pen or the brush, which it has a switch for. It
; is an actor on the board's stage, as a floating menu
; is: a tap of any device on a colour is that device's, "Just another tap
; with device to change colour/fill", and it stays till its cross is
; tapped. A drag on its edge moves it.
;
; What a tap does is the app's: it is given a function, called with the
; menu, the event of the tap, as the device was when it went down, what
; was tapped, :color or :alpha, and its value. The switch is its own. As many as are wanted can
; be open, as floating menus, Chris: "each pupile can have one, there is
; no limit".
;
; Its sizes are of the screen, and are divided by the board's :zoom.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./palette.inc")

;a cell, the room round them and between them, and how see through a
;brush can be
(defq +swatch_cell 20.0 +swatch_pad 6.0 +swatch_cols 12
	+swatch_alphas ''(0xff 0xc0 0x80 0x40))

;a cell, a list
(enums +swcell 0
	(enum what val x0 y0 x1 y1))

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

(defclass Swatches (board x y key colors on_pick) (Actor)
	; (Swatches board x y key colors on_pick) -> swatches
	;about a point of the document, for the custom ink of key, :color for
	;the pen or :fill for the brush, colors the two of them now. on_pick
	;is called (on_pick swatches event what val), with :key the one it is
	;for
	(def this :board board :origin (list (n2f x) (n2f y)) :key key :colors colors
		:on_pick on_pick :hover :nil :cells :nil)

	(defmethod :get_cells ()
		; (. swatches :get_cells) -> cells
		;made once, about the middle, in the space of the screen
		(unless (get :cells this)
			(defq cells (list) colors (swatch-colors) n (/ (length colors) +swatch_cols)
				w (+ (* +swatch_cell (n2f +swatch_cols)) (* 2.0 +swatch_pad))
				h (+ (* +swatch_cell (n2f (inc n))) (* 3.0 +swatch_pad))
				x0 (neg (* 0.5 w)) y0 (neg (* 0.5 h)))
			(each (lambda (col)
					(defq cx (+ x0 +swatch_pad (* +swatch_cell (n2f (% (!) +swatch_cols))))
						cy (+ y0 +swatch_pad (* +swatch_cell (n2f (/ (!) +swatch_cols)))))
					(push cells (list :color col cx cy (+ cx +swatch_cell) (+ cy +swatch_cell))))
				colors)
			;the last row: the pen and the brush, how see through a brush is,
			;and the cross
			(defq cy (+ y0 (* 2.0 +swatch_pad) (* +swatch_cell (n2f n))) cy1 (+ cy +swatch_cell))
			(each (lambda (key)
					(defq cx (+ x0 +swatch_pad (* +swatch_cell (n2f (!)))))
					(push cells (list :key key cx cy (+ cx +swatch_cell) cy1)))
				'(:color :fill))
			(if (eql (get :key this) :fill)
				(each (lambda (alpha)
						(defq cx (+ x0 +swatch_pad (* +swatch_cell (+ 2.5 (* 2.0 (n2f (!)))))))
						(push cells (list :alpha alpha cx cy (+ cx (* 2.0 +swatch_cell)) cy1)))
					+swatch_alphas))
			(defq cx (- (* 0.5 w) +swatch_pad +swatch_cell))
			(push cells (list :close :nil cx cy (+ cx +swatch_cell) cy1))
			(def this :cells cells :size (list w h)))
		(get :cells this))

	(defmethod :to_own (x y)
		; (. swatches :to_own x y) -> (x y)
		;a point of the document, about the middle, at the size it is on the
		;screen, the right way up whichever way the board is turned
		(bind '(ox oy) (get :origin this))
		(defq zoom (get :zoom (get :board this)) turn (get :view_turn (get :board this)))
		(Mat3x2-point (and turn (/= turn 0.0) (Mat3x2-rot turn)) (* (- (n2f x) ox) zoom) (* (- (n2f y) oy) zoom)))

	(defmethod :cell_at (px py)
		; (. swatches :cell_at px py) -> :nil | cell
		(some (lambda (cell)
				(bind '(& & x0 y0 x1 y1) cell)
				(and (>= px x0) (< px x1) (>= py y0) (< py y1) cell))
			(. this :get_cells)))

	(defmethod :where (what &optional val)
		; (. swatches :where what [val]) -> :nil | (x y)
		;the point of the document that the middle of a cell is at, for what
		;would tap it: (:where :color 0xff000000), (:where :alpha 0x80),
		;(:where :close)
		(when (defq cell (some (# (and (eql (elem-get %0 +swcell_what) what)
					(or (eql what :close) (eql (elem-get %0 +swcell_val) val)) %0))
				(. this :get_cells)))
			(bind '(ox oy) (get :origin this))
			(bind '(& & x0 y0 x1 y1) cell)
			(defq zoom (get :zoom (get :board this)) turn (get :view_turn (get :board this)))
			(bind '(px py) (Mat3x2-point (and turn (/= turn 0.0) (Mat3x2-rot (neg turn)))
				(* 0.5 (+ x0 x1)) (* 0.5 (+ y0 y1))))
			(list (+ ox (/ px zoom)) (+ oy (/ py zoom)))))

	(defmethod :close ()
		; (. swatches :close) -> swatches
		(. (. (get :board this) :get_stage) :sub this)
		(. (get :board this) :touch +board_dirty_overlay)
		this)

	(defmethod :hit (x y)
		; (. swatches :hit x y) -> :t | :nil
		(. this :get_cells)
		(bind '(px py) (. this :to_own x y))
		(bind '(w h) (get :size this))
		(and (<= (abs px) (* 0.5 w)) (<= (abs py) (* 0.5 h))))

	(defmethod :pointers (stage events)
		; (. swatches :pointers stage events) -> swatches
		;a pointer that comes up on the cell it went down on taps it. One
		;that went down between the cells and is dragged moves the menu
		(each (lambda (event)
			(bind '(pid pkind buttons x y &ignore) event)
			(bind '(px py) (. this :to_own x y))
			(defq cell (. this :cell_at px py) state (. this :state pid))
			(cond
				((and (/= buttons 0) (not state))
					(. this :set_state pid (list cell x y (get :origin this) buttons))
					(. this :hot cell))
				((/= buttons 0)
					(bind '(was sx sy (ox oy) &ignore) state)
					(cond
						(was (. this :hot cell))
						(:t (def this :origin (list (+ ox (- x sx)) (+ oy (- y sy))))
							(. (get :board this) :touch +board_dirty_overlay))))
				(state
					(. this :set_state pid :nil)
					(. this :hot :nil)
					(and cell (eql cell (first state))
						(case (elem-get cell +swcell_what)
							(:close (. this :close))
							(:key (def this :key (elem-get cell +swcell_val) :cells :nil :hover :nil)
								(. (get :board this) :touch +board_dirty_overlay))
							(:t ((get :on_pick this) this (ptr-event pid pkind (elem-get state 4) x y)
								(elem-get cell +swcell_what) (elem-get cell +swcell_val))))))
				(:t (. this :hot cell))))
			events)
		this)

	(defmethod :hot (cell)
		; (. swatches :hot cell) -> swatches
		(unless (eql cell (get :hover this))
			(def this :hover cell)
			(. (get :board this) :touch +board_dirty_overlay))
		this)

	(defmethod :leave (stage pid)
		(. this :hot :nil))

	(defmethod :set_color (key color)
		; (. swatches :set_color key color) -> swatches
		;the custom pen, key :color, or brush, :fill, is this now, it is marked
		(elem-set (get :colors this) (if (eql key :color) 0 1) color)
		(. (get :board this) :touch +board_dirty_overlay)
		this)

	(defmethod :draw (canvas &optional m)
		; (. swatches :draw canvas [m]) -> swatches
		(defq board (get :board this) hover (get :hover this) key (get :key this)
			color (elem-get (get :colors this) (if (eql key :color) 0 1))
			turn (get :view_turn board) cells (. this :get_cells))
		(bind '(ox oy) (get :origin this))
		(bind '(w h) (get :size this))
		(defq base (Mat3x2-mul m (Mat3x2-mul (Mat3x2-translate ox oy)
				(Mat3x2-mul (and turn (/= turn 0.0) (Mat3x2-rot (neg turn))) (Mat3x2-scale (/ 1.0 (get :zoom board))))))
			items (list (cwb-shape (cwb-d-rect (* -0.5 w) (* -0.5 h) (* 0.5 w) (* 0.5 h) 6.0)
				:fill +pal_back :stroke +pal_ink :width 1.0))
			rgb (logand color 0xffffff) alpha (>> (logand color 0xff000000) 24))
		(each (lambda (cell)
				(bind '(what val x0 y0 x1 y1) cell)
				(defq on (case what (:color (= (logand val 0xffffff) rgb)) (:alpha (= val alpha)) (:key (eql val key))))
				(case what
					(:color (push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0)) :fill val :stroke 0)))
					(:alpha
						;the colour at how see through it would be, on a light
						;and a dark half
						(push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (* 0.5 (+ x0 x1)) (- y1 1.0)) :fill 0xffffffff :stroke 0)
							(cwb-shape (cwb-d-rect (* 0.5 (+ x0 x1)) (+ y0 1.0) (- x1 1.0) (- y1 1.0)) :fill 0xff000000 :stroke 0)
							(cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0))
								:fill (logior rgb (<< val 24)) :stroke 0)))
					;the pen a line, the brush a spot, each of its colour now
					(:key (defq cx (* 0.5 (+ x0 x1)) cy (* 0.5 (+ y0 y1))
							c (elem-get (get :colors this) (if (eql val :color) 0 1)))
						(push items (if (eql val :color)
							(pal-line (cwb-d "M" (- cx 5.0) (+ cy 5.0) "L" (+ cx 5.0) (- cy 5.0)) :stroke (logior c 0xff000000) :width 3.0)
							(cwb-shape (cwb-d-ellipse cx cy 6.0) :fill c :stroke +pal_ink :width 1.0))))
					(:close (defq cx (* 0.5 (+ x0 x1)) cy (* 0.5 (+ y0 y1)) r 5.0)
						(push items (pal-line (cwb-d "M" (- cx r) (- cy r) "L" (+ cx r) (+ cy r) "M" (+ cx r) (- cy r) "L" (- cx r) (+ cy r))))))
				(cond
					((eql cell hover)
						(push items (cwb-shape (cwb-d-rect x0 y0 x1 y1) :fill 0 :stroke +pal_hot :width 2.5)))
					(on (push items (cwb-shape (cwb-d-rect x0 y0 x1 y1) :fill 0 :stroke +pal_mark :width 2.5)))))
			cells)
		(cwb-draw-items canvas items base)
		this)
	)

(defun swatches-on (board)
	; (swatches-on board) -> (swatches ...)
	;those open on the board
	(filter (const Swatches?) (. (. board :get_stage) :get_actors)))
