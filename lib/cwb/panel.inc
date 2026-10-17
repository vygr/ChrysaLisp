;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/panel.inc - a menu on the board, a panel of cells under a title bar
;
; What every menu of the board is: a panel that lies on the glass, the right
; way up whichever way the board is turned, at the size it is on the screen.
; A bar across the top to drag it by, with the cross that shuts it at its
; right end, as a window's is, and under it rows of square cells. A pointer
; that comes up on the cell it went down on taps it; one that went down on
; the bar, or between the cells, and is dragged moves the panel.
;
; A kind of menu says what its cells are, (:layout), how each is drawn,
; (:cell_items), what a tap on one does, (:tap), and what one is called,
; (:tip). It is an actor on the board's stage, lib/cwb/pointer.inc, so any
; pointer may use it, and a script as a hand does.
;
; Its sizes are of the screen, and are divided by the board's :zoom.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./tools.inc")

;the room round the cells, the bar, and the colours of every menu
(defq +panel_pad 6.0 +panel_bar 16.0
	+pal_back 0xf0283038 +pal_hot 0xff3d6fd0 +pal_ink 0xffffffff +pal_mark 0xff58c0ff
	+panel_bar_back 0xf0485058)

;a cell, a list
(enums +cell 0
	(enum what val x0 y0 x1 y1))

(defun pal-line (d &rest more)
	; (pal-line d [key val] ...) -> shape
	;a line of an icon, round at its ends and its corners unless it is said
	(apply (const cwb-shape) (cat (list d :stroke +pal_ink :width 2.0 :join :round
		:cap1 :round :cap2 :round) more)))

(defun cell-rows (rows cols cell)
	; (cell-rows rows cols cell) -> (cells w h)
	;cells of a size, each row of (what val) starting a new line and going
	;on to the next after cols of them, from 0 0
	(defq cells (list) y 0.0)
	(each (lambda (row)
			(each (lambda ((what val))
					(defq x (* cell (n2f (% (!) cols))) yy (+ y (* cell (n2f (/ (!) cols)))))
					(push cells (list what val x yy (+ x cell) (+ yy cell))))
				row)
			(setq y (+ y (* cell (n2f (/ (+ (length row) cols -1) cols))))))
		rows)
	(list cells (* cell (n2f cols)) y))

(defclass Panel (board x y) (Actor)
	; (Panel board x y) -> panel
	;about a point of the document
	(def this :board (weak-ref board) :origin (list (n2f x) (n2f y)) :hover :nil :cells :nil :shut :nil)

	(defmethod :layout ()
		; (. panel :layout) -> (cells w h)
		;the cells from 0 0, and how wide and high they are together
		(list (list) 0.0 0.0))

	(defmethod :get_cells ()
		; (. panel :get_cells) -> cells
		;made once, about the middle, in the space of the screen, the cross
		;at the right end of the bar the last
		(unless (get :cells this)
			(bind '(inner iw ih) (. this :layout))
			(defq w (+ iw (* 2.0 +panel_pad)) h (+ ih +panel_bar (* 3.0 +panel_pad))
				x0 (neg (* 0.5 w)) y0 (neg (* 0.5 h))
				dx (+ x0 +panel_pad) dy (+ y0 +panel_bar (* 2.0 +panel_pad))
				cells (map (lambda ((what val cx0 cy0 cx1 cy1))
					(list what val (+ cx0 dx) (+ cy0 dy) (+ cx1 dx) (+ cy1 dy))) inner))
			(push cells (list :close :nil (- (* 0.5 w) 2.0 +panel_bar) (+ y0 2.0) (- (* 0.5 w) 2.0) (+ y0 2.0 +panel_bar)))
			(def this :cells cells :size (list w h))
			(. this :zones_changed))
		(get :cells this))

	(defmethod :zones ()
		; (. panel :zones) -> ((name polygons) ...)
		;its cross, the bar it is dragged by, and all of it
		(bind '(& & cx0 cy0 cx1 cy1) (last (. this :get_cells)))
		(bind '(w h) (get :size this))
		(defq x0 (* -0.5 w) y0 (* -0.5 h) x1 (* 0.5 w) y1 (* 0.5 h) by1 (+ y0 +panel_bar 2.0))
		(list
			(list :close (list (list cx0 cy0 cx1 cy0 cx1 cy1 cx0 cy1)))
			(list :bar (list (list x0 y0 x1 y0 x1 by1 x0 by1)))
			(list :frame (list (list x0 y0 x1 y0 x1 y1 x0 y1)))))

	(defmethod :to_own (x y)
		; (. panel :to_own x y) -> (x y)
		;a point of the document, about the middle, at the size it is on the
		;screen, the right way up whichever way the board is turned
		(bind '(ox oy) (get :origin this))
		(defq zoom (get :zoom (obj-ref (get :board this))) turn (get :view_turn (obj-ref (get :board this))))
		(Mat3x2-point (and turn (/= turn 0.0) (Mat3x2-rot turn)) (* (- (n2f x) ox) zoom) (* (- (n2f y) oy) zoom)))

	(defmethod :cell_at (px py)
		; (. panel :cell_at px py) -> :nil | :bar | cell
		;what a point of its own is on: a cell, the cross among them, or
		;the bar
		(cond
			((some (lambda (cell)
					(bind '(& & x0 y0 x1 y1) cell)
					(and (>= px x0) (< px x1) (>= py y0) (< py y1) cell))
				(. this :get_cells)))
			((eql (. this :zone_at px py) :bar) :bar)))

	(defmethod :where (what &optional val)
		; (. panel :where what [val]) -> :nil | (x y)
		;the point of the document that the middle of a cell is at, for what
		;would tap it, (:where :close) for the cross; with no value the first
		;of that kind
		(when (defq cell (some (# (and (eql (elem-get %0 +cell_what) what)
					(or (nil? val) (eql (elem-get %0 +cell_val) val)) %0))
				(. this :get_cells)))
			(bind '(ox oy) (get :origin this))
			(bind '(& & x0 y0 x1 y1) cell)
			(defq zoom (get :zoom (obj-ref (get :board this))) turn (get :view_turn (obj-ref (get :board this))))
			(bind '(px py) (Mat3x2-point (and turn (/= turn 0.0) (Mat3x2-rot (neg turn)))
				(* 0.5 (+ x0 x1)) (* 0.5 (+ y0 y1))))
			(list (+ ox (/ px zoom)) (+ oy (/ py zoom)))))

	(defmethod :close ()
		; (. panel :close) -> panel
		(unless (get :shut this)
			(def this :shut :t :hover :nil)
			(. (. (obj-ref (get :board this)) :get_stage) :sub this)
			(. (obj-ref (get :board this)) :touch +board_dirty_overlay))
		this)

	(defmethod :hit (x y &optional event)
		; (. panel :hit x y) -> :t | :nil
		;by its outline, till it is shut
		(unless (get :shut this) (.super this :hit x y event)))

	(defmethod :tap (cell event)
		; (. panel :tap cell event) -> panel
		;a cell, not the cross, was tapped by event, the device as it went down
		this)

	(defmethod :pointers (stage events)
		; (. panel :pointers stage events) -> panel
		;a pointer that comes up on the cell it went down on taps it. One
		;that went down on the bar, or between the cells, and is dragged
		;moves the panel
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
						((list? was) (. this :hot cell))
						(:t (def this :origin (list (+ ox (- x sx)) (+ oy (- y sy))))
							(. (obj-ref (get :board this)) :touch +board_dirty_overlay))))
				(state
					(. this :set_state pid :nil)
					(. this :hot :nil)
					(and (list? cell) (eql cell (first state))
						(if (eql (elem-get cell +cell_what) :close)
							(. this :close)
							(. this :tap cell (ptr-event pid pkind (elem-get state 4) x y)))))
				(:t (. this :hot cell))))
			events)
		this)

	(defmethod :hot (cell)
		; (. panel :hot cell) -> panel
		(unless (eql cell (get :hover this))
			(def this :hover cell)
			(. (obj-ref (get :board this)) :touch +board_dirty_overlay))
		this)

	(defmethod :leave (stage pid)
		(. this :hot :nil))

	(defmethod :tip (cell)
		; (. panel :tip cell) -> :nil | str
		;the name of a cell, shown above the panel while a pointer is on it
		(if (eql (elem-get cell +cell_what) :close) "close"))

	(defmethod :draw_state ()
		; (. panel :draw_state) -> state
		;what the drawing of each cell is to know, worked out once a draw
		:nil)

	(defmethod :cell_items (cell state)
		; (. panel :cell_items cell state) -> shapes
		;what is drawn of a cell, in the panel's own space
		(list))

	(defmethod :draw (canvas &optional m)
		; (. panel :draw canvas [m]) -> panel
		(defq board (obj-ref (get :board this)) hover (get :hover this)
			turn (get :view_turn board) cells (. this :get_cells) state (. this :draw_state))
		(bind '(ox oy) (get :origin this))
		(bind '(w h) (get :size this))
		(defq base (Mat3x2-mul m (Mat3x2-mul (Mat3x2-translate ox oy)
				(Mat3x2-mul (and turn (/= turn 0.0) (Mat3x2-rot (neg turn))) (Mat3x2-scale (/ 1.0 (get :zoom board))))))
			items (list (cwb-shape (cwb-d-rect (* -0.5 w) (* -0.5 h) (* 0.5 w) (* 0.5 h) 6.0)
				:fill +pal_back :stroke +pal_ink :width 1.0)))
		;the bar to drag it by, lighter, with a grip of three lines
		(defq bx (* -0.5 w) by (* -0.5 h) cy (+ by 2.0 (* 0.5 +panel_bar)))
		(push items (cwb-shape (cwb-d-rect (+ bx 2.0) (+ by 2.0) (- (* 0.5 w) 2.0) (+ by +panel_bar 2.0) 4.0)
				:fill (if (eql hover :bar) +pal_hot +panel_bar_back) :stroke 0)
			(pal-line (cwb-d "M" -14.0 (- cy 3.0) "L" 14.0 (- cy 3.0) "M" -14.0 cy "L" 14.0 cy "M" -14.0 (+ cy 3.0) "L" 14.0 (+ cy 3.0)) :width 1.2))
		(each (lambda (cell)
				(bind '(what & x0 y0 x1 y1) cell)
				(cond
					((eql what :close)
						(defq cx (* 0.5 (+ x0 x1)) cy (* 0.5 (+ y0 y1)) r 4.0)
						(if (eql cell hover)
							(push items (cwb-shape (cwb-d-rect x0 y0 x1 y1 3.0) :fill +pal_hot :stroke 0)))
						(push items (pal-line (cwb-d "M" (- cx r) (- cy r) "L" (+ cx r) (+ cy r) "M" (+ cx r) (- cy r) "L" (- cx r) (+ cy r)) :width 1.6)))
					(:t (each (# (push items %0)) (. this :cell_items cell state))
						(if (eql cell hover)
							(push items (cwb-shape (cwb-d-rect x0 y0 x1 y1) :fill 0 :stroke +pal_hot :width 2.5))))))
			cells)
		;the name of the cell a pointer is on, above the panel
		(when (and (list? hover) (defq tip (. this :tip hover)))
			(bind '(& & x0 y0 x1 &ignore) hover)
			(defq text (cwb-text tip 0.0 0.0 :fill +pal_ink :font_size 16.0))
			(when (defq box (cwb-bounds (list text)))
				(bind '(bx by bx1 by1) box)
				(defq tx (- (* 0.5 (+ x0 x1)) (* 0.5 (+ bx bx1))) ty (- (* -0.5 h) 8.0 by1))
				(push items (cwb-shape (cwb-d-rect (+ bx tx -4.0) (+ by ty -3.0) (+ bx1 tx 4.0) (+ by1 ty 3.0) 3.0)
						:fill +pal_back :stroke +pal_ink :width 1.0)
					(cwb-text tip tx ty :fill +pal_ink :font_size 16.0))))
		(cwb-draw-items canvas items base)
		this)
	)
