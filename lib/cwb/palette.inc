;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/palette.inc - the menu of the board: tools, pens, widths, things to do
;
; The board is kept clear. A hand that taps where there is nothing, with
; nothing selected, opens one there, and a button of a toolbar floats one.
; A panel, lib/cwb/panel.inc, in the same style as every menu of the board:
; a bar to drag it by with its cross, and rows of cells, the tools, the
; pens, the widths, and the things to do.
;
; One opened by a tap belongs to the pointer that opened it, and shuts on a
; pick of a tool, or a tap of that pointer anywhere off it. A floating one
; stays till its cross is tapped, and a tap on it is for the device and
; button that tapped, which it shows, a square of each device on what it
; holds.
;
; Its sizes are of the screen, and are divided by the board's :zoom.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./panel.inc")

;a cell, how many across, the squares of the devices on a floating one
(defq +pal_cell 26.0 +pal_cols 8 +pal_dot 4.0
	+pal_tools ''(:select :pen :line :arrow :arrow2 :rect :ellipse :frect :fellipse :text :eraser)
	+pal_colors ''(0xff000000 0xff7f7f7f 0xffffffff 0xffe03131 0xfff08c00 0xffffd43b
		0xff2f9e44 0xff15aabf 0xff1971c2 0xff7048e8 0xffd6336c 0xff8d5524)
	+pal_widths ''(1.5 3.0 6.0 12.0)
	;the instruments together, the rest together
	+pal_actions ''(:undo :redo :snap :duplicate :delete :ruler :protractor :full_protractor :set_square :set_square45))

(defun pal-tip (what val)
	; (pal-tip what val) -> :nil | str
	;the name of a cell, as the toolbars' tips say it. A colour or a pen has
	;none, as an ink of the bar has none
	(case what
		(:tool (elem-get '("select" "pen" "line" "arrow" "double arrow" "rect" "ellipse"
				"filled rect" "filled ellipse" "text" "eraser") (ifn (find val +pal_tools) 0)))
		(:width (cond ((<= val 1.5) "fine") ((<= val 3.0) "thin") ((<= val 6.0) "medium") ("thick")))
		(:action (case val
			(:undo "undo") (:redo "redo") (:snap "snap grid") (:duplicate "duplicate") (:delete "delete")
			(:ruler "ruler") (:protractor "protractor") (:full_protractor "full protractor")
			(:set_square "set square 30") (:set_square45 "set square 45") (:swatches "palette") (:window "window")))))

(defun pal-icon (what val)
	; (pal-icon what val) -> shapes
	;what is drawn on a cell, about 0 0, in a box of 20 or so
	(case what
		(:tool (case val
			(:select (list (cwb-shape "M -5 -9 L -5 6 L -1.5 2.5 L 1.5 9 L 4 8 L 1 1.5 L 6 1.5 Z" :fill +pal_ink :stroke 0)))
			(:pen (list (pal-line "M -8 4 Q -4 -10 0 0 Q 4 10 8 -4")))
			(:line (list (pal-line "M -7 6 L 7 -6")))
			(:arrow (list (pal-line "M -7 6 L 7 -6 M 1 -5.5 L 7 -6 L 6.5 0")))
			(:arrow2 (list (pal-line "M -7 6 L 7 -6 M 1 -5.5 L 7 -6 L 6.5 0 M -1 5.5 L -7 6 L -6.5 0")))
			(:rect (list (pal-line (cwb-d-rect -8 -5.5 8 5.5) :join :miter)))
			(:ellipse (list (pal-line (cwb-d-ellipse 0 0 8.5 6))))
			(:frect (list (cwb-shape (cwb-d-rect -8 -5.5 8 5.5) :fill +pal_ink :stroke 0)))
			(:fellipse (list (cwb-shape (cwb-d-ellipse 0 0 8.5 6) :fill +pal_ink :stroke 0)))
			(:text (list (pal-line "M -6 -7 L 6 -7 M 0 -7 L 0 8" :width 2.6)))
			(:eraser (list (pal-line "M -8 2 L -2 -7 L 8 -1 L 2 8 Z") (pal-line "M -4.7 -2.9 L 5.3 3.1")))
			(:t (list))))
		(:width (list (cwb-shape (cwb-d-ellipse 0 0 (max 1.5 (min 9.0 (* 0.5 val)))) :fill +pal_ink :stroke 0)))
		(:action (case val
			(:undo (list (pal-line "M -7 -2 L 2 -2 A 4.5 4.5 0 0 1 2 7 L -3 7 M -3 -6 L -7 -2 L -3 2")))
			(:redo (list (pal-line "M 7 -2 L -2 -2 A 4.5 4.5 0 0 0 -2 7 L 3 7 M 3 -6 L 7 -2 L 3 2")))
			(:ruler (list (pal-line (cwb-d-rect -9 -3.5 9 3.5) :width 1.6 :join :miter)
				(pal-line "M -5 -3.5 L -5 0 M -1 -3.5 L -1 0 M 3 -3.5 L 3 0 M 7 -3.5 L 7 0" :width 1.4)))
			;the instruments as they are: a protractor its marks round its
			;edge, the whole one its line across, and a set square a right
			;angle with the hole in it a set square has
			(:protractor (list (pal-line "M -9 4 A 9 9 0 0 1 9 4 Z" :width 1.6)
				(pal-line "M 0 -5 L 0 -2.5 M -6.4 -1.6 L -4.6 -0.4 M 6.4 -1.6 L 4.6 -0.4 M 0 4 L 0 1.5" :width 1.2)))
			(:full_protractor (list (pal-line (cwb-d-ellipse 0 0 8.5) :width 1.6)
				(pal-line "M -8.5 0 L 8.5 0 M 0 -8.5 L 0 -6 M 0 8.5 L 0 6 M 6 -6 L 4.5 -4.5 M -6 -6 L -4.5 -4.5 M 6 6 L 4.5 4.5 M -6 6 L -4.5 4.5" :width 1.2)))
			(:set_square (list (pal-line "M -6 8 L 6 8 L -6 -9 Z" :width 1.6)
				(pal-line "M -3.6 5.6 L 0.8 5.6 L -3.6 -1.6 Z" :width 1.2)))
			(:set_square45 (list (pal-line "M -8 7 L 8 7 L -8 -9 Z" :width 1.6)
				(pal-line "M -5.4 4.6 L 0.6 4.6 L -5.4 -1.4 Z" :width 1.2)))
			(:window (list (pal-line (cwb-d-rect -8 -6 8 6) :width 1.6 :join :miter)
				(pal-line "M -8 -2 L 8 -2" :width 1.4)))
			(:snap (list (pal-line "M -7 -2.5 L 7 -2.5 M -7 2.5 L 7 2.5 M -2.5 -7 L -2.5 7 M 2.5 -7 L 2.5 7" :width 1.4)))
			(:duplicate (list (pal-line (cwb-d-rect -7 -7 3 3) :width 1.6 :join :miter)
				(cwb-shape (cwb-d-rect -3 -3 7 7) :fill +pal_ink :stroke 0)))
			(:delete (list (pal-line "M -6 -6 L 6 6 M 6 -6 L -6 6" :width 2.6)))
			;a painter's palette, as the toolbar's symbol is
			(:swatches (list (pal-line (cwb-d-ellipse 0 0 8.5) :width 1.6) (pal-line (cwb-d-ellipse 3 3.5 1.8) :width 1.2)
				(cwb-shape (cwb-d-ellipse -3.8 -2.5 1.6) :fill +pal_ink :stroke 0)
				(cwb-shape (cwb-d-ellipse 0 -5.2 1.6) :fill +pal_ink :stroke 0)
				(cwb-shape (cwb-d-ellipse 3.8 -2.5 1.6) :fill +pal_ink :stroke 0)))
			(:t (list))))
		(:t (list))))

;a cell of a pen or a colour, what is drawn in it
(defun pal-ink-cell (x0 y0 x1 y1 ink)
	; (pal-ink-cell x0 y0 x1 y1 ink) -> shapes
	(if (board-none? ink)
		(list (cwb-shape (cwb-d-rect (+ x0 2.0) (+ y0 2.0) (- x1 2.0) (- y1 2.0)) :fill 0xffffffff :stroke 0)
			(cwb-shape (cwb-d "M" (+ x0 4.0) (- y1 4.0) "L" (- x1 4.0) (+ y0 4.0)) :stroke 0xffe03131 :width 2.0))
		(list (cwb-shape (cwb-d-rect (+ x0 2.0) (+ y0 2.0) (- x1 2.0) (- y1 2.0)) :fill ink :stroke 0))))

(defclass Palette (board x y &optional kind id) (Panel board x y)
	; (Palette board x y [kind id]) -> palette
	;about a point of the document, for the pointer of that kind and id,
	;default the mouse. It is not on the stage till it is put there,
	;(palette-open) does both
	(def this :kind (ifn kind :mouse) :id (ifn id 0)
		;a floating menu, (palette-float): it stays till its cross is
		;tapped, and binds whatever taps it
		:floating :nil)

	(defmethod :get_cells ()
		; (. palette :get_cells) -> cells
		;made again if what the board offers, swatches or the window, has
		;changed since they were made
		(defq board (get :board this)
			offers (+ (if (get :on_swatches board) 1 0) (if (get :on_window board) 2 0)))
		(unless (eql offers (get :offers this)) (def this :cells :nil :offers offers))
		(.super this :get_cells))

	(defmethod :layout ()
		; (. palette :layout) -> (cells w h)
		;the tools, the pens of a board that has them else its colours, the
		;widths, and the things to do: the swatches if what shows the board
		;has them, :on_swatches, and the window if the board is shown with
		;nothing round it, :on_window
		(defq board (get :board this))
		(cell-rows (list
				(map (# (list :tool %0)) +pal_tools)
				(if (defq pens (get :pens board))
					(map (# (list :pen %0)) (range 0 (length pens)))
					(map (# (list :color %0)) (ifn (get :menu_colors board) +pal_colors)))
				(map (# (list :width %0)) (ifn (get :menu_widths board) +pal_widths))
				(map (# (list :action %0)) (cat +pal_actions
					(if (get :on_swatches board) '(:swatches) '())
					(if (get :on_window board) '(:window) '()))))
			+pal_cols +pal_cell))

	(defmethod :value (key)
		; (. palette :value key) -> val
		;:mode, :color or :width, as the pointer it belongs to has it, a
		;pen that is bound to its own, or the board's
		(defq board (get :board this))
		(. (. board :get_bindings) :value
			(ptr-event (get :id this) (get :kind this) +pev_left 0 0) key (get key board)))

	(defmethod :pick (what &optional val event)
		; (. palette :pick what [val event]) -> palette
		;do what a tap on a cell does, see (:where). What it picks is the
		;board's to do, (. board :pick), for what the menu is for: a floating
		;menu, given the event of the tap, what tapped it, and it stays; one
		;opened for a pen, that pen; one opened by the mouse or a finger, the
		;board's own. One not floating shuts on a tool
		(defq board (get :board this) floating (and (get :floating this) event)
			user (if floating (. board :user_of event))
			who (or floating (ptr-event (get :id this) (get :kind this) +pev_left 0 0))
			by (cond (floating event) ((eql (get :kind this) :pen) who)))
		(case what
			(:tool (. board :pick by :mode val) (unless floating (. this :close)))
			((:color :pen :width) (. board :pick by what val))
			(:action
				(bind '(ox oy) (get :origin this))
				;an instrument is made where the palette is, and with the
				;snap on is on the lines of the paper from the first
				(case val
					;only the teacher undoes
					(:undo (if (or (not floating) (= user +board_teacher)) (. board :undo)))
					(:redo (if (or (not floating) (= user +board_teacher)) (. board :redo)))
					(:ruler (. (. board :get_stage) :add (. (Ruler board ox oy) :snap_zero who)) (unless floating (. this :close)))
					(:protractor (. (. board :get_stage) :add (. (Protractor board ox oy) :snap_zero who)) (unless floating (. this :close)))
					(:full_protractor (. (. board :get_stage) :add (. (Circle board ox oy) :snap_zero who)) (unless floating (. this :close)))
					(:set_square (. (. board :get_stage) :add (. (Setsquare board ox oy) :snap_zero who)) (unless floating (. this :close)))
					(:set_square45 (. (. board :get_stage) :add (. (Setsquare45 board ox oy) :snap_zero who)) (unless floating (. this :close)))
					(:window (if (defq on_window (get :on_window board)) (on_window)) (unless floating (. this :close)))
					;the swatches, a menu of their own beside this one, if what
					;shows the board has them, :on_swatches
					(:swatches (if (defq on_swatches (get :on_swatches board))
							(on_swatches (+ ox (/ (+ (* 0.5 (first (get :size this))) 140.0) (get :zoom board))) oy))
						(unless floating (. this :close)))
					;snap is each device's own, as its colour is
					(:snap (. board :pick by :switch :snap))
					;what the user of the tap holds, or by no device all of it
					(:duplicate (. board :duplicate :nil :nil (if by (. board :selected_by (. board :user_of by))))
						(unless floating (. this :close)))
					(:delete (. board :delete (if by (. board :selected_by (. board :user_of by))))
						(unless floating (. this :close))))))
		(. board :touch +board_dirty_overlay)
		this)

	(defmethod :dismiss (event)
		; (. palette :dismiss event) -> :t | :nil
		;a pointer has gone down on the board, off this. It shuts if the
		;pointer is the one it belongs to: of its kind, and for a pen the
		;same pen. Did it
		(when (and (not (get :shut this)) (not (get :floating this)) (eql (elem-get event +pev_kind) (get :kind this))
				(or (not (eql (get :kind this) :pen)) (= (ptr-device (elem-get event +pev_id)) (ptr-device (get :id this)))))
			(. this :close)
			:t))

	;;;;;;;;;;
	; pointers
	;;;;;;;;;;

	(defmethod :tap (cell event)
		; (. palette :tap cell event) -> palette
		(. this :pick (elem-get cell +cell_what) (elem-get cell +cell_val) event))

	(defmethod :tip (cell)
		; (. palette :tip cell) -> :nil | str
		(if (eql (elem-get cell +cell_what) :close) "close"
			(pal-tip (elem-get cell +cell_what) (elem-get cell +cell_val))))

	(defmethod :devices ()
		; (. palette :devices) -> ((color what slot) ...)
		;the dots of the devices, (. bindings :devices), each in its place
		(defq board (get :board this))
		(. (. board :get_bindings) :devices (# (. board :device_slot %0 %1 %2))))

	(defmethod :draw_state ()
		; (. palette :draw_state) -> (mode color width pen snap pens devices)
		;what is current for its pointer, the inks of the pens as they are
		;now, and for a floating one the squares of the devices
		(defq board (get :board this) who (ptr-event (get :id this) (get :kind this) +pev_left 0 0))
		(list (. this :value :mode) (. this :value :color) (. this :value :width)
			(. board :pen_of who) (/= (first (. board :pointer_settings who)) 0.0)
			(get :pens board) (if (get :floating this) (. this :devices))))

	(defmethod :cell_items (cell state)
		; (. palette :cell_items cell state) -> shapes
		;its back, or its ink, its icon, and what is current for its pointer
		;marked, or on a floating one a square of each device that holds it
		(bind '(what val x0 y0 x1 y1) cell)
		(bind '(mode color width pen snap pens devices) state)
		(defq board (get :board this) items (list) cx (* 0.5 (+ x0 x1)) cy (* 0.5 (+ y0 y1)))
		(case what
			(:pen (setq items (pal-ink-cell x0 y0 x1 y1 (elem-get pens val))))
			(:color (setq items (pal-ink-cell x0 y0 x1 y1 val)))
			(:t (push items (cwb-shape (cwb-d-rect (+ x0 1.0) (+ y0 1.0) (- x1 1.0) (- y1 1.0) 3.0) :fill 0xf0384048 :stroke 0))
				(if (nempty? (defq icon (pal-icon what val)))
					(push items (cwb-group icon :m (Mat3x2-translate cx cy))))))
		(cond
			(devices
				(defq key (case what (:tool :mode) (:color :color) (:pen :pen) (:width :width) (:action (if (eql val :snap) :snap))))
				(each (lambda ((col bound slot))
						(when (and key (if (eql key :snap) (. board :switch_of bound :snap)
								(and (defq at (ptr-at bound key)) (eql (elem-get bound at) val))))
							;a device's column, its second button's row above the first's
							(defq dx (+ x0 1.5 (* +pal_dot (n2f (% slot 6)))) dy (- y1 1.5 (* +pal_dot (n2f (inc (/ slot 6))))))
							(push items (cwb-shape (cwb-d-rect dx dy (+ dx +pal_dot) (+ dy +pal_dot))
								:fill col :stroke 0xff000000 :width 0.75))))
					devices))
			((case what
					(:tool (eql val mode))
					(:color (= val color))
					(:pen (eql val pen))
					(:width (= val width))
					(:action (and (eql val :snap) snap)))
				(push items (cwb-shape (cwb-d-rect (+ x0 1.5) (+ y0 1.5) (- x1 1.5) (- y1 1.5) 3.0) :fill 0 :stroke +pal_mark :width 2.5))))
		items)
	)

(defun palette-open (board x y &optional kind id)
	; (palette-open board x y [kind id]) -> palette
	;a menu on the board, about a point, for the pointer of that kind and
	;id, default the mouse. One that pointer had open shuts. It is about the
	;point, wherever that is: under the hand that asked for it
	(setd kind :mouse id 0)
	(defq stage (. board :get_stage) x (n2f x) y (n2f y))
	(each (# (and (Palette? %0) (eql (get :kind %0) kind) (= (get :id %0) id) (. %0 :close)))
		(. stage :get_actors))
	(. stage :add (defq palette (Palette board x y kind id)))
	(. board :touch +board_dirty_overlay)
	palette)

(defun palette-float (board x y)
	; (palette-float board x y) -> palette
	;a floating menu about a point, as many as are wanted, from a button of
	;a toolbar. It stays till its cross is tapped, and a tap on it binds the
	;device that tapped, see (. palette :pick) and (. board :pick)
	(defq stage (. board :get_stage))
	(. stage :add (defq palette (Palette board (n2f x) (n2f y))))
	(def palette :floating :t)
	(. board :touch +board_dirty_overlay)
	palette)

(defun palette-enable (board &optional colors widths)
	; (palette-enable board [colors widths]) -> board
	;from now a hand that taps where there is nothing, with nothing
	;selected, opens a palette there, its own. With the colours and the
	;widths given, or its own twelve and four
	(def board :menu_colors colors :menu_widths widths)
	(def board :menu (lambda (board x y event)
		(palette-open board x y (elem-get event +pev_kind) (elem-get event +pev_id))))
	board)

(defun palettes (board)
	; (palettes board) -> palettes
	;those that are open on a board
	(filter (# (and (Palette? %0) (not (get :shut %0)))) (. (. board :get_stage) :get_actors)))
