;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/palette.inc - a palette that opens on the board, where it is wanted
;
; The board is kept clear. A hand that taps where there is nothing, with
; nothing selected, opens a palette there: rings of wedges about the point,
; the tools, the colours and widths, and things to do. It opens out, a ring
; after a ring, and is put away by a pick of a tool, a tap on its middle,
; or a tap anywhere off it.
;
; It is an actor on the board's stage, lib/cwb/pointer.inc, so it takes
; pointers as everything there does, any of them, and is there for a
; script as it is for a hand.
;
; It belongs to the pointer that opened it. What a pen picks is that pen's:
; the pen is bound, by its id, to the tool, the colour and the width, and
; another pen keeps its own. What a mouse or a finger picks is the board's.
;
; It moves by time it is told, (. board :tick time), microseconds. Nothing
; here asks what the time is, so a test steps it.
;
; Its sizes are of the screen, and are divided by the board's :zoom.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./tools.inc")

;the middle, and where each of the three rings starts and ends
(defq +pal_hub 24.0 +pal_radii ''((30.0 78.0) (82.0 112.0) (116.0 150.0)) +pal_gap 1.5
	;how long a ring takes to open, how long after the last the next starts,
	;and how long it takes to shut, microseconds
	+pal_open 220000 +pal_lag 50000 +pal_shut 120000
	+pal_back 0xf0283038 +pal_hot 0xff3d6fd0 +pal_ink 0xffffffff +pal_mark 0xff58c0ff
	+pal_tools ''(:select :pen :line :arrow :arrow2 :rect :ellipse :frect :fellipse :text :eraser)
	+pal_colors ''(0xff000000 0xff7f7f7f 0xffffffff 0xffe03131 0xfff08c00 0xffffd43b
		0xff2f9e44 0xff15aabf 0xff1971c2 0xff7048e8 0xffd6336c 0xff8d5524)
	+pal_widths ''(1.5 3.0 6.0 12.0)
	+pal_actions ''(:undo :redo :ruler :protractor :full_protractor :set_square :snap :duplicate :delete))

;a wedge, a list
(enums +wedge 0
	(enum ring what val a0 a1 r0 r1 back hot mark icon))

(defun pal-point (r a)
	; (pal-point r a) -> (x y)
	;the point that far from the middle, at an angle from straight up
	;round the way a clock goes
	(list (* r (sin a)) (neg (* r (cos a)))))

(defun pal-wedge-d (r0 r1 a0 a1)
	; (pal-wedge-d r0 r1 a0 a1) -> d
	;the path of a wedge of a ring, between two radii and two angles
	(bind '(x0 y0) (pal-point r0 a0)) (bind '(x1 y1) (pal-point r1 a0))
	(bind '(x2 y2) (pal-point r1 a1)) (bind '(x3 y3) (pal-point r0 a1))
	(defq big (if (> (- a1 a0) +fp_pi) 1 0))
	(cwb-d "M" x0 y0 "L" x1 y1 "A" r1 r1 0 big 1 x2 y2 "L" x3 y3 "A" r0 r0 0 big 0 x0 y0 "Z"))

(defun pal-line (d &rest more)
	; (pal-line d [key val] ...) -> shape
	;a line of an icon, round at its ends and its corners unless it is said
	(apply (const cwb-shape) (cat (list d :stroke +pal_ink :width 2.0 :join :round
		:cap1 :round :cap2 :round) more)))

(defun pal-icon (what val)
	; (pal-icon what val) -> shapes
	;what is drawn on a wedge, about 0 0, in a box of 20 or so
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
			(:protractor (list (pal-line "M -8.5 4 A 8.5 8.5 0 0 1 8.5 4 Z" :width 1.6) (pal-line "M 0 4 L 0 1" :width 1.4)))
			(:full_protractor (list (pal-line (cwb-d-ellipse 0 0 8.5) :width 1.6)
				(pal-line "M 0 -8.5 L 0 -5 M 8.5 0 L 5 0 M 0 8.5 L 0 5 M -8.5 0 L -5 0 M -1.5 0 L 1.5 0 M 0 -1.5 L 0 1.5" :width 1.4)))
			(:set_square (list (pal-line "M -7 6 L 7 6 L -7 -7 Z" :width 1.6)))
			(:snap (list (pal-line "M -7 -2.5 L 7 -2.5 M -7 2.5 L 7 2.5 M -2.5 -7 L -2.5 7 M 2.5 -7 L 2.5 7" :width 1.4)))
			(:duplicate (list (pal-line (cwb-d-rect -7 -7 3 3) :width 1.6 :join :miter)
				(cwb-shape (cwb-d-rect -3 -3 7 7) :fill +pal_ink :stroke 0)))
			(:delete (list (pal-line "M -6 -6 L 6 6 M 6 -6 L -6 6" :width 2.6)))
			(:t (list))))
		(:t (list))))

(defun pal-ease (x)
	; (pal-ease x) -> num
	;0 to 1, by a little past 1 and back, as a thing that springs open does
	(defq x (- (max 0.0 (min 1.0 x)) 1.0))
	(+ 1.0 (* 2.70158 x x x) (* 1.70158 x x)))

(defclass Palette (board x y &optional kind id) (Actor)
	; (Palette board x y [kind id]) -> palette
	;about a point of the document, for the pointer of that kind and id,
	;default the mouse. It is not on the stage till it is put there,
	;(palette-open) does both. It starts to open at the time the board
	;was last told
	(def this :board board :origin (list (n2f x) (n2f y)) :kind (ifn kind :mouse) :id (ifn id 0)
		:start (get :time board) :shut :nil :now (get :time board) :hover :nil :wedges :nil :gone :nil)

	(defmethod :build ()
		; (. palette :build) -> palette
		;the wedges, each with what is drawn for it, made once
		(defq wedges (list))
		(each (lambda (entries)
			(defq k (!) n (length entries) step (/ +fp_2pi (n2f n)))
			(bind '(r0 r1) (elem-get +pal_radii k))
			(defq gap (/ +pal_gap (* 0.5 (+ r0 r1))))
			(each (lambda ((what val))
				(defq a0 (* step (n2f (!))) a1 (+ a0 step)
					d (pal-wedge-d r0 r1 (+ a0 gap) (- a1 gap))
					fill (if (eql what :color) val +pal_back))
				(bind '(cx cy) (pal-point (* 0.5 (+ r0 r1)) (* 0.5 (+ a0 a1))))
				(push wedges (list k what val a0 a1 r0 r1
					(cwb-shape d :fill fill :stroke (if (eql what :color) +pal_back 0) :width 1.5)
					(cwb-shape d :fill (if (eql what :color) val +pal_hot) :stroke +pal_ink :width 2.0)
					(cwb-shape (cwb-d "M" (first (pal-point (- r1 3.0) (+ a0 gap gap))) (second (pal-point (- r1 3.0) (+ a0 gap gap)))
						"A" (- r1 3.0) (- r1 3.0) 0 0 1
						(first (pal-point (- r1 3.0) (- a1 gap gap))) (second (pal-point (- r1 3.0) (- a1 gap gap))))
						:stroke +pal_mark :width 3.5 :cap1 :round :cap2 :round)
					(if (nempty? (defq icon (pal-icon what val)))
						(cwb-group icon :m (cwb-mat-move cx cy))))))
				entries))
			(list (map (# (list :tool %0)) +pal_tools)
				(cat (map (# (list :color %0)) (ifn (get :menu_colors (get :board this)) +pal_colors))
					(map (# (list :width %0)) (ifn (get :menu_widths (get :board this)) +pal_widths)))
				(map (# (list :action %0)) +pal_actions)))
		(def this :wedges wedges)
		this)

	(defmethod :get_wedges ()
		; (. palette :get_wedges) -> wedges
		(unless (get :wedges this) (. this :build))
		(get :wedges this))

	(defmethod :to_own (x y)
		; (. palette :to_own x y) -> (x y)
		;a point of the document, about the middle of the palette at the
		;size it is on the screen
		(bind '(ox oy) (get :origin this))
		(defq zoom (get :zoom (get :board this)))
		(list (* (- (n2f x) ox) zoom) (* (- (n2f y) oy) zoom)))

	(defmethod :wedge_at (px py)
		; (. palette :wedge_at px py) -> :nil | :hub | wedge
		;what a point of its own is on
		(defq r (sqrt (+ (* px px) (* py py)))
			a (+ (path-angle px py) +fp_hpi))
		(if (< a 0.0) (setq a (+ a +fp_2pi)))
		(cond
			((<= r +pal_hub) :hub)
			(:t (some (# (if (and (>= r (elem-get %0 +wedge_r0)) (<= r (elem-get %0 +wedge_r1))
					(>= a (elem-get %0 +wedge_a0)) (< a (elem-get %0 +wedge_a1))) %0))
				(. this :get_wedges)))))

	(defmethod :find_wedge (what val)
		; (. palette :find_wedge what val) -> :nil | wedge
		(some (# (if (and (eql (elem-get %0 +wedge_what) what) (eql (elem-get %0 +wedge_val) val)) %0))
			(. this :get_wedges)))

	(defmethod :where (what &optional val)
		; (. palette :where what [val]) -> :nil | (x y)
		;the point of the document that the middle of a wedge is at, for
		;what would tap it: (:where :tool :rect), (:where :color 0xffe03131),
		;(:where :width 8.0), (:where :action :undo), and (:where :hub)
		(bind '(ox oy) (get :origin this))
		(defq zoom (get :zoom (get :board this)))
		(cond
			((eql what :hub) (list ox oy))
			((defq wedge (. this :find_wedge what val))
				(bind '(px py) (pal-point (* 0.5 (+ (elem-get wedge +wedge_r0) (elem-get wedge +wedge_r1)))
					(* 0.5 (+ (elem-get wedge +wedge_a0) (elem-get wedge +wedge_a1)))))
				(list (+ ox (/ px zoom)) (+ oy (/ py zoom))))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; what its pointer has, and is given
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :value (key)
		; (. palette :value key) -> val
		;:mode, :color or :width, as the pointer it belongs to has it, a
		;pen that is bound to its own, or the board's
		(defq board (get :board this))
		(. (. board :get_bindings) :value
			(ptr-event (get :id this) (get :kind this) +pev_left 0 0) key (get key board)))

	(defmethod :set_value (key val)
		; (. palette :set_value key val) -> palette
		(defq board (get :board this) id (get :id this))
		(cond
			((eql (get :kind this) :pen)
				;the pen, by its id, with its tip down, is what it was and this
				(defq what (list :tool :pen :mode (. this :value :mode)
					:color (. this :value :color) :width (. this :value :width)))
				(elem-set what (inc (find key what)) val)
				(.-> (. board :get_bindings) (:unbind :pen id +pev_left) (:bind :pen id +pev_left what)))
			(:t (def board key val)))
		(. board :touch +board_dirty_overlay)
		this)

	(defmethod :pick (what &optional val)
		; (. palette :pick what [val]) -> palette
		;do what a tap on a wedge does, see (:where)
		(defq board (get :board this))
		(case what
			(:hub (. this :close))
			(:tool (. this :set_value :mode val) (. this :close))
			(:color (. this :set_value :color val)
				(if (nempty? (. board :get_selected)) (. board :style :stroke val)))
			(:width (. this :set_value :width val)
				(if (nempty? (. board :get_selected)) (. board :style :width val)))
			(:action
				(bind '(ox oy) (get :origin this))
				(case val
					(:undo (. board :undo))
					(:redo (. board :redo))
					(:ruler (. (. board :get_stage) :add (Ruler board ox oy)) (. this :close))
					(:protractor (. (. board :get_stage) :add (Protractor board ox oy)) (. this :close))
					(:full_protractor (. (. board :get_stage) :add (Circle board ox oy)) (. this :close))
					(:set_square (. (. board :get_stage) :add (Setsquare board ox oy)) (. this :close))
					(:snap (def board :snap (if (= (get :snap board) 0.0)
						(n2f (. (. board :get_doc) :find :grid)) 0.0)))
					(:duplicate (. board :duplicate) (. this :close))
					(:delete (. board :delete) (. this :close)))))
		(. board :touch +board_dirty_overlay)
		this)

	(defmethod :close ()
		; (. palette :close) -> palette
		;it shuts, and is off the stage when it has, at a (:tick)
		(unless (get :shut this) (def this :shut (get :now this) :hover :nil))
		(. (get :board this) :touch +board_dirty_overlay)
		this)

	(defmethod :dismiss (event)
		; (. palette :dismiss event) -> :t | :nil
		;a pointer has gone down on the board, off this. It shuts if the
		;pointer is the one it belongs to: of its kind, and for a pen the
		;same pen. Did it
		(when (and (not (get :shut this)) (eql (elem-get event +pev_kind) (get :kind this))
				(or (not (eql (get :kind this) :pen)) (= (elem-get event +pev_id) (get :id this))))
			(. this :close)
			:t))

	;;;;;;;;;;
	; pointers
	;;;;;;;;;;

	(defmethod :hit (x y)
		; (. palette :hit x y) -> :t | :nil
		;all within its outer ring is on it, till it shuts
		(unless (get :shut this)
			(bind '(px py) (. this :to_own x y))
			(defq r (second (last +pal_radii)))
			(<= (+ (* px px) (* py py)) (* r r))))

	(defmethod :hot (wedge)
		; (. palette :hot wedge) -> palette
		;the wedge a pointer is on is drawn so
		(unless (eql wedge (get :hover this))
			(def this :hover wedge)
			(. (get :board this) :touch +board_dirty_overlay))
		this)

	(defmethod :pointers (stage events)
		; (. palette :pointers stage events) -> palette
		;a pointer that comes up on a wedge picks it, wherever on the
		;palette it went down
		(each (lambda (event)
			(bind '(pid pkind buttons x y &ignore) event)
			(bind '(px py) (. this :to_own x y))
			(defq wedge (. this :wedge_at px py) state (. this :state pid))
			(cond
				((/= buttons 0)
					(unless state (. this :set_state pid (list wedge)))
					(. this :hot wedge))
				(state
					(. this :set_state pid :nil)
					(. this :hot :nil)
					(cond
						((eql wedge :hub) (. this :pick :hub))
						(wedge (. this :pick (elem-get wedge +wedge_what) (elem-get wedge +wedge_val)))))
				(:t (. this :hot wedge))))
			events)
		this)

	(defmethod :leave (stage pid)
		(. this :hot :nil))

	;;;;;;;;;;;;;;;;;;
	; time and drawing
	;;;;;;;;;;;;;;;;;;

	(defmethod :tick (time)
		; (. palette :tick time) -> :t | :nil
		;the time is now that, microseconds. Is it still moving
		(def this :now time)
		(cond
			((defq shut (get :shut this))
				(cond
					((>= (- time shut) +pal_shut)
						(. (. (get :board this) :get_stage) :sub this)
						(def this :gone :t)
						;it is gone, and what is over the board is drawn once
						;more, without it. It is no longer moving, so nothing
						;else would, and the last of it as it shut, small,
						;was left on the board till something else was drawn
						(. (get :board this) :touch +board_dirty_overlay)
						:nil)
					(:t :t)))
			((< (- time (get :start this)) (+ +pal_open (* 3 +pal_lag))) :t)))

	(defmethod :ring_scale (k)
		; (. palette :ring_scale k) -> num
		;how far open a ring is, 0 for not at all, 1 for open, by the time
		(defq now (get :now this)
			open (pal-ease (/ (n2f (- now (get :start this) (* k +pal_lag))) (n2f +pal_open))))
		(if (defq shut (get :shut this))
			(* open (- 1.0 (max 0.0 (min 1.0 (/ (n2f (- now shut)) (n2f +pal_shut))))))
			open))

	(defmethod :draw (canvas &optional m)
		; (. palette :draw canvas [m]) -> palette
		;on a canvas, by the matrix the board is seen by. Each ring is as
		;far open as it is, and turns as it opens
		(defq board (get :board this) hover (get :hover this) wedges (. this :get_wedges)
			mode (. this :value :mode) color (. this :value :color) width (. this :value :width)
			snap (/= (get :snap board) 0.0))
		(bind '(ox oy) (get :origin this))
		(defq base (cwb-mat-mul m (cwb-mat-mul (cwb-mat-move ox oy) (cwb-mat-scale (/ 1.0 (get :zoom board))))))
		;the outer ring first, so one that is part open is under those inside it
		(each (lambda (k)
			(defq open (. this :ring_scale k))
			(when (> open 0.02)
				(defq items (list))
				(each (lambda (wedge)
					(when (= (elem-get wedge +wedge_ring) k)
						(defq what (elem-get wedge +wedge_what) val (elem-get wedge +wedge_val))
						(push items (elem-get wedge (if (eql wedge hover) +wedge_hot +wedge_back)))
						(if (elem-get wedge +wedge_icon) (push items (elem-get wedge +wedge_icon)))
						(if (case what
								(:tool (eql val mode))
								(:color (= val color))
								(:width (= val width))
								(:action (and (eql val :snap) snap)))
							(push items (elem-get wedge +wedge_mark)))))
					wedges)
				(cwb-draw-items canvas items (cwb-mat-mul base
					(cwb-mat-mul (cwb-mat-scale open) (cwb-mat-turn (* (- 1.0 open) -0.8)))))))
			'(2 1 0))
		;the middle, a spot of the colour, as wide as the width
		(defq open (. this :ring_scale 0))
		(when (> open 0.02)
			(cwb-draw-items canvas (list
				(cwb-shape (cwb-d-ellipse 0 0 +pal_hub) :fill (if (eql hover :hub) +pal_hot +pal_back)
					:stroke +pal_ink :width 1.5)
				(cwb-shape (cwb-d-ellipse 0 0 (max 2.0 (min 16.0 (* 0.5 (n2f width)))))
					:fill color :stroke +pal_ink :width 1.0))
				(cwb-mat-mul base (cwb-mat-scale open))))
		this)
	)

(defun palette-open (board x y &optional kind id)
	; (palette-open board x y [kind id]) -> palette
	;a palette on the board, about a point, for the pointer of that kind
	;and id, default the mouse. One that pointer had open shuts. It is kept
	;on the document, if that is big enough to hold it
	(setd kind :mouse id 0)
	(defq stage (. board :get_stage) doc (. board :get_doc)
		r (/ (second (last +pal_radii)) (get :zoom board))
		w (n2f (. doc :find :width)) h (n2f (. doc :find :height)) x (n2f x) y (n2f y))
	(if (> w (* 2.0 r)) (setq x (max r (min (- w r) x))))
	(if (> h (* 2.0 r)) (setq y (max r (min (- h r) y))))
	(each (# (if (and (Palette? %0) (eql (get :kind %0) kind) (= (get :id %0) id)) (. %0 :close)))
		(. stage :get_actors))
	(. stage :add (defq palette (Palette board x y kind id)))
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
