;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/tools.inc - the instruments of a board: ruler, protractor, set square
;
; An instrument lies on the board, in front of what is drawn, and is not
; part of the document. What is drawn with it is.
;
; It is where it is, turned as it is and as long as it is, and everything
; else about it is said in its own space, about its own middle, as paths:
;
;	its outline, what a point has to be in to be on it;
;	its edges, each a line or an arc a pen can be run along. A pen that
;	goes down near an edge is held to it, and what it draws is that edge
;	from where it went down to where it is, a true line or a true arc, not
;	the wobble of the hand. More than one pen can draw along it at once;
;	its parts, each a region and what a pointer that goes down in it does:
;	move the instrument, turn it, make it longer or shorter, put it away.
;	Only a pointer that is alone on it does those, one that holds it
;	still while another draws does not move it;
;	its marks, the ticks and numbers, which are only drawn.
;
; Two fingers on it move and turn it together.
;
; So an instrument is a table and a few lines, and another is made by
; writing another table, (Ruler) and (Protractor) below are each that.
; Chris Hinsley wrote the whiteboards that schools have on the wall, and
; this way of making instruments, edges that hold a pen and regions that
; do things, is his from then. This is that idea made again here, not
; that code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./board.inc")

;how near an edge a pen has to go down to be held to it, as the screen has it
(defq +tool_band 14.0)

;an instrument that is turned is pulled to the angles that matter, when it
;is near one. Each is how far apart the angles are and from how far off
;one pulls, in degrees: every 45 pulls from 2.5 away, every 10, where a
;protractor has its long marks and its numbers, from 0.75. The first that
;pulls is the one
(defq +tool_magnets ''((45.0 2.5) (10.0 0.75)))

(defun tool-magnet (angle)
	; (tool-magnet angle) -> angle
	;an angle, in radians, or the one that matters that it is near
	;by 180 and then over pi, and back by pi and then over 180: a degree
	;in radians is too small a number to be held well
	(defq deg (/ (* angle 180.0) +fp_pi))
	(if (defq to (some (lambda ((step pull))
			(defq near (board-snap deg step))
			(if (<= (abs (- deg near)) pull) near)) +tool_magnets))
		(/ (* to +fp_pi) 180.0)
		angle))

(defun tool-glyph (what)
	; (tool-glyph what) -> shapes
	;the sign of a part of an instrument, about 0 0, some 20 across: an
	;arrow that goes round for :turn, one with two heads for :size, a
	;cross for :close
	(defq ink 0xff000000)
	(case what
		(:turn (list (cwb-shape "M -7 4 A 8 8 0 1 1 7 4 M 7 4 L 3 3.5 M 7 4 L 8.5 0 M -7 4 L -3 3.5 M -7 4 L -8.5 0"
			:stroke ink :width 1.6 :join :round)))
		(:size (list (cwb-shape "M -9 0 L 9 0 M -9 0 L -5 -4 M -9 0 L -5 4 M 9 0 L 5 -4 M 9 0 L 5 4"
			:stroke ink :width 1.6 :join :round)))
		(:close (list (cwb-shape (cat (cwb-d-ellipse 0 0 9) " M -4.5 -4.5 L 4.5 4.5 M 4.5 -4.5 L -4.5 4.5")
			:stroke ink :width 1.4)))
		(:t (list))))

(defun tool-ticks (x0 y0 x1 y1 nx ny step &optional small mid big)
	; (tool-ticks x0 y0 x1 y1 nx ny step [small mid big]) -> d
	;marks along a line, every step from its start, each a little line
	;the way nx ny points, which is one long. Every fifth is longer and
	;every tenth longer still
	(setd small 5.0 mid 9.0 big 14.0)
	(defq dx (- x1 x0) dy (- y1 y0) len (sqrt (+ (* dx dx) (* dy dy))) out (list) n 0 at 0.0)
	(when (> len 0.0)
		(setq dx (/ dx len) dy (/ dy len))
		(while (<= at (+ len 0.01))
			(defq tick (cond ((= (% n 10) 0) big) ((= (% n 5) 0) mid) (:t small))
				x (+ x0 (* dx at)) y (+ y0 (* dy at)))
			(push out "M" x y "L" (+ x (* nx tick)) (+ y (* ny tick)))
			(setq n (inc n) at (+ at step))))
	(apply (const cwb-d) out))

(defun tool-nearest-on-line (x y x0 y0 x1 y1)
	; (tool-nearest-on-line x y x0 y0 x1 y1) -> (px py t)
	;the point of a line nearest a point, and how far along it that is, 0 to 1
	(defq dx (- x1 x0) dy (- y1 y0) len2 (+ (* dx dx) (* dy dy))
		along (if (= len2 0.0) 0.0 (max 0.0 (min 1.0 (/ (+ (* (- x x0) dx) (* (- y y0) dy)) len2)))))
	(list (+ x0 (* along dx)) (+ y0 (* along dy)) along))

(defun tool-angle-wrap (a)
	; (tool-angle-wrap a) -> a
	;an angle as one of more than -pi and no more than pi
	(while (> a +fp_pi) (setq a (- a +fp_2pi)))
	(while (<= a (neg +fp_pi)) (setq a (+ a +fp_2pi)))
	a)

;what is kept for a pointer that is down on an instrument, a list
(enums +tool_state 0
	(enum what part edge x y origin angle length shape near turned last_angle last_x last_y))

(defclass Instrument (board x y) (Actor)
	; (Instrument board x y) -> instrument
	;what every instrument is. One of a kind sets :length, how big it is
	;made, and has a :rebuild that sets what it is made of, at that size.
	;It is then seen bigger or smaller by :size, 1.0 for as made, all of
	;it, its marks and its numbers with it:
	;	:outline	a d, closed
	;	:edges		((kind ...) ...), (:line x0 y0 x1 y1) or (:arc cx cy r a0 a1)
	;	:parts		((what d) ...), what is :move :turn :size or :close,
	;				the first that a point is in is the one, so put the
	;				small ones first. A :turn can have the point it turns
	;				about after its d, x y, default the middle. A part
	;				that ends with :quiet is not drawn darker, it is a
	;				part of the thing that is plain to see already
	;	:marks		shapes, drawn over the body
	;	:glyphs		((what x y [angle]) ...), where the sign of a part is
	;				drawn, so that it is seen what the part is for. A
	;				part other than :move is drawn too, a shade darker
	;	:readouts	((x y [turn]) ...), where the angle it is turned to
	;				is written, in degrees, the way a protractor counts,
	;				up from level. One with a turn is written turned by
	;				that, and says the angle that much on: a ruler seen
	;				from its other side
	(def this :board board :origin (list (n2f x) (n2f y)) :angle 0.0
		:length 400.0 :size 1.0 :min_size 0.4 :max_size 5.0 :readouts (list) :said :nil
		:outline "" :edges (list) :parts (list) :marks (list) :glyphs (list)
		:mode :line :body :nil :regions :nil :zones :nil :grab :nil)

	(defmethod :rebuild ()
		; (. instrument :rebuild) -> instrument
		this)

	(defmethod :extent ()
		; (. instrument :extent) -> num
		;how big it is, the number that is made more or less when it is
		;sized. :size, how much bigger than it was made all of it is seen.
		;One of a kind may have another: a ruler has how long it is
		(get :size this))

	(defmethod :set_extent (extent)
		; (. instrument :set_extent extent) -> instrument
		;it is that big, or as near as it may be
		(def this :size (max (get :min_size this) (min (get :max_size this) extent)))
		(. (get :board this) :touch +board_dirty_overlay)
		this)

	(defmethod :built ()
		; (. instrument :built) -> instrument
		;its length or what it is has changed, it is made again
		(. this :rebuild)
		(def this :body (cwb-shape (get :outline this) :fill 0x70808080 :stroke 0xff000000 :width 1.0 :join :miter)
			:regions (map (lambda ((what d &rest pivot))
					(list what (cwb-shape d :fill 0xff000000 :stroke 0) (filter (# (not (eql %0 :quiet))) pivot)))
				(get :parts this))
			;what is seen of the parts: each that is not the one to move it
			;by, a shade darker with a line round it, and the signs
			:zones (cat
				(map (lambda ((what d &ignore)) (cwb-shape d :fill 0x30000000 :stroke 0x80000000 :width 0.75 :join :miter))
					(filter (# (not (or (find (first %0) '(:move :close)) (eql (last %0) :quiet)))) (get :parts this)))
				(map (lambda (glyph)
					(cwb-group (tool-glyph (first glyph)) :m (cwb-mat-mul (cwb-mat-move (second glyph) (third glyph))
						(cwb-mat-turn (if (> (length glyph) 3) (elem-get glyph 3) 0.0)))))
					(get :glyphs this))))
		(. (get :board this) :touch +board_dirty_overlay)
		this)

	(defmethod :matrix ()
		; (. instrument :matrix) -> matrix
		;from its own space to the board's
		(bind '(ox oy) (get :origin this))
		(cwb-mat-mul (cwb-mat-move ox oy)
			(cwb-mat-mul (cwb-mat-turn (get :angle this)) (cwb-mat-scale (get :size this)))))

	(defmethod :to_own (x y)
		; (. instrument :to_own x y) -> (x y)
		(cwb-mat-point (cwb-mat-invert (. this :matrix)) x y))

	(defmethod :to_board (x y)
		; (. instrument :to_board x y) -> (x y)
		(cwb-mat-point (. this :matrix) x y))

	(defmethod :edge_near (x y)
		; (. instrument :edge_near x y) -> :nil | (edge px py)
		;the edge a point, in its own space, is near enough to be held to,
		;and the point of the edge that is nearest
		;the nearest, and of two as near the first, so the order they are
		;given in says which has a corner where two meet
		(defq band (/ +tool_band (* (get :zoom (get :board this)) (get :size this))) best :nil best_d (+ band 0.001))
		(each (lambda (edge)
			(defq near (. this :on_edge edge x y) dx (- x (first near)) dy (- y (second near))
				d (sqrt (+ (* dx dx) (* dy dy))))
			(when (< d best_d) (setq best_d d best (list edge (first near) (second near)))))
			(get :edges this))
		best)

	(defmethod :on_edge (edge x y)
		; (. instrument :on_edge edge x y) -> (px py)
		;the point of an edge nearest a point, both in its own space
		(case (first edge)
			(:arc (bind '(kind cx cy r a0 a1) edge)
				(defq a (path-angle (- x cx) (- y cy)))
				;an arc that is not all the way round ends where it ends
				(when (< (- a1 a0) (const (- +fp_2pi 0.001)))
					(defq mid (* 0.5 (+ a0 a1)) half (* 0.5 (- a1 a0)) off (tool-angle-wrap (- a mid)))
					(setq a (+ mid (max (neg half) (min half off)))))
				(list (+ cx (* r (cos a))) (+ cy (* r (sin a)))))
			(:t (bind '(kind x0 y0 x1 y1) edge)
				(most (tool-nearest-on-line x y x0 y0 x1 y1)))))

	(defmethod :part_at (x y)
		; (. instrument :part_at x y) -> :nil | (what shape pivot)
		;the part a point, in its own space, is in
		(some (lambda (region) (if (cwb-shape-hit? (second region) :nil x y) region)) (get :regions this)))

	(defmethod :hit (x y)
		;on it, or near enough one of its edges to draw along it
		(unless (get :body this) (. this :built))
		(bind '(lx ly) (. this :to_own x y))
		(or (cwb-shape-hit? (get :body this) :nil lx ly) (if (. this :edge_near lx ly) :t)))

	(defmethod :edge_d (state x y)
		; (. instrument :edge_d state x y) -> d
		;what a pen held to an edge has drawn, now it is at a point, in the
		;board's space. A line along a line. Along an arc, the arc from
		;where it went down, round the way it has gone, or with :mode :pie
		;a slice of that, or :circle all the way round
		(bind '(edge sx sy) (elem-get state +tool_state_near))
		(bind '(px py) (. this :on_edge edge x y))
		(case (first edge)
			(:arc (bind '(kind cx cy r &ignore) edge)
				(defq a (path-angle (- px cx) (- py cy))
					turned (+ (elem-get state +tool_state_turned)
						(tool-angle-wrap (- a (elem-get state +tool_state_last_angle))))
					a_from (path-angle (- sx cx) (- sy cy)))
				(elem-set state +tool_state_turned turned)
				(elem-set state +tool_state_last_angle a)
				;where it is on the board: its middle, and its turn added to each angle
				(bind '(bcx bcy) (. this :to_board cx cy))
				;and it is as big on the board as the instrument is seen
				(defq turn (get :angle this) mode (get :mode this) r (* r (get :size this))
					a0 (+ turn (if (>= turned 0.0) a_from (+ a_from turned)))
					a1 (+ turn (if (>= turned 0.0) (+ a_from turned) a_from)))
				(cond
					((or (eql mode :circle) (>= (abs turned) +fp_2pi)) (cwb-d-ellipse bcx bcy r))
					((= turned 0.0) "")
					(:t (cwb-d-arc bcx bcy r a0 a1 (eql mode :pie)))))
			(:t (bind '(bx0 by0) (. this :to_board sx sy))
				(bind '(bx1 by1) (. this :to_board px py))
				(cwb-d-line bx0 by0 bx1 by1))))

	(defmethod :begin (stage event)
		; (. instrument :begin stage event) -> state
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(lx ly) (. this :to_own x y))
		(defq near (. this :edge_near lx ly) region (. this :part_at lx ly)
			tool (. (get :bindings board) :value event :tool :pen)
			state (list :none region :nil x y (cat (get :origin this)) (get :angle this) (. this :extent)
				:nil near 0.0 0.0 x y))
		(cond
			((and near (find tool '(:pen)))
				;held to an edge, and what it draws is that edge
				(bind '(edge px py) near)
				(elem-set state +tool_state_what :draw)
				(when (eql (first edge) :arc)
					(bind '(kind cx cy &ignore) edge)
					(elem-set state +tool_state_last_angle (path-angle (- px cx) (- py cy))))
				(elem-set state +tool_state_shape (. board :new_shape event :line)))
			(region (elem-set state +tool_state_what (first region))))
		state)

	(defmethod :step (stage event state)
		; (. instrument :step stage event state)
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(what region edge sx sy origin angle length shape near &ignore) state)
		(elem-set state +tool_state_last_x x)
		(elem-set state +tool_state_last_y y)
		(case what
			(:draw
				(bind '(lx ly) (. this :to_own x y))
				(cwb-set shape :d (. this :edge_d state lx ly))
				(. board :touch +board_dirty_overlay))
			(:move
				(when (and (= (. this :held) 1) (not (get :grab this)))
					(def this :origin (list (+ (first origin) (- x sx)) (+ (second origin) (- y sy))))
					(. board :touch +board_dirty_overlay)))
			(:turn
				(when (and (= (. this :held) 1) (not (get :grab this)))
					;about a point of it, which stays where it is on the board
					(defq pivot (third region) m0 (cwb-mat-mul (cwb-mat-move (first origin) (second origin))
						(cwb-mat-mul (cwb-mat-turn angle) (cwb-mat-scale (get :size this)))))
					(bind '(bx by) (if (nempty? pivot) (cwb-mat-point m0 (first pivot) (second pivot)) origin))
					(defq turn (tool-angle-wrap (- (path-angle (- x bx) (- y by)) (path-angle (- sx bx) (- sy by))))
						snap (get :snap_angle board))
					;with angles that snap it is at a step and nowhere else. If
					;not, it is free, but for the pull of the angles that matter
					(setq turn (- (if (/= snap 0.0) (board-snap (+ angle turn) snap) (tool-magnet (+ angle turn))) angle))
					(def this :angle (+ angle turn)
						:origin (cwb-mat-point (cwb-mat-turn turn bx by) (first origin) (second origin)))
					(. board :touch +board_dirty_overlay)))
			(:size
				(when (and (= (. this :held) 1) (not (get :grab this)))
					;by how much further from its middle the pointer is
					(defq d0 (max 1.0 (sqrt (+ (* (- sx (first origin)) (- sx (first origin))) (* (- sy (second origin)) (- sy (second origin))))))
						d1 (sqrt (+ (* (- x (first origin)) (- x (first origin))) (* (- y (second origin)) (- y (second origin))))))
					(. this :set_extent (* length (/ d1 d0)))))))

	(defmethod :end (stage event state)
		; (. instrument :end stage event state)
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(what region edge sx sy origin angle length shape &ignore) state)
		(case what
			(:draw
				(. board :drop_temp id)
				(if (third (cwb-flat shape)) (. board :commit shape)))
			(:close
				;put away, if it came up where it went down
				(bind '(lx ly) (. this :to_own x y))
				(when (and (defq now (. this :part_at lx ly)) (eql (first now) :close))
					(. stage :sub this)
					(. board :touch +board_dirty_overlay)))))

	(defmethod :pointers (stage events)
		(unless (get :body this) (. this :built))
		(each (lambda (event)
			(bind '(id kind buttons &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(. this :set_state id (. this :begin stage event))
					(. this :step stage event (. this :state id)))
				((and state (/= buttons 0)) (. this :step stage event state))
				(state
					(. this :step stage event state)
					(. this :end stage event state)
					(. this :set_state id :nil))))
			events)
		(. this :two_hands)
		this)

	(defmethod :two_hands ()
		; (. instrument :two_hands)
		;two pointers on its parts, not its edges, whichever parts they are,
		;move it, turn it and size it together, by where they were when the
		;second went down and where they are
		(defq hands (list) grab (get :grab this))
		(. (get :states this) :each (lambda (id state)
			(if (find (elem-get state +tool_state_what) '(:move :turn :size)) (push hands state))))
		(cond
			((< (length hands) 2)
				(when grab
					(def this :grab :nil)
					;the one that is left carries on from where things are
					(each (lambda (state)
						(elem-set state +tool_state_x (elem-get state +tool_state_last_x))
						(elem-set state +tool_state_y (elem-get state +tool_state_last_y))
						(elem-set state +tool_state_origin (cat (get :origin this)))
						(elem-set state +tool_state_angle (get :angle this))
						(elem-set state +tool_state_length (. this :extent))) hands)))
			(:t (defq a (first hands) b (second hands)
					pa (list (elem-get a +tool_state_last_x) (elem-get a +tool_state_last_y))
					pb (list (elem-get b +tool_state_last_x) (elem-get b +tool_state_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb (cat (get :origin this)) (get :angle this) (. this :extent)))))
				(bind '(pa0 pb0 origin angle length) grab)
				;it is as much bigger as they are further apart, within what it can be
				(defq d0 (max 1.0 (sqrt (+ (* (- (first pb0) (first pa0)) (- (first pb0) (first pa0)))
						(* (- (second pb0) (second pa0)) (- (second pb0) (second pa0))))))
					d1 (sqrt (+ (* (- (first pb) (first pa)) (- (first pb) (first pa)))
						(* (- (second pb) (second pa)) (- (second pb) (second pa)))))
					by (/ (. (. this :set_extent (* length (/ d1 d0))) :extent) length))
				(defq turn (tool-angle-wrap (- (path-angle (- (first pb) (first pa)) (- (second pb) (second pa)))
						(path-angle (- (first pb0) (first pa0)) (- (second pb0) (second pa0)))))
					;pulled to the angles that matter, as it is by one pointer
					turn (- (tool-magnet (+ angle turn)) angle)
					m0x (* 0.5 (+ (first pa0) (first pb0))) m0y (* 0.5 (+ (second pa0) (second pb0)))
					m1x (* 0.5 (+ (first pa) (first pb))) m1y (* 0.5 (+ (second pa) (second pb)))
					m (cwb-mat-mul (cwb-mat-move (- m1x m0x) (- m1y m0y))
						(cwb-mat-mul (cwb-mat-turn turn m0x m0y) (cwb-mat-scale by by m0x m0y))))
				(def this :angle (+ angle turn) :origin (cwb-mat-point m (first origin) (second origin)))
				(. (get :board this) :touch +board_dirty_overlay))))

	(defmethod :draw (canvas &optional m)
		; (. instrument :draw canvas [m]) -> instrument
		;on a canvas, by the matrix the board is seen by
		(unless (get :body this) (. this :built))
		(defq m (cwb-mat-mul m (. this :matrix)))
		(cwb-draw-items canvas (list (get :body this)) m)
		(cwb-draw-items canvas (get :zones this) m)
		(cwb-draw-items canvas (get :marks this) m)
		(cwb-draw-items canvas (. this :readout) m)
		this)

	(defmethod :degrees (&optional turn)
		; (. instrument :degrees [turn]) -> str
		;the angle it is turned to, in degrees, to a tenth, counted the way
		;a protractor's numbers go, up from level, 0 to 360. With a turn,
		;that much on
		(defq deg (/ (* (neg (+ (get :angle this) (ifn turn 0.0))) 180.0) +fp_pi)
			tenths (% (+ (% (n2i (floor (+ (* deg 10.0) 0.5))) 3600) 3600) 3600))
		(cat (str (/ tenths 10)) (if (= (% tenths 10) 0) "" (cat "." (str (% tenths 10))))))

	(defmethod :readout ()
		; (. instrument :readout) -> shapes
		;the angle, written at each of its :readouts, made again only when
		;what it says is not what it said
		(defq says (map (lambda (at) (. this :degrees (if (> (length at) 2) (third at)))) (get :readouts this))
			said (get :said this))
		(unless (and said (= (length (first said)) (length says)) (every (const eql) (first said) says))
			(def this :said (setq said (list says (map (lambda (at text)
				(defq shape (cwb-text-mid text 0 0 :font_size 13))
				(elem-set shape +cwb_m (cwb-mat-mul (cwb-mat-move (first at) (second at))
					(cwb-mat-mul (cwb-mat-turn (if (> (length at) 2) (third at) 0.0)) (elem-get shape +cwb_m))))
				shape) (get :readouts this) says)))))
		(second said))
	)

;;;;;;;;;;;
; the ruler
;;;;;;;;;;;

;a ruler is that high, each end of it is that wide, and a millimetre of its
;marks is that far, a centimetre is ten of them
(defq +ruler_height 80.0 +ruler_cap 26.0 +tool_mm 5.0)

(defclass Ruler (board x y) (Instrument board x y)
	; (Ruler board x y) -> ruler
	;a straight edge, its two long sides are edges. Each end makes it
	;longer or shorter, a longer ruler with more marks, no wider. The hole by each end turns it, about the 0 of the
	;marks at its other end. The rest moves it, and the ring puts it away. It is
	;marked in millimetres along both sides, the bottom the other way to
	;the top, longer every five, and numbered in centimetres
	(def this :min_length 200.0 :max_length 3000.0)

	;a ruler that is sized is a longer ruler, or a shorter, as wide as it
	;was, with its marks as far apart and more of them or fewer. It is not
	;seen bigger, a ruler is to be read
	(defmethod :extent () (get :length this))

	(defmethod :set_extent (extent)
		(defq length (max (get :min_length this) (min (get :max_length this) extent)))
		(unless (= length (get :length this))
			(def this :length length)
			(. this :built))
		this)

	(defmethod :rebuild ()
		(defq half (* 0.5 (get :length this)) top (* -0.5 +ruler_height) bot (* 0.5 +ruler_height)
			inset (- half +ruler_cap) marks (list))
		(def this :outline (cwb-d-rect (neg half) top half bot)
			:edges (list (list :line (neg inset) top inset top) (list :line (neg inset) bot inset bot))
			:parts (list
				(list :close (cwb-d-ellipse 0 0 11))
				(list :size (cwb-d-rect (neg half) top (neg inset) bot))
				(list :size (cwb-d-rect inset top half bot))
				;the hole by each end, as a ruler has to hang it by. Each
				;turns it about the 0 of the marks at the other end: the top
				;side's are from the left, the bottom side's from the right.
				;With a 0 on a point, lines are drawn from the point every way
				(list :turn (cwb-d-ellipse (+ (neg inset) 24.0) 0.0 12.0) inset bot)
				(list :turn (cwb-d-ellipse (- inset 24.0) 0.0 12.0) (neg inset) top)
				(list :move (cwb-d-rect (neg half) top half bot)))
			:glyphs (list (list :close 0.0 0.0)
				(list :size (- (neg half) (* -0.5 +ruler_cap)) 0.0) (list :size (- half (* 0.5 +ruler_cap)) 0.0)
				(list :turn (+ (neg inset) 24.0) 0.0) (list :turn (- inset 24.0) 0.0)))
		;centimetres along the top side, in millimetres, from its left end.
		;Inches along the bottom side, in sixteenths, from its right end,
		;and written the other way up: the bottom is the top turned half
		;round about the middle, to be read from the other side
		(defq inch (* 25.4 +tool_mm) ticks (list (tool-ticks (neg inset) top inset top 0.0 1.0 +tool_mm))
			n 0 at 0.0 flip (lambda (shape x y)
				;words, about their own middle, turned half round, and put at a point
				(elem-set shape +cwb_m (cwb-mat-mul (cwb-mat-move x y) (cwb-mat-mul (cwb-mat-turn +fp_pi) (elem-get shape +cwb_m))))
				shape))
		(while (<= at (+ (* 2.0 inset) 0.01))
			(defq tick (cond ((= (% n 16) 0) 14.0) ((= (% n 8) 0) 11.0) ((= (% n 4) 0) 8.0) ((= (% n 2) 0) 6.0) (:t 4.0)))
			(push ticks (cwb-d "M" (- inset at) bot "L" (- inset at) (- bot tick)))
			(if (and (= (% n 16) 0) (< at (- (* 2.0 inset) 6.0)))
				(push marks (flip (cwb-text-mid (str (/ n 16)) 0 0 :font_size 13) (- inset at (if (= n 0) 6.0 0.0)) (- bot 22.0))))
			(setq n (inc n) at (/ (* (n2f n) inch) 16.0)))
		(push marks (cwb-shape (join ticks " ") :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(setq n 0 at 0.0)
		(while (< at (- (* 2.0 inset) 6.0))
			(push marks (cwb-text-mid (str n) (+ (neg inset) at (if (= n 0) 6.0 0.0)) (+ top 22.0) :font_size 13))
			(setq n (inc n) at (+ at (* 10.0 +tool_mm))))
		;what each side is in, by its 0
		(push marks (cwb-text-mid "cm" (+ (neg inset) 26.0) (+ top 23.0) :font_size 10))
		(push marks (flip (cwb-text-mid "in" 0 0 :font_size 10) (- inset 24.0) (- bot 23.0)))
		;the angle it is at, either side of its middle, the one for who
		;reads the top side and the one for who reads the bottom
		(def this :readouts (list (list 44.0 0.0) (list -44.0 0.0 +fp_pi)))
		(def this :marks marks)
		this)
	)

;;;;;;;;;;;;;;;;
; the protractor
;;;;;;;;;;;;;;;;

(defclass Protractor (board x y) (Instrument board x y)
	; (Protractor board x y) -> protractor
	;half a circle and a little more, its round side goes on round below
	;the line through its middle to its straight side. The two are edges. A pen
	;run round the round side draws an arc, with :mode :pie a slice, with
	;:mode :circle the whole circle. :length is across its straight side.
	;The band half way in turns it about its middle, the two squares by
	;its straight side make it bigger or smaller, and so does the outer
	;scale, the rest, its middle, moves it, the
	;ring puts it away. Marked every degree, longer every five and ten,
	;numbered every ten, with a cross where its middle is
	(def this :length 360.0)

	(defmethod :rebuild ()
		(defq r (* 0.5 (get :length this)) in0 (* 0.46 r) in1 (* 0.62 r) marks (list) ticks (list)
			sq0 (* 0.16 r) sq1 (* 0.38 r) sqy0 (* -0.24 r) sqy1 (* -0.08 r)
			;the strip below the line through its middle, as a protractor
			;has, its straight side is the bottom of that
			;the round side goes on round, past the line through the middle,
			;to meet the straight side: it has the one straight side and the
			;one round one. A tenth of the radius down, the circle is that
			;much less than the radius across, and that much angle further
			base (* 0.1 r) across (* 0.99499 r) past 0.10017
			whole (cwb-d "M" (neg across) base "A" r r 0 0 1 0 (neg r) "A" r r 0 0 1 across base "Z"))
		(def this :outline whole
			:edges (list (list :arc 0.0 0.0 r (- (neg +fp_pi) past) past) (list :line (neg across) base across base))
			:parts (list
				(list :close (cwb-d-ellipse 0 (* -0.3 r) 11))
				(list :size (cwb-d-rect sq0 sqy0 sq1 sqy1))
				(list :size (cwb-d-rect (neg sq1) sqy0 (neg sq0) sqy1))
				(list :turn (cwb-d "M" (neg in1) 0 "A" in1 in1 0 0 1 in1 0 "L" in0 0 "A" in0 in0 0 0 0 (neg in0) 0 "Z"))
				;the outer scale, all of it outside the band that turns it,
				;sizes it too, as the squares do. A pen close on the round
				;side is held to it and draws, that comes first
				(list :size (cwb-d "M" (neg r) 0 "A" r r 0 0 1 r 0 "L" in1 0 "A" in1 in1 0 0 0 (neg in1) 0 "Z") :quiet)
				(list :move whole))
			:glyphs (list (list :close 0.0 (* -0.3 r))
				(list :size (* 0.5 (+ sq0 sq1)) (* 0.5 (+ sqy0 sqy1))) (list :size (* -0.5 (+ sq0 sq1)) (* 0.5 (+ sqy0 sqy1)))
				(list :turn (* 0.54 r 0.7071) (* -0.54 r 0.7071) (* 0.25 +fp_pi))
				(list :turn (* -0.54 r 0.7071) (* -0.54 r 0.7071) (* -0.25 +fp_pi))))
		;a mark every degree round from the right, longer every five and every ten
		(each (lambda (deg)
			(defq a (neg (* (n2f deg) (const (/ +fp_pi 180.0)))) c (cos a) s (sin a)
				tick (cond ((= (% deg 10) 0) 15.0) ((= (% deg 5) 0) 10.0) (:t 6.0)))
			(push ticks "M" (* r c) (* r s) "L" (* (- r tick) c) (* (- r tick) s))
			(if (and (= (% deg 10) 0) (> deg 0) (< deg 180))
				(push marks (cwb-text-mid (str deg) (* (- r 27.0) c) (* (- r 27.0) s) :font_size 12))))
			(range 0 181))
		;the band that turns it is the inner scale, as a protractor has, that
		;reads the other way round, marked every five and numbered every thirty
		(each (lambda (deg)
			(defq a (neg (* (n2f deg) (const (/ +fp_pi 180.0)))) c (cos a) s (sin a)
				tick (if (= (% deg 10) 0) 8.0 4.0))
			(push ticks "M" (* in1 c) (* in1 s) "L" (* (- in1 tick) c) (* (- in1 tick) s))
			(if (and (= (% deg 30) 0) (> deg 0) (< deg 180))
				(push marks (cwb-text-mid (str (- 180 deg)) (* (- in1 17.0) c) (* (- in1 17.0) s) :font_size 10))))
			(range 0 181 5))
		;its middle, on the straight side, and the line straight up from it
		;and the line through it from 0 to 180, with the strip below marked
		;in millimetres along the straight side
		(push ticks "M" (neg r) 0 "L" r 0 "M" 0 base "L" 0 -14 "M" 0 (neg in0) "L" 0 (- 22.0 r))
		;out from the middle both ways, so the middle is on a long mark and
		;stays on it as the protractor is made bigger or smaller
		(push marks (cwb-shape (cat (tool-ticks 0.0 base across base 0.0 -1.0 +tool_mm 3.0 5.0 8.0)
				" " (tool-ticks 0.0 base (neg across) base 0.0 -1.0 +tool_mm 3.0 5.0 8.0))
			:stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(push marks (cwb-shape (apply (const cwb-d) ticks) :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(def this :marks marks :readouts (list (list 0.0 (* -0.16 r))))
		this)
	)

;;;;;;;;;;;;;;;;
; the set square
;;;;;;;;;;;;;;;;

(defclass Setsquare (board x y) (Instrument board x y)
	; (Setsquare board x y) -> setsquare
	;a triangle with a square corner, each of its three sides is an edge.
	;:length is its longest square side, the other is that over the root
	;of 3, so its corners are 30, 60 and 90 degrees. The two sharp corners
	;turn it about its middle, the square by its square corner makes it
	;bigger or smaller, the rest moves it. Its two square sides are
	;marked in millimetres from the square corner, and its long side from
	;a little below its top corner, numbered in centimetres
	(def this :length 360.0)

	(defmethod :rebuild ()
		(defq w (get :length this) h (* w 0.57735)
			;the square corner is bottom left, its middle is the middle of the three
			ax (* w -0.33333) ay (* h 0.33333) bx (* w 0.66667) by ay cx ax cy (* h -0.66667)
			;a corner, cut off a quarter of the way along its two sides
			corner (lambda (px py qx qy rx ry)
				(cwb-d "M" px py "L" (+ px (* 0.25 (- qx px))) (+ py (* 0.25 (- qy py)))
					(+ px (* 0.25 (- rx px))) (+ py (* 0.25 (- ry py))) "Z"))
			marks (list))
		(def this :outline (cwb-d "M" ax ay "L" bx by cx cy "Z")
			:edges (list (list :line ax ay bx by) (list :line bx by cx cy) (list :line cx cy ax ay))
			:parts (list
				(list :close (cwb-d-ellipse 0 0 11))
				(list :size (cwb-d-rect (+ ax 28.0) (- ay 62.0) (+ ax 62.0) (- ay 28.0)))
				(list :turn (corner bx by ax ay cx cy))
				(list :turn (corner cx cy ax ay bx by))
				(list :move (cwb-d "M" ax ay "L" bx by cx cy "Z")))
			:glyphs (list (list :close 0.0 0.0)
				(list :size (+ ax 45.0) (- ay 45.0) (* -0.25 +fp_pi))
				(list :turn (+ bx (* 0.11 (- ax bx)) (* 0.05 (- cx bx))) (+ by (* 0.05 (- cy by)) -4.0))
				(list :turn (+ cx (* 0.05 (- bx cx)) 6.0) (+ cy (* 0.12 (- ay cy)) (* 0.05 (- by cy))))))
		;millimetres along the two square sides, from the square corner
		;and along the long side, the marks pointing in, from a little way
		;down from its top corner to a little short of the other, as a
		;ruler's are in from its ends: at a sharp corner a mark would go
		;out through the other side
		(defq long (sqrt (+ (* w w) (* h h))) ux (/ w long) uy (/ h long))
		(push marks (cwb-shape (cat (tool-ticks ax ay (- bx 30.0) by 0.0 -1.0 +tool_mm)
				" " (tool-ticks ax ay cx (+ cy 30.0) 1.0 0.0 +tool_mm)
				" " (tool-ticks (+ cx (* 40.0 ux)) (+ cy (* 40.0 uy)) (- bx (* 40.0 ux)) (- by (* 40.0 uy)) (neg uy) ux +tool_mm))
			:stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		;what a set square has: the hole in its middle, the same shape, the
		;arc of each sharp corner with its angle, and the mark of a square corner
		(defq arc_b (* 0.25 w) arc_c (* 0.25 h))
		(push marks (cwb-shape (cat
				(cwb-d "M" (* 0.38 ax) (* 0.38 ay) "L" (* 0.38 bx) (* 0.38 by) (* 0.38 cx) (* 0.38 cy) "Z")
				" " (cwb-d-arc bx by (* 0.6 arc_b) +fp_pi (+ +fp_pi (/ +fp_pi 6.0)))
				" " (cwb-d-arc cx cy (* 0.6 arc_c) (/ +fp_pi 6.0) +fp_hpi))
			:stroke 0xff000000 :width 0.8 :join :miter))
		(push marks (cwb-text-mid "30" (- bx (* 0.78 arc_b)) (- by (* 0.11 arc_b)) :font_size 12))
		(push marks (cwb-text-mid "60" (+ cx (* 0.2 arc_c)) (+ cy (* 0.82 arc_c)) :font_size 12))
		(defq n 1 at (* 10.0 +tool_mm))
		(while (< at (- w 60.0))
			;no number where a corner has its arc and its angle
			(if (< at (* 0.72 w)) (push marks (cwb-text-mid (str n) (+ ax at) (- ay 22.0) :font_size 12)))
			(if (< at (* 0.72 h)) (push marks (cwb-text-mid (str n) (+ ax 22.0) (- ay at) :font_size 12)))
			(if (< at (- long 110.0))
				(push marks (cwb-text-mid (str n) (+ cx (* ux (+ at 40.0)) (* -22.0 uy)) (+ cy (* uy (+ at 40.0)) (* 22.0 ux)) :font_size 12)))
			(setq n (inc n) at (+ at (* 10.0 +tool_mm))))
		(def this :marks marks :readouts (list (list -14.0 17.0)))
		this)
	)
