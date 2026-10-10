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
	;what every instrument is. One of a kind sets :length and its limits,
	;and has a :rebuild that sets what it is made of:
	;	:outline	a d, closed
	;	:edges		((kind ...) ...), (:line x0 y0 x1 y1) or (:arc cx cy r a0 a1)
	;	:parts		((what d) ...), what is :move :turn :size or :close,
	;				the first that a point is in is the one, so put the
	;				small ones first. A :turn can have the point it turns
	;				about after its d, x y, default the middle
	;	:marks		shapes, drawn over the body
	;	:glyphs		((what x y [angle]) ...), where the sign of a part is
	;				drawn, so that it is seen what the part is for. A
	;				part other than :move is drawn too, a shade darker
	(def this :board board :origin (list (n2f x) (n2f y)) :angle 0.0
		:length 400.0 :min_length 150.0 :max_length 2000.0
		:outline "" :edges (list) :parts (list) :marks (list) :glyphs (list)
		:mode :line :body :nil :regions :nil :zones :nil :grab :nil)

	(defmethod :rebuild ()
		; (. instrument :rebuild) -> instrument
		this)

	(defmethod :built ()
		; (. instrument :built) -> instrument
		;its length or what it is has changed, it is made again
		(. this :rebuild)
		(def this :body (cwb-shape (get :outline this) :fill 0x70808080 :stroke 0xff000000 :width 1.0 :join :miter)
			:regions (map (lambda ((what d &rest pivot)) (list what (cwb-shape d :fill 0xff000000 :stroke 0) pivot))
				(get :parts this))
			;what is seen of the parts: each that is not the one to move it
			;by, a shade darker with a line round it, and the signs
			:zones (cat
				(map (lambda ((what d &ignore)) (cwb-shape d :fill 0x30000000 :stroke 0x80000000 :width 0.75 :join :miter))
					(filter (# (not (find (first %0) '(:move :close)))) (get :parts this)))
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
		(cwb-mat-mul (cwb-mat-move ox oy) (cwb-mat-turn (get :angle this))))

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
		(defq band (/ +tool_band (get :zoom (get :board this))) best :nil best_d (+ band 0.001))
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
				(defq turn (get :angle this) mode (get :mode this)
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
			state (list :none region :nil x y (cat (get :origin this)) (get :angle this) (get :length this)
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
					(defq pivot (third region) m0 (cwb-mat-mul (cwb-mat-move (first origin) (second origin)) (cwb-mat-turn angle)))
					(bind '(bx by) (if (nempty? pivot) (cwb-mat-point m0 (first pivot) (second pivot)) origin))
					(defq turn (tool-angle-wrap (- (path-angle (- x bx) (- y by)) (path-angle (- sx bx) (- sy by))))
						snap (get :snap_angle board))
					(if (/= snap 0.0) (setq turn (- (board-snap (+ angle turn) snap) angle)))
					(def this :angle (+ angle turn)
						:origin (cwb-mat-point (cwb-mat-turn turn bx by) (first origin) (second origin)))
					(. board :touch +board_dirty_overlay)))
			(:size
				(when (and (= (. this :held) 1) (not (get :grab this)))
					;by how much further from its middle the pointer is
					(defq d0 (max 1.0 (sqrt (+ (* (- sx (first origin)) (- sx (first origin))) (* (- sy (second origin)) (- sy (second origin))))))
						d1 (sqrt (+ (* (- x (first origin)) (- x (first origin))) (* (- y (second origin)) (- y (second origin))))))
					(def this :length (max (get :min_length this) (min (get :max_length this) (* length (/ d1 d0)))))
					(. this :built)))))

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
		;two pointers on its parts, not its edges, move and turn it together,
		;by where they were when the second went down and where they are
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
						(elem-set state +tool_state_length (get :length this))) hands)))
			(:t (defq a (first hands) b (second hands)
					pa (list (elem-get a +tool_state_last_x) (elem-get a +tool_state_last_y))
					pb (list (elem-get b +tool_state_last_x) (elem-get b +tool_state_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb (cat (get :origin this)) (get :angle this)))))
				(bind '(pa0 pb0 origin angle) grab)
				;it is moved and turned, not sized, it is a thing of a length
				(defq turn (tool-angle-wrap (- (path-angle (- (first pb) (first pa)) (- (second pb) (second pa)))
						(path-angle (- (first pb0) (first pa0)) (- (second pb0) (second pa0)))))
					m0x (* 0.5 (+ (first pa0) (first pb0))) m0y (* 0.5 (+ (second pa0) (second pb0)))
					m1x (* 0.5 (+ (first pa) (first pb))) m1y (* 0.5 (+ (second pa) (second pb)))
					m (cwb-mat-mul (cwb-mat-move (- m1x m0x) (- m1y m0y)) (cwb-mat-turn turn m0x m0y)))
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
		this)
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
	;longer or shorter. The hole by each end turns it, about the 0 of its
	;marks. The rest moves it, and the ring puts it away. It is
	;marked in millimetres along its top side, longer every five, and
	;numbered in centimetres
	(defmethod :rebuild ()
		(defq half (* 0.5 (get :length this)) top (* -0.5 +ruler_height) bot (* 0.5 +ruler_height)
			inset (- half +ruler_cap) marks (list))
		(def this :outline (cwb-d-rect (neg half) top half bot)
			:edges (list (list :line (neg inset) top inset top) (list :line (neg inset) bot inset bot))
			:parts (list
				(list :close (cwb-d-ellipse 0 8 11))
				(list :size (cwb-d-rect (neg half) top (neg inset) bot))
				(list :size (cwb-d-rect inset top half bot))
				;the hole by each end, as a ruler has to hang it by
				;and both turn it about the 0 of its marks, on its top side:
				;with that on a point, lines are drawn from the point every way
				(list :turn (cwb-d-ellipse (+ (neg inset) 26.0) 12.0 15.0) (neg inset) top)
				(list :turn (cwb-d-ellipse (- inset 26.0) 12.0 15.0) (neg inset) top)
				(list :move (cwb-d-rect (neg half) top half bot)))
			:glyphs (list (list :close 0.0 8.0)
				(list :size (- (neg half) (* -0.5 +ruler_cap)) 0.0) (list :size (- half (* 0.5 +ruler_cap)) 0.0)
				(list :turn (+ (neg inset) 26.0) 12.0) (list :turn (- inset 26.0) 12.0)))
		;millimetres along the top side, from its left end, and along the bottom
		(push marks (cwb-shape (cat (tool-ticks (neg inset) top inset top 0.0 1.0 +tool_mm)
				" " (tool-ticks (neg inset) bot inset bot 0.0 -1.0 (* 5.0 +tool_mm) 4.0 4.0 8.0))
			:stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(defq n 0 at (neg inset))
		(while (< at (- inset 6.0))
			(push marks (cwb-text-mid (str n) (+ at (if (= n 0) 6.0 0.0)) (+ top 24.0) :font_size 15))
			(setq n (inc n) at (+ at (* 10.0 +tool_mm))))
		(def this :marks marks)
		this)
	)

;;;;;;;;;;;;;;;;
; the protractor
;;;;;;;;;;;;;;;;

(defclass Protractor (board x y) (Instrument board x y)
	; (Protractor board x y) -> protractor
	;half a circle, its round side and its straight side are edges. A pen
	;run round the round side draws an arc, with :mode :pie a slice, with
	;:mode :circle the whole circle. :length is across its straight side.
	;The band half way in turns it about its middle, the two squares by
	;its straight side make it bigger or smaller, the rest moves it, the
	;ring puts it away. Marked every degree, longer every five and ten,
	;numbered every ten, with a cross where its middle is
	(def this :length 360.0 :min_length 200.0 :max_length 1600.0)

	(defmethod :rebuild ()
		(defq r (* 0.5 (get :length this)) in0 (* 0.46 r) in1 (* 0.62 r) marks (list) ticks (list)
			sq0 (* 0.16 r) sq1 (* 0.38 r) sqy0 (* -0.24 r) sqy1 (* -0.08 r))
		(def this :outline (cwb-d "M" (neg r) 0 "A" r r 0 0 1 r 0 "Z")
			:edges (list (list :arc 0.0 0.0 r (neg +fp_pi) 0.0) (list :line (neg r) 0.0 r 0.0))
			:parts (list
				(list :close (cwb-d-ellipse 0 (* -0.3 r) 11))
				(list :size (cwb-d-rect sq0 sqy0 sq1 sqy1))
				(list :size (cwb-d-rect (neg sq1) sqy0 (neg sq0) sqy1))
				(list :turn (cwb-d "M" (neg in1) 0 "A" in1 in1 0 0 1 in1 0 "L" in0 0 "A" in0 in0 0 0 0 (neg in0) 0 "Z"))
				(list :move (cwb-d "M" (neg r) 0 "A" r r 0 0 1 r 0 "Z")))
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
		(push ticks "M" -10 0 "L" 10 0 "M" 0 0 "L" 0 -14 "M" 0 (neg in0) "L" 0 (- 22.0 r))
		(push marks (cwb-shape (apply (const cwb-d) ticks) :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(def this :marks marks)
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
	;marked in millimetres from the square corner, numbered in centimetres
	(def this :length 360.0 :min_length 160.0 :max_length 1600.0)

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
		(push marks (cwb-shape (cat (tool-ticks ax ay (- bx 30.0) by 0.0 -1.0 +tool_mm)
				" " (tool-ticks ax ay cx (+ cy 30.0) 1.0 0.0 +tool_mm))
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
			(push marks (cwb-text-mid (str n) (+ ax at) (- ay 22.0) :font_size 12))
			(if (< at (- h 50.0)) (push marks (cwb-text-mid (str n) (+ ax 22.0) (- ay at) :font_size 12)))
			(setq n (inc n) at (+ at (* 10.0 +tool_mm))))
		(def this :marks marks)
		this)
	)
