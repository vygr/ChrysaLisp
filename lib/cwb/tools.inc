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

;how near an edge a pen has to go down to be held to it, as the screen has
;it: the catch of an edge is as wide, across it, as the widest line the
;Whiteboard draws, 12
(defq +tool_band 6.0)

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

(defun tool-glyph (what &optional mode)
	; (tool-glyph what [mode]) -> shapes
	;the sign of a part of an instrument, about 0 0, some 20 across: an
	;arrow that goes round for :turn, one with two heads for :size, a
	;cross for :close. For :mode, in a ring, what the instrument is set
	;to draw: an arc, a slice of pie, or a circle, either of those filled
	(defq ink 0xff000000)
	(case what
		(:mode (list (cwb-shape (cwb-d-ellipse 0 0 11) :stroke ink :width 1.0 :fill 0x40ffffff)
			(case mode
				(:pie (cwb-shape "M -1 1 L 5.5 1 A 6.5 6.5 0 1 0 -1 7.5 Z" :stroke ink :width 1.5 :join :round))
				(:fpie (cwb-shape "M -1 1 L 5.5 1 A 6.5 6.5 0 1 0 -1 7.5 Z" :stroke ink :width 1.5 :fill ink :join :round))
				(:circle (cwb-shape (cwb-d-ellipse 0 0 6) :stroke ink :width 1.5))
				(:fcircle (cwb-shape (cwb-d-ellipse 0 0 6) :stroke ink :width 1.5 :fill ink))
				(:t (cwb-shape "M -6.5 3 A 6.5 6.5 0 0 1 6.5 3" :stroke ink :width 1.8)))))
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

;where an edge catches a pen is shown when one is in its catch and not down,
;and would be held to that edge if it went down: a thin dotted line round
;the edge, the board's :hover_edges. A dot and the gap after it are that long,
;and the line that wide and that colour, all as the screen has it, at any
;zoom
(defq +tool_dot 4.0 +tool_gap 4.0 +tool_dot_width 1.2 +tool_dot_ink 0xff0050ff)

(defun tool-arc-points (out cx cy r a0 a1 seg)
	; (tool-arc-points out cx cy r a0 a1 seg) -> out
	;points round part of a circle, from one angle to the other, whichever
	;way that is, no further apart than seg, pushed on a list as x y
	(defq n (max 2 (n2i (+ 0.9999 (/ (* (abs (- a1 a0)) (max r 0.0)) seg)))) i -1)
	(while (<= (++ i) n)
		(defq a (+ a0 (/ (* (- a1 a0) (n2f i)) (n2f n))))
		(push out (+ cx (* r (cos a))) (+ cy (* r (sin a)))))
	out)

(defun tool-catch (edge band)
	; (tool-catch edge band) -> (points ...)
	;the outlines of where an edge catches a pen: every point no more than
	;the band from it. Each a closed line, x y x y ...
	(defq seg (* band 0.25))
	(case (first edge)
		(:arc (bind '(kind cx cy r a0 a1) edge)
			(cond
				((>= (- a1 a0) (const (- +fp_2pi 0.001)))
					;all the way round, a ring, its inside the other way round,
					;so it is a hole in it
					(cat (list (tool-arc-points (list) cx cy (+ r band) 0.0 +fp_2pi seg))
						(if (> r band) (list (tool-arc-points (list) cx cy (- r band) +fp_2pi 0.0 seg)) (list))))
				(:t ;round the outside, about the end, back round the inside, about the start
					(defq out (tool-arc-points (list) cx cy (+ r band) a0 a1 seg))
					(tool-arc-points out (+ cx (* r (cos a1))) (+ cy (* r (sin a1))) band a1 (+ a1 +fp_pi) seg)
					(if (> r band) (tool-arc-points out cx cy (- r band) a1 a0 seg))
					(tool-arc-points out (+ cx (* r (cos a0))) (+ cy (* r (sin a0))) band (+ a0 +fp_pi) (+ a0 +fp_2pi) seg)
					(list out))))
		(:t (bind '(kind x0 y0 x1 y1) edge)
			(defq a (path-angle (- x1 x0) (- y1 y0))
				out (tool-arc-points (list) x1 y1 band (- a +fp_hpi) (+ a +fp_hpi) seg))
			(list (tool-arc-points out x0 y0 band (+ a +fp_hpi) (+ a +fp_hpi +fp_pi) seg)))))

;what is to be rubbed out along an edge: the line, arc or slice a pen
;would draw there, as wide as the eraser reaches, the polygons of all of it
;as one. Shown as the eraser is, and rubbed out as it lets go
(defun tool-rub-area (shape)
	; (tool-rub-area shape) -> (path ...)
	(bind '(fills strokes &ignore) (cwb-flat shape))
	(if (nempty? (defq all (cat fills strokes))) (poly-union all (list) 1) (list)))

(defun tool-area-d (shape)
	; (tool-area-d shape) -> d
	;the outline of that, to be drawn
	(cwb-d-paths (tool-rub-area shape) :t))

;what is kept for a pointer that is down on an instrument, a list

(enums +tool_state 0
	(enum what part edge x y origin angle length shape near turned last_angle last_x last_y))

;how many squares of the paper's grid a centimetre of an instrument's marks is
(defq +tool_grid_cm 1.0)

(defclass Instrument (board x y) (Actor)
	; (Instrument board x y) -> instrument
	;what every instrument is. One of a kind sets :length, how big it is,
	;and its limits, and has a :rebuild that sets what it is made of, at
	;that size. :size is how much bigger all of it is seen, 1.0, and is
	;not what a hand changes:
	;	:outline	a d, closed
	;	:edges		((kind ...) ...), (:line x0 y0 x1 y1) or (:arc cx cy r a0 a1)
	;	:parts		((what d) ...), what is :move :turn :size :close, or
	;				:mode, which when tapped has it draw the next of an
	;				arc, a slice and a circle,
	;				the first that a point is in is the one, so put the
	;				small ones first. A :turn can have the point it turns
	;				about after its d, x y, default the middle. A part
	;				that ends with :quiet is not drawn darker, it is a
	;				part of the thing that is plain to see already
	;	:marks		shapes, drawn over the body
	;	:glyphs		((what x y [angle]) ...), where the sign of a part is
	;				drawn, so that it is seen what the part is for. A
	;				part other than :move is drawn too, a shade darker
	;	:zero		(x y), the 0 of its marks, what is put on a line of the
	;				paper when the board's snap is on, (:snap_zero)
	;	:readouts	((x y [turn]) ...), where the angle it is turned to
	;				is written, in degrees, the way a protractor counts,
	;				up from level. One with a turn is written turned by
	;				that, and says the angle that much on: a ruler seen
	;				from its other side
	(def this :board (weak-ref board) :origin (list (n2f x) (n2f y)) :angle 0.0
		:length 400.0 :min_length 150.0 :max_length 2000.0 :readouts (list) :said :nil
		;it is the size it is on the screen, whatever the zoom the board
		;is seen at: it lies on the glass, it is not of the document
		:size (/ 1.0 (n2f (ifn (get :zoom board) 1.0)))
		:outline "" :edges (list) :parts (list) :marks (list) :glyphs (list)
		:mode :line :body :nil :regions :nil :shades :nil :grab :nil :dotted :nil)

	(defmethod :mm ()
		; (. instrument :mm) -> num
		;how far a millimetre of its marks is. The marks are of the paper:
		;a centimetre is so many squares of the document's grid, so where
		;the zoom rests a mark of the instrument can lie on a line of the
		;paper, and the next on the next. Chris: "We should make the
		;Ruler's and other markings on the instruments match up with the
		;background marks, it could be 2x the background grid, but the
		;lines should match up." An instrument is the size it is on the
		;screen whatever the zoom, and the lines of the paper, where the
		;zoom rests, are the grid apart on the screen and half of it. The
		;inches of a ruler are inches by these, and lie on no line
		(defq board (obj-ref (get :board this)) doc (and board (. board :get_doc)))
		(/ (* +tool_grid_cm (n2f (ifn (and doc (. doc :find :grid)) 32))) 10.0))

	(defmethod :rebuild ()
		; (. instrument :rebuild) -> instrument
		this)

	(defmethod :extent ()
		; (. instrument :extent) -> num
		;how big it is, the number that is made more or less when it is
		;sized: :length
		(get :length this))

	(defmethod :set_extent (extent)
		; (. instrument :set_extent extent) -> instrument
		;it is that big, or as near as it may be, and is made again at
		;that size. It is not seen bigger: the marks of a straight side are
		;as far apart as they were, and there are more of them, as a
		;longer ruler has. What goes round, the degrees of a protractor,
		;is the one thing that is spread out
		(defq length (max (get :min_length this) (min (get :max_length this) extent)))
		(unless (= length (get :length this))
			(def this :length length)
			(. this :built))
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
			:shades (cat
				(map (lambda ((what d &ignore)) (cwb-shape d :fill 0x30000000 :stroke 0x80000000 :width 0.75 :join :miter))
					(filter (# (not (or (find (first %0) '(:move :close :mode)) (eql (last %0) :quiet)))) (get :parts this)))
				(map (lambda (glyph)
					(cwb-group (tool-glyph (first glyph) (get :mode this)) :m (Mat3x2-mul (Mat3x2-translate (second glyph) (third glyph))
						(Mat3x2-rot (if (> (length glyph) 3) (elem-get glyph 3) 0.0)))))
					(get :glyphs this))))
		(def this :dotted :nil)
		(. this :zones_changed)
		(. (obj-ref (get :board this)) :touch +board_dirty_overlay)
		this)

	(defmethod :zones_key ()
		; (. instrument :zones_key) -> num
		;how big it is seen, which how wide the band along an edge is is of
		(* (get :zoom (obj-ref (get :board this))) (get :size this)))

	(defmethod :zones ()
		; (. instrument :zones) -> ((name polygons) ...)
		;each of its parts, small ones first as they are given, the band
		;along each edge where a pen is held to it, and its body
		(unless (get :body this) (. this :built))
		(defq band (/ +tool_band (. this :zones_key)))
		(cat (map (lambda ((what shape &ignore)) (list what (first (cwb-flat shape)))) (get :regions this))
			(list (list :edge (if (nempty? (defq edges (get :edges this))) (apply (const cat) (map (# (tool-catch %0 band)) edges)) (list)))
				(list :body (first (cwb-flat (get :body this)))))))

	(defmethod :catch_shape (edge)
		; (. instrument :catch_shape edge) -> :nil | shape
		;where one of its edges catches a pen, as a thin dotted line, in
		;its own space. Made again only when it, or how big it is seen, is
		;not what it was. :nil for an edge that is not one of its now
		(unless (get :body this) (. this :built))
		(defq by (* (get :zoom (obj-ref (get :board this))) (get :size this)) kept (get :dotted this))
		(unless (and kept (= (first kept) by))
			(def this :dotted (setq kept (list by (map (lambda (edge)
				(defq out (list))
				(each (# (each (# (push out %0)) (path-dash (apply (const path) %0) (/ +tool_dot by) (/ +tool_gap by) :t)))
					(tool-catch edge (/ +tool_band by)))
				(cwb-shape (cwb-d-paths out) :stroke +tool_dot_ink :width (/ +tool_dot_width by) :fill 0))
				(get :edges this))))))
		(if (defq at (some (# (if (eql %0 edge) (!))) (get :edges this))) (elem-get (second kept) at)))

	(defmethod :drawing? ()
		; (. instrument :drawing?) -> :t | :nil
		;is any pen drawing along an edge of it, or rubbing out along one
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(if (eql (elem-get state +tool_state_what) :edge) (setq found :t))))
		found)

	(defmethod :matrix ()
		; (. instrument :matrix) -> matrix
		;from its own space to the board's
		(bind '(ox oy) (get :origin this))
		(Mat3x2-mul (Mat3x2-translate ox oy)
			(Mat3x2-mul (Mat3x2-rot (get :angle this)) (Mat3x2-scale (get :size this)))))

	(defmethod :snap_zero (&optional event)
		; (. instrument :snap_zero [event]) -> instrument
		;with the snap on, of the pointer that made it, the event of its
		;press, or the board's, it is moved the little it takes to
		;have the 0 of its marks, :zero, on a line of the paper each way: a
		;ruler's 0, the middle of a protractor's straight side, the square
		;corner of a set square. Its marks are of the paper, (:mm), so with
		;the 0 on a line, and it level or upright, each centimetre is on
		;one. Chris: "yes, snap the 0 mark to the grid when snap is on"
		(unless (get :body this) (. this :built))
		(defq board (obj-ref (get :board this)) zero (def? :zero this) step (cond ((not board) 0.0) (event (first (. board :pointer_settings event))) ((get :snap board))))
		(when (and zero (/= step 0.0))
			(bind '(zx zy) (. this :to_board (first zero) (second zero)))
			(bind '(ox oy) (get :origin this))
			(def this :origin (list (+ ox (- (board-snap zx step) zx)) (+ oy (- (board-snap zy step) zy)))))
		this)

	(defmethod :to_own (x y)
		; (. instrument :to_own x y) -> (x y)
		(Mat3x2-point (Mat3x2-invert (. this :matrix)) x y))

	(defmethod :to_board (x y)
		; (. instrument :to_board x y) -> (x y)
		(Mat3x2-point (. this :matrix) x y))

	(defmethod :edge_near (x y)
		; (. instrument :edge_near x y) -> :nil | (edge px py)
		;the edge a point, in its own space, is near enough to be held to,
		;and the point of the edge that is nearest
		;the nearest, and of two as near the first, so the order they are
		;given in says which has a corner where two meet
		(defq band (/ +tool_band (* (get :zoom (obj-ref (get :board this))) (get :size this))) best :nil best_d (+ band 0.001))
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
					(defq mid (* 0.5 (+ a0 a1)) half (* 0.5 (- a1 a0)) off (vector-angle-wrap (- a mid)))
					(setq a (+ mid (max (neg half) (min half off)))))
				(list (+ cx (* r (cos a))) (+ cy (* r (sin a)))))
			(:t (bind '(kind x0 y0 x1 y1) edge)
				(most (vector-nearest-on-line-2d x y x0 y0 x1 y1)))))

	(defmethod :part_at (x y)
		; (. instrument :part_at x y) -> :nil | (what shape pivot)
		;the part a point, in its own space, is in
		(some (lambda (region) (if (cwb-shape-hit? (second region) :nil x y) region)) (get :regions this)))


	(defmethod :edge_d (state x y)
		; (. instrument :edge_d state x y) -> d
		;what a pen held to an edge has drawn, now it is at a point, in the
		;board's space. A line along a line. Along an arc, the arc from
		;where it went down, round the way it has gone, or with :mode :pie
		;a slice of that, or :circle all the way round. :fpie and :fcircle
		;are those filled, which is the shape's doing, (:begin)
		(bind '(edge sx sy) (elem-get state +tool_state_near))
		(bind '(px py) (. this :on_edge edge x y))
		(case (first edge)
			(:arc (bind '(kind cx cy r &ignore) edge)
				(defq a (path-angle (- px cx) (- py cy))
					turned (+ (elem-get state +tool_state_turned)
						(vector-angle-wrap (- a (elem-get state +tool_state_last_angle))))
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
					((or (find mode '(:circle :fcircle)) (>= (abs turned) +fp_2pi)) (cwb-d-ellipse bcx bcy r))
					((= turned 0.0) "")
					(:t (cwb-d-arc bcx bcy r a0 a1 (find mode '(:pie :fpie))))))
			(:t (bind '(bx0 by0) (. this :to_board sx sy))
				(bind '(bx1 by1) (. this :to_board px py))
				(cwb-d-line bx0 by0 bx1 by1))))

	(defmethod :begin (stage event)
		; (. instrument :begin stage event) -> state
		(defq board (obj-ref (get :board this)))
		(bind '(id kind buttons x y &ignore) event)
		(bind '(lx ly) (. this :to_own x y))
		(defq near (. this :edge_near lx ly) region (. this :part_at lx ly)
			state (list :none region :nil x y (cat (get :origin this)) (get :angle this) (. this :extent)
				:nil near 0.0 0.0 x y))
		(cond
			(near
				;held to an edge, and what it draws is that edge. Any contact,
				;a third finger of the hand that holds the ruler too. Chris:
				;"you can hold an instrument with 2 fingers and draw your third
				;or forth down the edges, and they WILL draw !" One that rubs out
				;rubs out what it would draw, as wide as it reaches, "we should
				;be able to erase along an instrument guided edge as well !",
				;"That also means arcs and pie slices". Made the same way, and
				;as it lets go it is put into the document or rubbed out of it,
				;by what its slot is, the left button a pen, the right an eraser,
				;Chris: "we just now inject a shape/stroke or erase into the
				;canvas on release depending on the mode we are in"
				(bind '(edge px py) near)
				(elem-set state +tool_state_what :edge)
				(when (eql (first edge) :arc)
					(bind '(kind cx cy &ignore) edge)
					(elem-set state +tool_state_last_angle (path-angle (- px cx) (- py cy))))
				(defq rubs (board-rubs? (get :bindings board) event (get :mode board))
					filled (and (eql (first edge) :arc) (find (get :mode this) '(:fpie :fcircle)))
					shape (if rubs
						(cwb-shape "" :stroke 0xff000000 :kind :line :join :round :cap1 :round :cap2 :round
							:width (* 2.0 (. board :reach (. (get :bindings board) :value event :width (get :width board)))))
						(. board :new_shape event :line)))
				;round an arc, a slice or a circle that is filled is all of it,
				;and no more: the fill alone, drawn or rubbed out, not a band
				;of the pen's or the eraser's width round its edge
				(if filled (cwb-set shape :fill (if rubs 0xff000000 (cwb-get shape :stroke)) :stroke 0))
				(elem-set state +tool_state_shape shape)
				;what rubs out is shown as the eraser is, its area pale with a
				;dashed line round it, and is let go of as it is rubbed out
				(when rubs
					(defq look (board-rub-look (list) (get :zoom board)))
					(elem-set state +tool_state_edge id)
					(. (get :temp board) :insert id look)))
			((. this :body_taken id kind)
				;the body is had by another device, or by two of this one:
				;this contact does nothing till it lifts. A pen on a ruler
				;fingers hold does not take it, "students can't fight over
				;them"
				)
			(region (elem-set state +tool_state_what (first region))
				;taken hold of, not drawn along, it comes to the front of the
				;instruments. By the right button alone it goes to the back
				;of them, which is how one that is under another is got at
				(. this :restack stage (/= buttons +pev_right))))
		state)

	(defmethod :body_held ()
		; (. instrument :body_held) -> num
		;how many contacts have hold of its body, to move, turn or size it.
		;One held to an edge, drawing along it, is not one: the hand that
		;holds the ruler moves it while the pen draws
		(defq n 0)
		(. (get :states this) :each (lambda (id state)
			(if (find (elem-get state +tool_state_what) '(:move :turn :size)) (++ n))))
		n)

	(defmethod :body_taken (id kind)
		; (. instrument :body_taken id kind) -> :t | :nil
		;the body, not an edge, is the first device's that went down on it.
		;Only touch has more than one contact, and only two of them work it
		;at once, (:two_hands): a third waits, and is one of the two when
		;one lifts. Is it had already by a device this contact is not of
		(defq mine (list))
		(. (get :states this) :each (lambda (oid state)
			(if (find (elem-get state +tool_state_what) '(:move :turn :size)) (push mine oid))))
		(cond
			((empty? mine) :nil)
			((not (eql kind :touch)) :t)
			((/= (ptr-device (first mine)) (ptr-device id)) :t)))

	(defmethod :restack (stage front)
		; (. instrument :restack stage front) -> instrument
		;to the front of the instruments on a stage, or to the back of
		;them. They stay in front of the board's own, the canvas, the things
		;and their handles, and behind what else is there, a palette
		(defq board (obj-ref (get :board this)) own (list (get :surface board) (get :things board) (get :handles board))
			actors (. stage :get_actors)
			tools (filter (# (and (Instrument? %0) (not (eql %0 this)))) actors)
			low (filter (# (find %0 own)) actors)
			high (filter (# (not (or (Instrument? %0) (find %0 own)))) actors))
		(def stage :actors (cat low (if front (cat tools (list this)) (cat (list this) tools)) high))
		(. (obj-ref (get :board this)) :touch +board_dirty_overlay)
		this)

	(defmethod :step (stage event state)
		; (. instrument :step stage event state)
		(defq board (obj-ref (get :board this)))
		(bind '(id kind buttons x y &ignore) event)
		(bind '(what region edge sx sy origin angle length shape near &ignore) state)
		(elem-set state +tool_state_last_x x)
		(elem-set state +tool_state_last_y y)
		(case what
			(:edge (. this :edge_follow state))
			(:move
				(when (and (= (. this :body_held) 1) (not (get :grab this)))
					(def this :origin (list (+ (first origin) (- x sx)) (+ (second origin) (- y sy))))
					;by the snap of the device that moves it, not the board's,
					;as its squares on the bars show
					(. this :snap_zero (ptr-main (ptr-event id kind buttons x y)))
					(. board :touch +board_dirty_overlay)))
			(:turn
				(when (and (= (. this :body_held) 1) (not (get :grab this)))
					;about a point of it, which stays where it is on the board
					(defq pivot (third region) m0 (Mat3x2-mul (Mat3x2-translate (first origin) (second origin))
						(Mat3x2-mul (Mat3x2-rot angle) (Mat3x2-scale (get :size this)))))
					(bind '(bx by) (if (nempty? pivot) (Mat3x2-point m0 (first pivot) (second pivot)) origin))
					(defq turn (vector-angle-wrap (- (path-angle (- x bx) (- y by)) (path-angle (- sx bx) (- sy by))))
						snap (. board :angle_step id))
					;with angles that snap it is at a step and nowhere else. If
					;not, it is free, but for the pull of the angles that matter
					(setq turn (- (if (/= snap 0.0) (board-snap (+ angle turn) snap) (tool-magnet (+ angle turn))) angle))
					(def this :angle (+ angle turn)
						:origin (Mat3x2-point (Mat3x2-rot turn bx by) (first origin) (second origin)))
					(. board :touch +board_dirty_overlay)))
			(:size
				(when (and (= (. this :body_held) 1) (not (get :grab this)) (= (length (third region)) 1))
					;a part that sizes it from one end: it is longer by as far as
					;the pointer has gone along it, that way, and its other end
					;stays where it is on the board. A ruler with its 0 on a
					;point is made longer and the 0 is still on the point
					(defq way (first (third region)) c (cos angle) s (sin angle)
						along (* way (+ (* (- x sx) c) (* (- y sy) s))))
					(. this :set_extent (+ length along))
					(defq half (* 0.5 way (- (. this :extent) length)))
					(def this :origin (list (+ (first origin) (* half c)) (+ (second origin) (* half s))))
					;the end with the 0 of its marks, with the snap on: it is
					;as long as has the 0 on a line of the paper, the lines
					;that cross it most squarely. Its other end has not moved,
					;so only how long it is can put it there
					(defq zero (def? :zero this) step (. board :snap_step id))
					(when (and zero (/= step 0.0) (> (* way (first zero)) 0.0))
						(bind '(zx zy) (. this :to_board (first zero) (second zero)))
						(defq along (if (>= (abs c) (abs s)) (/ (- (board-snap zx step) zx) c) (/ (- (board-snap zy step) zy) s)))
						(. this :set_extent (+ (. this :extent) (* way along)))
						(setq half (* 0.5 way (- (. this :extent) length)))
						(def this :origin (list (+ (first origin) (* half c)) (+ (second origin) (* half s)))))
					(. board :touch +board_dirty_overlay))
				(when (and (= (. this :body_held) 1) (not (get :grab this)) (/= (length (third region)) 1))
					;by how much further from its middle the pointer is. The 0
					;of its marks stays where it is on the board: a set square
					;with its square corner on a point is made bigger from it
					(defq d0 (max 1.0 (sqrt (+ (* (- sx (first origin)) (- sx (first origin))) (* (- sy (second origin)) (- sy (second origin))))))
						d1 (sqrt (+ (* (- x (first origin)) (- x (first origin))) (* (- y (second origin)) (- y (second origin)))))
						zero (def? :zero this) was (if zero (. this :to_board (first zero) (second zero))))
					(. this :set_extent (* length (/ d1 d0)))
					(when (and was (defq zero (def? :zero this)))
						(bind '(zx zy) (. this :to_board (first zero) (second zero)))
						(bind '(ox oy) (get :origin this))
						(def this :origin (list (+ ox (- (first was) zx)) (+ oy (- (second was) zy)))))))))

	(defmethod :end (stage event state)
		; (. instrument :end stage event state)
		(defq board (obj-ref (get :board this)))
		(bind '(id kind buttons x y &ignore) event)
		(bind '(what region edge sx sy origin angle length shape &ignore) state)
		(case what
			(:edge
				;into the document, or out of it, as one step if anything was
				(. board :drop_temp id)
				(cond
					((not edge)
						(if (third (cwb-flat shape)) (. board :commit shape (. board :user_of event))))
					((nempty? (defq swept (tool-rub-area shape)))
						(. board :snapshot)
						(unless (. board :bite_area swept (. board :user_of event)) (. board :forget)))))
			(:close
				;put away, if it came up where it went down
				(bind '(lx ly) (. this :to_own x y))
				(when (and (defq now (. this :part_at lx ly)) (eql (first now) :close))
					(. stage :sub this)
					(. board :touch +board_dirty_overlay)))
			(:mode
				;what it draws is the next of what it can, its :modes, if it
				;came up where it went down. A half protractor has no circle
				;to draw, a whole one has. Its sign is of that, so it is made again
				(bind '(lx ly) (. this :to_own x y))
				(when (and (defq now (. this :part_at lx ly)) (eql (first now) :mode))
					(defq modes (ifn (def? :modes this) '(:line :pie :fpie))
						at (ifn (find (get :mode this) modes) -1))
					(def this :mode (elem-get modes (% (inc at) (length modes))))
					(. this :built)))))

	(defmethod :edge_follow (state)
		; (. instrument :edge_follow state) -> instrument
		;the line, arc or slice of a contact held to an edge, from where it
		;went down to where it is, and the look of what it would rub out.
		;Both ends are of the edge, in its own space, so where the
		;instrument is now is where they are: moved, turned or sized while a
		;pen draws along it, what is drawn goes with it, and is put into
		;the document only as the pen lifts, where it then is. Chris: "the
		;partial drawn instrument shape, or line or arc should stay with the
		;instrument until i remove the pen, it is only then that the
		;shape/line/arc is injected into the document, at the final place it
		;was released"
		(bind '(lx ly) (. this :to_own (elem-get state +tool_state_last_x) (elem-get state +tool_state_last_y)))
		(cwb-set (elem-get state +tool_state_shape) :d (. this :edge_d state lx ly))
		;what it would rub out is shown again, by the contact's id it is kept
		;under in the board's temp
		(when (defq id (elem-get state +tool_state_edge))
			(defq board (obj-ref (get :board this)))
			(. (get :temp board) :insert id
				(board-rub-look (tool-rub-area (elem-get state +tool_state_shape)) (get :zoom board))))
		(. (obj-ref (get :board this)) :touch +board_dirty_overlay)
		this)

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
		;what is drawn along an edge goes with the instrument as it moves
		(. (get :states this) :each (lambda (id state)
			(if (eql (elem-get state +tool_state_what) :edge) (. this :edge_follow state))))
		this)

	(defmethod :two_hands ()
		; (. instrument :two_hands)
		;two pointers on its parts, not its edges, whichever parts they are,
		;move it, turn it and size it together, by where they were when the
		;second went down and where they are.
		;
		;The first two that came are the two. A third that comes down on it
		;while they have it does nothing, they have it. When one of the two
		;lifts, those that are left carry on from where things are, the
		;next two of them together
		(defq hands (list) grab (get :grab this) pair :nil)
		(. (get :states this) :each (lambda (id state)
			(if (find (elem-get state +tool_state_what) '(:move :turn :size)) (push hands (list id state)))))
		;in the order they came, a contact that lands later has the bigger
		;number. What is kept of them comes in no order, and the two must be
		;the same way round each time: the other way round they are a half
		;turn from where they began
		(sort hands (# (- (first %0) (first %1))))
		(when grab
			(setq pair (filter (# (find (first %0) (elem-get grab 5))) hands))
			(when (< (length pair) 2)
				(def this :grab (setq grab :nil pair :nil))
				;each that is left carries on from where things are
				(each (lambda ((id state))
					(elem-set state +tool_state_x (elem-get state +tool_state_last_x))
					(elem-set state +tool_state_y (elem-get state +tool_state_last_y))
					(elem-set state +tool_state_origin (cat (get :origin this)))
					(elem-set state +tool_state_angle (get :angle this))
					(elem-set state +tool_state_length (. this :extent))) hands)))
		;the two that came first
		(and (not pair) (>= (length hands) 2) (setq pair (slice hands 0 2)))
		(cond
			((not pair))
			(:t (defq a (second (first pair)) b (second (second pair))
					pa (list (elem-get a +tool_state_last_x) (elem-get a +tool_state_last_y))
					pb (list (elem-get b +tool_state_last_x) (elem-get b +tool_state_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb (cat (get :origin this)) (get :angle this) (. this :extent)
						(list (first (first pair)) (first (second pair)))))))
				(bind '(pa0 pb0 origin angle length &ignore) grab)
				;it is as much bigger as they are further apart, within what it can be
				(defq d0 (max 1.0 (sqrt (+ (* (- (first pb0) (first pa0)) (- (first pb0) (first pa0)))
						(* (- (second pb0) (second pa0)) (- (second pb0) (second pa0))))))
					d1 (sqrt (+ (* (- (first pb) (first pa)) (- (first pb) (first pa)))
						(* (- (second pb) (second pa)) (- (second pb) (second pa)))))
					by (/ (. (. this :set_extent (* length (/ d1 d0))) :extent) length))
				(defq turn (vector-angle-wrap (- (path-angle (- (first pb) (first pa)) (- (second pb) (second pa)))
						(path-angle (- (first pb0) (first pa0)) (- (second pb0) (second pa0)))))
					;pulled to the angles that matter, as it is by one pointer
					turn (- (tool-magnet (+ angle turn)) angle)
					m0x (* 0.5 (+ (first pa0) (first pb0))) m0y (* 0.5 (+ (second pa0) (second pb0)))
					m1x (* 0.5 (+ (first pa) (first pb))) m1y (* 0.5 (+ (second pa) (second pb)))
					m (Mat3x2-mul (Mat3x2-translate (- m1x m0x) (- m1y m0y))
						(Mat3x2-mul (Mat3x2-rot turn m0x m0y) (Mat3x2-scale by by m0x m0y))))
				(def this :angle (+ angle turn) :origin (Mat3x2-point m (first origin) (second origin)))
				(. (obj-ref (get :board this)) :touch +board_dirty_overlay))))

	(defmethod :draw (canvas &optional m)
		; (. instrument :draw canvas [m]) -> instrument
		;on a canvas, by the matrix the board is seen by
		(unless (get :body this) (. this :built))
		(defq m (Mat3x2-mul m (. this :matrix)))
		(cwb-draw-items canvas (list (get :body this)) m)
		(cwb-draw-items canvas (get :shades this) m)
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
				(cwb-place! shape (place-move (place-turn (elem-get shape +cwb_place) (if (> (length at) 2) (third at) 0.0) 0.0 0.0)
					(first at) (second at)))
				shape) (get :readouts this) says)))))
		(second said))
	)

;;;;;;;;;;;
; the ruler
;;;;;;;;;;;

;a ruler is that high, and each end of it is that wide
(defq +ruler_height 80.0 +ruler_cap 26.0)

(defclass Ruler (board x y) (Instrument board x y)
	; (Ruler board x y) -> ruler
	;a straight edge, its two long sides are edges. Each end makes it
	;longer or shorter, a longer ruler with more marks, no wider. The
	;strip of marks along each side turns it, about the 0 of that side,
	;and so does the hole by each end, about the 0 at the other end. The
	;middle moves it, and the ring puts it away. Its top side is
	;centimetres from the left, its bottom side inches from the right
	(def this :min_length 200.0 :max_length 3000.0)

	(defmethod :rebuild ()
		(defq half (* 0.5 (get :length this)) top (* -0.5 +ruler_height) bot (* 0.5 +ruler_height)
			inset (- half +ruler_cap) marks (list))
		(def this :outline (cwb-d-rect (neg half) top half bot)
			:edges (list (list :line (neg inset) top inset top) (list :line (neg inset) bot inset bot))
			:parts (list
				(list :close (cwb-d-ellipse 0 0 11))
				;each end makes it longer or shorter that way, the other end
				;stays where it is
				(list :size (cwb-d-rect (neg half) top (neg inset) bot) -1.0)
				(list :size (cwb-d-rect inset top half bot) 1.0)
				;the hole by each end, as a ruler has to hang it by. Each
				;turns it about the 0 of the marks at the other end: the top
				;side's are from the left, the bottom side's from the right.
				;With a 0 on a point, lines are drawn from the point every way
				(list :turn (cwb-d-ellipse (+ (neg inset) 24.0) 0.0 12.0) inset bot)
				(list :turn (cwb-d-ellipse (- inset 24.0) 0.0 12.0) (neg inset) top)
				;from the outside in: a pen close on a side is held to it and
				;draws, that is looked for before any part. Then the strip
				;along each side, its marks and its numbers, turns it, about
				;the 0 of that side's marks. Then the middle moves it
				(list :turn (cwb-d-rect (neg inset) top inset (+ top 26.0)) (neg inset) top :quiet)
				(list :turn (cwb-d-rect (neg inset) (- bot 26.0) inset bot) inset bot :quiet)
				(list :move (cwb-d-rect (neg half) top half bot)))
			:glyphs (list (list :close 0.0 0.0)
				(list :size (- (neg half) (* -0.5 +ruler_cap)) 0.0) (list :size (- half (* 0.5 +ruler_cap)) 0.0)
				(list :turn (+ (neg inset) 24.0) 0.0) (list :turn (- inset 24.0) 0.0)))
		;centimetres along the top side, in millimetres, from its left end.
		;Inches along the bottom side, in sixteenths, from its right end,
		;and written the other way up: the bottom is the top turned half
		;round about the middle, to be read from the other side
		(defq mm (. this :mm) inch (* 25.4 mm) ticks (list (tool-ticks (neg inset) top inset top 0.0 1.0 mm))
			n 0 at 0.0 flip (lambda (shape x y)
				;words, about their own middle, turned half round, and put at a point
				(cwb-place! shape (place-move (place-turn (elem-get shape +cwb_place) +fp_pi 0.0 0.0) x y))
				shape))
		(while (<= at (+ (* 2.0 inset) 0.01))
			(defq tick (cond ((= (% n 16) 0) 14.0) ((= (% n 8) 0) 11.0) ((= (% n 4) 0) 8.0) ((= (% n 2) 0) 6.0) (:t 4.0)))
			(push ticks (cwb-d "M" (- inset at) bot "L" (- inset at) (- bot tick)))
			(and (= (% n 16) 0) (< at (- (* 2.0 inset) 6.0))
				(push marks (flip (cwb-text-mid (str (/ n 16)) 0 0 :font_size 13) (- inset at (if (= n 0) 6.0 0.0)) (- bot 22.0))))
			(setq n (inc n) at (/ (* (n2f n) inch) 16.0)))
		(push marks (cwb-shape (join ticks " ") :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(setq n 0 at 0.0)
		(while (< at (- (* 2.0 inset) 6.0))
			(push marks (cwb-text-mid (str n) (+ (neg inset) at (if (= n 0) 6.0 0.0)) (+ top 22.0) :font_size 13))
			(setq n (inc n) at (+ at (* 10.0 mm))))
		;what each side is in, by its 0
		;under the numbers, past the hole by the end: the 1 is a square of
		;the paper along from the 0, where it was
		(push marks (cwb-text-mid "cm" (+ (neg inset) 50.0) (+ top 35.0) :font_size 10))
		(push marks (flip (cwb-text-mid "in" 0 0 :font_size 10) (- inset 24.0) (- bot 23.0)))
		;the angle it is at, either side of its middle, the one for who
		;reads the top side and the one for who reads the bottom
		;The one that is the right way up is on the left, where reading starts
		(def this :readouts (list (list -44.0 0.0) (list 44.0 0.0 +fp_pi)))
		(def this :marks marks :zero (list (neg inset) top))
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
	(def this :length 360.0 :min_length 200.0 :max_length 1600.0)

	(defmethod :rebuild ()
		;what goes round is as much bigger as the protractor is, by, the
		;marks of the degrees and their numbers. The marks of its straight
		;side are not
		(defq r (* 0.5 (get :length this)) by (/ r 180.0) in0 (* 0.46 r) in1 (* 0.62 r) marks (list) ticks (list)
			sq0 (* 0.16 r) sq1 (* 0.38 r) sqy0 (* -0.24 r) sqy1 (* -0.08 r)
			;the strip below the line through its middle, as a protractor
			;has, its straight side is the bottom of that
			;the round side goes on round, past the line through the middle,
			;to meet the straight side: it has the one straight side and the
			;one round one. A tenth of the radius down, the circle is that
			;much less than the radius across, and that much angle further
			base (* 0.1 r) across (* 0.99499 r)
			whole (cwb-d "M" (neg across) base "A" r r 0 0 1 0 (neg r) "A" r r 0 0 1 across base "Z"))
		(def this :outline whole
			;the round side is an edge from its 0 to its 180 and no further:
			;below the line through the middle it is only the shape of the thing
			:edges (list (list :arc 0.0 0.0 r (neg +fp_pi) 0.0) (list :line (neg across) base across base))
			:parts (list
				(list :close (cwb-d-ellipse 0 (* -0.27 r) 11))
				(list :mode (cwb-d-ellipse 0 (* -0.115 r) 12))
				(list :size (cwb-d-rect sq0 sqy0 sq1 sqy1))
				(list :size (cwb-d-rect (neg sq1) sqy0 (neg sq0) sqy1))
				(list :turn (cwb-d "M" (neg in1) 0 "A" in1 in1 0 0 1 in1 0 "L" in0 0 "A" in0 in0 0 0 0 (neg in0) 0 "Z"))
				;the outer scale, all of it outside the band that turns it,
				;sizes it too, as the squares do. A pen close on the round
				;side is held to it and draws, that comes first
				(list :size (cwb-d "M" (neg r) 0 "A" r r 0 0 1 r 0 "L" in1 0 "A" in1 in1 0 0 0 (neg in1) 0 "Z") :quiet)
				(list :move whole))
			:glyphs (list (list :close 0.0 (* -0.27 r)) (list :mode 0.0 (* -0.115 r))
				(list :size (* 0.5 (+ sq0 sq1)) (* 0.5 (+ sqy0 sqy1))) (list :size (* -0.5 (+ sq0 sq1)) (* 0.5 (+ sqy0 sqy1)))
				(list :turn (* 0.54 r 0.7071) (* -0.54 r 0.7071) (* 0.25 +fp_pi))
				(list :turn (* -0.54 r 0.7071) (* -0.54 r 0.7071) (* -0.25 +fp_pi))))
		;a mark every degree round from the right, longer every five and every ten
		(each (lambda (deg)
			(defq a (neg (* (n2f deg) (const (/ +fp_pi 180.0)))) c (cos a) s (sin a)
				tick (* by (cond ((= (% deg 10) 0) 15.0) ((= (% deg 5) 0) 10.0) (:t 6.0))))
			(push ticks "M" (* r c) (* r s) "L" (* (- r tick) c) (* (- r tick) s))
			;numbered every ten both ways, as a protractor is: from the right
			;round the outside, and under those, smaller, from the left
			(and (= (% deg 10) 0) (> deg 0) (< deg 180)
				(push marks (cwb-text-mid (str deg) (* (- r (* by 27.0)) c) (* (- r (* by 27.0)) s) :font_size (n2i (* by 12.0)))
					(cwb-text-mid (str (- 180 deg)) (* (- r (* by 44.0)) c) (* (- r (* by 44.0)) s) :font_size (n2i (* by 9.0))))))
			(range 0 181))
		;the band that turns it is the inner scale, as a protractor has, that
		;reads the other way round, marked every five and numbered every thirty
		(each (lambda (deg)
			(defq a (neg (* (n2f deg) (const (/ +fp_pi 180.0)))) c (cos a) s (sin a)
				tick (* by (if (= (% deg 10) 0) 8.0 4.0)))
			(push ticks "M" (* in1 c) (* in1 s) "L" (* (- in1 tick) c) (* (- in1 tick) s))
			(and (= (% deg 30) 0) (> deg 0) (< deg 180)
				(push marks (cwb-text-mid (str (- 180 deg)) (* (- in1 (* by 17.0)) c) (* (- in1 (* by 17.0)) s) :font_size (n2i (* by 10.0))))))
			(range 0 181 5))
		;its middle, on the straight side, and the line straight up from it
		;and the line through it from 0 to 180, with the strip below marked
		;in millimetres along the straight side
		(push ticks "M" (neg r) 0 "L" r 0 "M" 0 base "L" 0 -14 "M" 0 (neg in0) "L" 0 (- (* by 53.0) r))
		;out from the middle both ways, so the middle is on a long mark and
		;stays on it as the protractor is made bigger or smaller
		(defq mm (. this :mm))
		(push marks (cwb-shape (cat (tool-ticks 0.0 base across base 0.0 -1.0 mm 3.0 5.0 8.0)
				" " (tool-ticks 0.0 base (neg across) base 0.0 -1.0 mm 3.0 5.0 8.0))
			:stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(push marks (cwb-shape (apply (const cwb-d) ticks) :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(def this :marks marks :readouts (list (list 0.0 (* -0.4 r))) :zero (list 0.0 0.0))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;
; the whole protractor
;;;;;;;;;;;;;;;;;;;;;;

(defclass Circle (board x y) (Protractor board x y)
	; (Circle board x y) -> circle
	;a protractor that is the whole circle, with no straight side. Its
	;rim is its one edge, all the way round, and draws as a protractor's
	;round side does, an arc, a slice or the circle. It is numbered every
	;ten degrees round from the right, the way the angle is counted, and
	;on its inner scale the other way round. The inner scale turns it, the
	;outer and the two squares size it, the middle moves it
	(defmethod :rebuild ()
		(defq r (* 0.5 (get :length this)) by (/ r 180.0) in0 (* 0.46 r) in1 (* 0.62 r) marks (list) ticks (list)
			sq0 (* 0.16 r) sq1 (* 0.38 r) sqy (* 0.08 r)
			;a ring is its outer round one way and its inner the other
			ring (lambda (r0 r1) (cwb-d "M" r1 0 "A" r1 r1 0 1 1 (neg r1) 0 "A" r1 r1 0 1 1 r1 0 "Z"
				"M" r0 0 "A" r0 r0 0 1 0 (neg r0) 0 "A" r0 r0 0 1 0 r0 0 "Z")))
		(def this :modes '(:line :pie :fpie :circle :fcircle) :outline (cwb-d-ellipse 0 0 r)
			:edges (list (list :arc 0.0 0.0 r (neg +fp_pi) +fp_pi))
			:parts (list
				(list :close (cwb-d-ellipse 0 (* -0.3 r) 11))
				(list :mode (cwb-d-ellipse 0 (* 0.3 r) 12))
				(list :size (cwb-d-rect sq0 (neg sqy) sq1 sqy))
				(list :size (cwb-d-rect (neg sq1) (neg sqy) (neg sq0) sqy))
				(list :move (cwb-d-ellipse 0 0 in0))
				(list :turn (ring in0 in1))
				(list :size (cwb-d-ellipse 0 0 r) :quiet))
			:glyphs (list (list :close 0.0 (* -0.3 r)) (list :mode 0.0 (* 0.3 r))
				(list :size (* 0.5 (+ sq0 sq1)) 0.0) (list :size (* -0.5 (+ sq0 sq1)) 0.0)
				(list :turn (* 0.54 r 0.7071) (* -0.54 r 0.7071) (* 0.25 +fp_pi))
				(list :turn (* -0.54 r 0.7071) (* 0.54 r 0.7071) (* 1.25 +fp_pi))))
		(each (lambda (deg)
			(defq a (neg (* (n2f deg) (const (/ +fp_pi 180.0)))) c (cos a) s (sin a)
				tick (* by (cond ((= (% deg 10) 0) 15.0) ((= (% deg 5) 0) 10.0) (:t 6.0))))
			(push ticks "M" (* r c) (* r s) "L" (* (- r tick) c) (* (- r tick) s))
			(if (= (% deg 10) 0)
				(push marks (cwb-text-mid (str deg) (* (- r (* by 27.0)) c) (* (- r (* by 27.0)) s) :font_size (n2i (* by 12.0)))
					(cwb-text-mid (str (% (- 360 deg) 360)) (* (- r (* by 44.0)) c) (* (- r (* by 44.0)) s) :font_size (n2i (* by 9.0)))))
			(when (= (% deg 5) 0)
				(defq tick (* by (if (= (% deg 10) 0) 8.0 4.0)))
				(push ticks "M" (* in1 c) (* in1 s) "L" (* (- in1 tick) c) (* (- in1 tick) s))
				(if (= (% deg 30) 0)
					(push marks (cwb-text-mid (str (% (- 360 deg) 360)) (* (- in1 (* by 17.0)) c) (* (- in1 (* by 17.0)) s)
						:font_size (n2i (* by 10.0)))))))
			(range 0 360))
		;its middle, and the four lines out from the inner scale
		(push ticks "M" -12 0 "L" 12 0 "M" 0 -12 "L" 0 12)
		(each (lambda ((c s)) (push ticks "M" (* in1 c) (* in1 s) "L" (* (- r (* by 53.0)) c) (* (- r (* by 53.0)) s)))
			'((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0)))
		(push marks (cwb-shape (apply (const cwb-d) ticks) :stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		(def this :marks marks :readouts (list (list 0.0 (* 0.16 r))) :zero (list 0.0 0.0))
		this)
	)

;;;;;;;;;;;;;;;;
; the set square
;;;;;;;;;;;;;;;;

(defclass Setsquare (board x y) (Instrument board x y)
	; (Setsquare board x y) -> setsquare
	;a triangle with a square corner, each of its three sides is an edge.
	;:length is its longest square side, the other is that times :ratio,
	;one over the root of 3, so its corners are 30, 60 and 90 degrees.
	;(Setsquare45) has a ratio of 1, and two corners of 45. The two sharp corners
	;turn it about its square corner, the square by its square corner makes
	;it bigger or smaller, and so does the strip along each side, where its
	;numbers are, and the square corner stays where it is as it does. Its middle moves it. Its two square sides are
	;marked in millimetres from the square corner, and its long side from
	;a little below its top corner, numbered in centimetres
	(def this :length 360.0 :min_length 200.0 :max_length 2000.0 :ratio 0.57735)

	(defmethod :rebuild ()
		(defq w (get :length this) h (* w (get :ratio this))
			;the angle of the corner at the end of the long square side, and
			;of the other sharp corner, and the first in degrees
			angle_b (path-angle w h) angle_c (- +fp_hpi angle_b)
			deg_b (n2i (+ (/ (* angle_b 180.0) +fp_pi) 0.5))
			;the square corner is bottom left, its middle is the middle of the three
			ax (* w -0.33333) ay (* h 0.33333) bx (* w 0.66667) by ay cx ax cy (* h -0.66667)
			;a corner, cut off a quarter of the way along its two sides
			corner (lambda (px py qx qy rx ry)
				(cwb-d "M" px py "L" (+ px (* 0.25 (- qx px))) (+ py (* 0.25 (- qy py)))
					(+ px (* 0.25 (- rx px))) (+ py (* 0.25 (- ry py))) "Z"))
			marks (list)
			;how far the point as far from all three sides is from them, and
			;where it is, and how much smaller the middle is
			long (sqrt (+ (* w w) (* h h))) ux (/ w long) uy (/ h long)
			ir (* 0.5 (- (+ w h) long)) ix (+ ax ir) iy (- ay ir)
			ik (/ (max 1.0 (- ir 26.0)) ir))
		(def this :outline (cwb-d "M" ax ay "L" bx by cx cy "Z")
			;a side is an edge from the 0 of its marks. The two square sides
			;have theirs at the square corner. The long side has its 0 a
			;little in from the top corner, and its marks end as far short of
			;the other
			:edges (list (list :line ax ay bx by)
				(list :line (- bx (* 40.0 ux)) (- by (* 40.0 uy)) (+ cx (* 40.0 ux)) (+ cy (* 40.0 uy)))
				(list :line cx cy ax ay))
			:parts (list
				(list :close (cwb-d-ellipse 0 0 11))
				(list :size (cwb-d-rect (+ ax 28.0) (- ay 62.0) (+ ax 62.0) (- ay 28.0)))
				;each sharp corner turns it about the square corner, the 0 of
				;the marks of its two square sides, which stays where it is.
				;Chris: "The snap corner and the rotation cneter of the
				;SetSquers should be the 0 scale corner."
				(list :turn (corner bx by ax ay cx cy) ax ay)
				(list :turn (corner cx cy ax ay bx by) ax ay)
				;from a side in, as a ruler: a pen close on the side draws.
				;Then the strip along each side, where its marks and numbers
				;are, makes it bigger or smaller. Then its middle moves it.
				;The middle is the same shape, 26 in from each side, about
				;the point that is as far from all three
				(list :move (cwb-d "M" (+ ix (* ik (- ax ix))) (+ iy (* ik (- ay iy)))
					"L" (+ ix (* ik (- bx ix))) (+ iy (* ik (- by iy))) (+ ix (* ik (- cx ix))) (+ iy (* ik (- cy iy))) "Z"))
				(list :size (cwb-d "M" ax ay "L" bx by cx cy "Z") :quiet))
			:glyphs (list (list :close 0.0 0.0)
				(list :size (+ ax 45.0) (- ay 45.0) (* -0.25 +fp_pi))
				(list :turn (+ bx (* 0.11 (- ax bx)) (* 0.05 (- cx bx))) (+ by (* 0.05 (- cy by)) -4.0))
				(list :turn (+ cx (* 0.05 (- bx cx)) 6.0) (+ cy (* 0.12 (- ay cy)) (* 0.05 (- by cy))))))
		;millimetres along the two square sides, from the square corner
		;and along the long side, the marks pointing in, from a little way
		;down from its top corner to a little short of the other, as a
		;ruler's are in from its ends: at a sharp corner a mark would go
		;out through the other side
		(defq mm (. this :mm))
		(push marks (cwb-shape (cat (tool-ticks ax ay (- bx 30.0) by 0.0 -1.0 mm)
				" " (tool-ticks ax ay cx (+ cy 30.0) 1.0 0.0 mm)
				" " (tool-ticks (+ cx (* 40.0 ux)) (+ cy (* 40.0 uy)) (- bx (* 40.0 ux)) (- by (* 40.0 uy)) (neg uy) ux mm))
			:stroke 0xff000000 :width 0.8 :cap1 :butt :cap2 :butt))
		;what a set square has: the hole in its middle, the same shape, the
		;arc of each sharp corner with its angle, and the mark of a square corner
		(defq arc_b (* 0.25 w) arc_c (* 0.25 h))
		(push marks (cwb-shape (cat
				(cwb-d "M" (* 0.38 ax) (* 0.38 ay) "L" (* 0.38 bx) (* 0.38 by) (* 0.38 cx) (* 0.38 cy) "Z")
				" " (cwb-d-arc bx by (* 0.6 arc_b) +fp_pi (+ +fp_pi angle_b))
				" " (cwb-d-arc cx cy (* 0.6 arc_c) angle_b +fp_hpi))
			:stroke 0xff000000 :width 0.8 :join :miter))
		;the angle of each, half way round its arc and out past it, clear of the sign that turns
		(defq mid_b (+ +fp_pi (* 0.5 angle_b)) mid_c (+ angle_b (* 0.5 angle_c)))
		(push marks (cwb-text-mid (str deg_b) (+ bx (* 1.0 arc_b (cos mid_b))) (+ by (* 1.0 arc_b (sin mid_b))) :font_size 12))
		(push marks (cwb-text-mid (str (- 90 deg_b)) (+ cx (* 1.4 arc_c (cos mid_c))) (+ cy (* 1.4 arc_c (sin mid_c))) :font_size 12))
		(defq n 1 at (* 10.0 mm))
		(while (< at (- w 60.0))
			;no number where a corner has its arc and its angle
			(if (< at (* 0.72 w)) (push marks (cwb-text-mid (str n) (+ ax at) (- ay 22.0) :font_size 12)))
			(if (< at (* 0.72 h)) (push marks (cwb-text-mid (str n) (+ ax 22.0) (- ay at) :font_size 12)))
			(if (< at (- long 110.0))
				(push marks (cwb-text-mid (str n) (+ cx (* ux (+ at 40.0)) (* -22.0 uy)) (+ cy (* uy (+ at 40.0)) (* 22.0 ux)) :font_size 12)))
			(setq n (inc n) at (+ at (* 10.0 mm))))
		(def this :marks marks :readouts (list (list -14.0 17.0)) :zero (list ax ay))
		this)
	)

(defclass Setsquare45 (board x y) (Setsquare board x y)
	; (Setsquare45 board x y) -> setsquare45
	;the other set square of a pair: its two square sides are the same,
	;and its corners are 45, 45 and 90 degrees
	(def this :length 300.0 :ratio 1.0))
