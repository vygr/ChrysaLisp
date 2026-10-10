;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/board.inc - a whiteboard that is worked on
;
; A board is a document, lib/cwb/doc.inc, with what it takes to change
; it: a stage that pointers go to, lib/cwb/pointer.inc, the bindings that
; say what each pointer is, what is selected, and the steps that can be
; undone.
;
; The stage has, from the back: the surface, which every point is on, and
; where a pointer draws, rubs out, or picks things up and moves them; the
; handles of what is selected, to size and turn it; and whatever else is
; put there, a ruler say, lib/cwb/tools.inc.
;
; Nothing here needs a screen. An app gives it pointers and asks it to
; draw, and so can a test or a script: (. board :pointers events) is the
; whole of how it is driven by hand, and every change a hand can make is
; a method that can be called without one.
;
; Every point is in the space of the document. Sizes that are of the
; screen, how near is near enough, how big a handle is, are divided by
; :zoom, which the app sets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./doc.inc")
(import "./pointer.inc")

;what has changed since it was last drawn
;the document, all of it. What is over it. And only that items were put on
;top of it, :appended, which can be drawn on what is there
(bits +board_dirty 0
	(bit doc overlay append))

(defq +board_handle 5.0 +board_near 4.0 +board_turn_gap 24.0)

;;;;;;;;;;;;;;;;;;
; a few small parts
;;;;;;;;;;;;;;;;;;

(defun board-copy-items (items)
	; (board-copy-items items) -> items
	;items that can be changed and leave those they were copied from as
	;they were. What a shape is flattened to goes with it, it is the same
	(defq root (list) todo (list (list items root)))
	(while (defq next (pop todo))
		(bind '(from into) next)
		(each (lambda (item)
			(defq copy (cat item))
			(push into copy)
			(when (cwb-group? item)
				(defq kids (list))
				(elem-set copy +cwb_items kids)
				(push todo (list (elem-get item +cwb_items) kids))))
			from))
	root)

(defun board-snap (v step)
	; (board-snap v step) -> v
	;to the nearest step, as it is if the step is 0
	(if (= step 0.0) v
		(* (floor (+ (/ v step) 0.5)) step)))

(defun board-pen-d (points)
	; (board-pen-d points) -> d
	;the path of a line drawn by hand, points is x y x y ... It goes
	;from the first to the last, and between is curved: each point is
	;where the line turns toward, and it passes half way between each two
	(defq pts (partition (map (const n2f) points) 2) n (length pts))
	(cond
		((= n 0) "")
		((= n 1) (cwb-d "M" (first (first pts)) (second (first pts)) "L" (first (first pts)) (second (first pts))))
		((= n 2) (apply (const cwb-d-line) (cat (first pts) (second pts))))
		(:t (defq out (list "M" (first (first pts)) (second (first pts))))
			(each! (lambda ((x y))
				(bind '(nx ny) (elem-get pts (inc (!))))
				(push out "Q" x y (* 0.5 (+ x nx)) (* 0.5 (+ y ny))))
				(list pts) 1 (dec n))
			(push out "L" (first (last pts)) (second (last pts)))
			(apply (const cwb-d) out))))

(defun board-two-point-mat (a0 b0 a1 b1)
	; (board-two-point-mat a0 b0 a1 b1) -> matrix
	;the move, turn and change of size that takes two points to where
	;they are now, as two fingers on a thing do. Each is (x y)
	(defq v0x (- (first b0) (first a0)) v0y (- (second b0) (second a0))
		v1x (- (first b1) (first a1)) v1y (- (second b1) (second a1))
		l0 (max 1.0 (sqrt (+ (* v0x v0x) (* v0y v0y))))
		l1 (max 1.0 (sqrt (+ (* v1x v1x) (* v1y v1y))))
		scale (/ l1 l0) angle (- (path-angle v1x v1y) (path-angle v0x v0y))
		m0x (* 0.5 (+ (first a0) (first b0))) m0y (* 0.5 (+ (second a0) (second b0)))
		m1x (* 0.5 (+ (first a1) (first b1))) m1y (* 0.5 (+ (second a1) (second b1))))
	(cwb-mat-mul (cwb-mat-move m1x m1y)
		(cwb-mat-mul (cwb-mat-turn angle)
			(cwb-mat-mul (cwb-mat-scale scale) (cwb-mat-move (neg m0x) (neg m0y))))))

;;;;;;;;;;;;;
; the surface
;;;;;;;;;;;;;

;what is kept for a pointer that is down on the surface, a list
(enums +stroke 0
	(enum tool x y points item moved last_x last_y snap box))

(defclass Surface (board) (Actor)
	; (Surface board) -> surface
	;the actor at the back, every point is on it
	(def this :board board :grab :nil)

	(defmethod :hit (x y) :t)

	(defmethod :things ()
		; (. surface :things) -> ((item matrix) ...)
		;what is selected, each with where it is now, to be moved from there
		(map (# (list %0 (elem-get %0 +cwb_m))) (. (get :board this) :selected_items)))

	(defmethod :holding ()
		; (. surface :holding) -> :nil | id
		;a pointer that has hold of what is selected
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(if (and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item))
				(setq found id))))
		found)

	(defmethod :begin (event)
		; (. surface :begin event) -> state
		;a pointer has gone down
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(defq tool (. (get :bindings board) :value event :tool :pen)
			;what was only there till the next pointer went down goes
			shut (. board :dismiss event))
		;a pen is whatever the board is set to draw, or it is bound to
		;itself, and in select mode it is the hand, so a mouse alone can
		;do everything
		(if (eql tool :pen) (setq tool (. (get :bindings board) :value event :mode (get :mode board))))
		(if (eql tool :select) (setq tool :hand))
		(defq state (list tool x y (list x y) :nil :nil x y :nil :nil)
			holding (. this :holding))
		(case tool
			(:hand
				(defq hit (cwb-hit (get :doc board) x y (/ +board_near (get :zoom board))))
				;anywhere in the box round what is selected is on it, a box
				;that is not filled, or a thin line, is hard to be on
				(if (and (not hit) (nempty? (defq items (. board :selected_items)))
						(defq box (cwb-bounds items))
						(>= x (first box)) (>= y (second box)) (<= x (third box)) (<= y (elem-get box 3)))
					(setq hit (list (first items) 0 :nil)))
				;a finger in the box of a thing is on the thing
				(if (and (not hit) (eql kind :touch)) (setq hit (cwb-hit-box (get :doc board) x y)))
				(cond
					((and hit holding (find (elem-get (first hit) +cwb_id) (get :selected board)))
						;a second hand on what the first is holding, the two
						;of them move it, see (:two_hands)
						(elem-set state +stroke_item (. this :things)))
					(holding
						;a second hand that is not on it does nothing
						(elem-set state +stroke_tool :idle))
					(hit (defq id_hit (elem-get (first hit) +cwb_id))
						(unless (find id_hit (get :selected board)) (. board :select (list id_hit)))
						(. board :snapshot)
						(elem-set state +stroke_snap :t)
						(elem-set state +stroke_item (. this :things))
						;the box round it as it is picked up, a corner of which
						;is what goes to the grid
						(elem-set state +stroke_box (cwb-bounds (. board :selected_items)))
						(. board :float (get :selected board)))
					(:t ;on nothing. With nothing selected, and nothing put
						;away by it, it may be a tap, see (:end)
						(if (and (not shut) (empty? (get :selected board)))
							(elem-set state +stroke_snap :tap))
						(. board :select (list)))))
			(:eraser (. board :snapshot))
			(:pan)
			(:text
				;the words the board has are put down where the pointer is,
				;they sit on the line through it, and that is all it does
				(elem-set state +stroke_tool :idle)
				(when (nempty? (defq words (get :text board)))
					(. board :commit (cwb-text words (. board :snap_x x) (. board :snap_y y)
						:fill (. (get :bindings board) :value event :color (get :color board))
						:font (get :font board) :font_size (get :font_size board)))))
			(:t ;something is drawn
				(elem-set state +stroke_x (. board :snap_x x))
				(elem-set state +stroke_y (. board :snap_y y))
				(elem-set state +stroke_points (list (elem-get state +stroke_x) (elem-get state +stroke_y)))
				(elem-set state +stroke_item (. board :new_shape event tool))))
		state)

	(defmethod :shape_d (state x y)
		; (. surface :shape_d state x y) -> d
		;the path of what a pointer is drawing, now it is at a point
		(bind '(tool sx sy points &ignore) state)
		(case tool
			((:line :arrow :arrow2) (cwb-d-line sx sy x y))
			((:rect :frect) (cwb-d-rect sx sy x y))
			((:ellipse :fellipse)
				(cwb-d-ellipse (* 0.5 (+ sx x)) (* 0.5 (+ sy y)) (* 0.5 (abs (- x sx))) (* 0.5 (abs (- y sy)))))
			(:t (board-pen-d points))))

	(defmethod :step (event state)
		; (. surface :step event state) -> state
		;a pointer that is down has moved, or come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved &ignore) state)
		(case tool
			(:idle)
			(:hand
				(cond
					(item ;what is selected goes with it. With snap, the top
						;left of the box round it goes to the grid, not the
						;pointer, so things line up with it and each other
						(defq mx (- x sx) my (- y sy) box (elem-get state +stroke_box))
						(when box
							(setq mx (- (. board :snap_x (+ (first box) mx)) (first box))
								my (- (. board :snap_y (+ (second box) my)) (second box))))
						(defq move (cwb-mat-move mx my))
						(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul move m0))) item)
						(if (or (/= x sx) (/= y sy)) (elem-set state +stroke_moved :t))
						(. board :touch +board_dirty_overlay))
					(:t ;a box is dragged out round things
						(def board :band (list (min sx x) (min sy y) (max sx x) (max sy y)))
						(. board :touch +board_dirty_overlay))))
			(:eraser
				;from where it was to where it is, and it is shown there
				(bind '(ox oy) points)
				(if (. board :rub_along ox oy x y) (elem-set state +stroke_moved :t))
				(elem-set state +stroke_points (list x y))
				(def board :rubber (list x y (/ (get :rub_reach board) (get :zoom board))))
				(. board :touch +board_dirty_overlay))
			(:pan
				(def board :pan (list (- x sx) (- y sy))))
			(:t ;something is drawn
				(defq x (. board :snap_x x) y (. board :snap_y y))
				(when (or (/= x (elem-get points -3)) (/= y (elem-get points -2)))
					(push points x y)
					(cwb-set item :d (. this :shape_d state x y))
					(. board :touch +board_dirty_overlay))))
		state)

	(defmethod :end (event state)
		; (. surface :end event state)
		;a pointer has come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved lx ly snap &ignore) state)
		(case tool
			(:idle)
			(:hand
				(cond
					(item (if (and snap (not moved)) (. board :forget))
						;put down, if no other hand has hold of it
						(unless (some (# (and (not (eql %0 state)) (eql (elem-get %0 +stroke_tool) :hand)
								(elem-get %0 +stroke_item)))
								(map (const second) (. (get :states this) :tolist)))
							(. board :float (list))))
					(:t (defq band (get :band board) near (/ +board_near (get :zoom board)))
						(def board :band :nil)
						(cond
							((and (eql snap :tap) (defq menu (get :menu board))
									(<= (abs (- x sx)) near) (<= (abs (- y sy)) near))
								;a tap on nothing, the board's menu, if it has
								;one, lib/cwb/palette.inc
								(menu board x y event))
							(band (. board :select (map (# (elem-get (first %0) +cwb_id))
								(cwb-in-box (get :doc board) band)))))
						(. board :touch +board_dirty_overlay))))
			(:eraser (unless moved (. board :forget))
				(def board :rubber :nil)
				(. board :touch +board_dirty_overlay))
			(:pan)
			(:t ;what was drawn goes into the document, if it is anything
				(. board :drop_temp id)
				(cond
					((and (find tool '(:pen)) (>= (length points) 2))
						;a line drawn by hand has the points that matter, and is curved
						(defq kept (if (> (length points) 4) (path-filter 0.5 (apply (const path) points) (path)) points))
						(cwb-set item :d (board-pen-d (map (const identity) kept)))
						(. board :commit item))
					((and (/= (length points) 2) (third (cwb-flat item)))
						(. board :commit item))))))

	(defmethod :pointers (stage events)
		; (. surface :pointers stage events) -> surface
		(raise :board)
		;two fingers, or two of anything that moves things, down on what is
		;selected, move it, turn it and size it together
		(each (lambda (event)
			(bind '(id kind buttons &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(. this :set_state id (. this :begin event)))
				((and state (/= buttons 0))
					(elem-set state +stroke_last_x (elem-get event +pev_x))
					(elem-set state +stroke_last_y (elem-get event +pev_y))
					(unless (get :grab this) (. this :step event state)))
				(state
					(unless (get :grab this) (. this :step event state))
					(. this :end event state)
					(. this :set_state id :nil))))
			events)
		(. this :two_hands)
		this)

	(defmethod :two_hands ()
		; (. surface :two_hands)
		;if two pointers have hold of what is selected, the two of them
		;move it, turn it and size it, by where they were when the second
		;took hold and where they are now. While they do, neither moves it
		;alone. When one lets go the other carries on from where things are
		(raise :board)
		(defq hands (list) grab (get :grab this))
		(. (get :states this) :each (lambda (id state)
			(if (and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item))
				(push hands (list id state)))))
		(cond
			((< (length hands) 2)
				(when grab
					(def this :grab :nil)
					(each (lambda ((id state))
						(elem-set state +stroke_x (elem-get state +stroke_last_x))
						(elem-set state +stroke_y (elem-get state +stroke_last_y))
						(elem-set state +stroke_item (. this :things))) hands)))
			(:t (defq a (second (first hands)) b (second (second hands))
					pa (list (elem-get a +stroke_last_x) (elem-get a +stroke_last_y))
					pb (list (elem-get b +stroke_last_x) (elem-get b +stroke_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb (. this :things)))))
				(bind '(pa0 pb0 things) grab)
				(defq move (board-two-point-mat pa0 pb0 pa pb))
				(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul move m0))) things)
				(each (lambda ((id state)) (elem-set state +stroke_moved :t)) hands)
				(. board :touch +board_dirty_overlay))))
	)

;;;;;;;;;;;;;
; the handles
;;;;;;;;;;;;;

(defclass Handles (board) (Actor)
	; (Handles board) -> handles
	;what is selected is sized and turned by these, in select mode. They
	;are in front of the surface, and a point is on them only where one is.
	;
	;One thing selected has them in its own frame: the box of what it is,
	;moved, turned and sized as it is. So a box that has been turned has its
	;handles at its own corners, and is sized along its own sides, and
	;stays a box. Several things have the box round them all.
	;
	;A line has none of that. It has a point at each end, and each is
	;dragged to where that end is to be.
	(def this :board board)

	(defmethod :box ()
		; (. handles :box) -> :nil | (x y x1 y1)
		;the box round what is selected, :nil if nothing is, or the board
		;is not in select mode
		(raise :board)
		(and (eql (get :mode board) :select) (nempty? (get :selected board))
			(cwb-bounds (. board :selected_items))))

	(defmethod :frame ()
		; (. handles :frame) -> :nil | (matrix box)
		;the box the handles are on, and the matrix that puts it where it
		;is. Of one thing, its own box and its own matrix. Of several, the
		;box round them and no change
		(raise :board)
		(when (and (eql (get :mode board) :select) (nempty? (defq items (. board :selected_items))))
			(cond
				((= (length items) 1)
					(defq item (first items)
						box (if (cwb-group? item) (cwb-bounds (elem-get item +cwb_items)) (third (cwb-flat item))))
					(if box (list (ifn (elem-get item +cwb_m) (cwb-mat)) box)))
				((defq box (cwb-bounds items)) (list (cwb-mat) box)))))

	(defmethod :ends ()
		; (. handles :ends) -> :nil | (item (x y x1 y1))
		;if what is selected is one line, it, and its two ends in its own space
		(raise :board)
		(when (and (eql (get :mode board) :select) (= (length (defq items (. board :selected_items))) 1)
				(cwb-shape? (defq item (first items)))
				(find (elem-get item +cwb_kind) '(:line :arrow :arrow2)))
			(defq parts (split (elem-get item +cwb_d) " "))
			(if (and (= (length parts) 6) (eql (first parts) "M") (eql (elem-get parts 3) "L"))
				(list item (map (# (n2f (str-to-num (elem-get parts %0)))) '(1 2 4 5))))))

	(defmethod :corners ()
		; (. handles :corners) -> :nil | (x y x y x y x y)
		;the four corners of the frame, where they are on the board
		(when (defq frame (. this :frame))
			(bind '(m (x y x1 y1)) frame)
			(cat (cwb-mat-point m x y) (cwb-mat-point m x1 y) (cwb-mat-point m x1 y1) (cwb-mat-point m x y1))))

	(defmethod :spots ()
		; (. handles :spots) -> ((kind x y fx fy) ...)
		;where each handle is on the board. kind is :size, :turn or :end.
		;fx and fy say which way a size handle pulls, -1 0 or 1 each way,
		;in the frame. fx of an end is which, 0 or 1
		(raise :board)
		(cond
			((defq ends (. this :ends))
				(bind '(item (x y x1 y1)) ends)
				(defq m (elem-get item +cwb_m))
				(list (cat (list :end) (cwb-mat-point m x y) (list 0 0))
					(cat (list :end) (cwb-mat-point m x1 y1) (list 1 0))))
			((defq frame (. this :frame))
				(bind '(m (x y x1 y1)) frame)
				(defq mx (* 0.5 (+ x x1)) my (* 0.5 (+ y y1))
					spots (map (lambda ((lx ly fx fy)) (cat (list :size) (cwb-mat-point m lx ly) (list fx fy)))
						(list (list x y -1 -1) (list mx y 0 -1) (list x1 y 1 -1)
							(list x my -1 0) (list x1 my 1 0)
							(list x y1 -1 1) (list mx y1 0 1) (list x1 y1 1 1))))
				;the one to turn it by is out from the middle of its top side,
				;the way that side faces
				(bind '(tx ty) (cwb-mat-point m mx y))
				(bind '(cx cy) (cwb-mat-point m mx my))
				(defq ux (- tx cx) uy (- ty cy) len (sqrt (+ (* ux ux) (* uy uy)))
					gap (/ +board_turn_gap (get :zoom board)))
				(if (= len 0.0) (setq ux 0.0 uy -1.0 len 1.0))
				(push spots (list :turn (+ tx (/ (* ux gap) len)) (+ ty (/ (* uy gap) len)) 0 0)))))

	(defmethod :spot_at (x y)
		; (. handles :spot_at x y) -> :nil | spot
		(raise :board)
		(defq reach (/ (* 1.6 +board_handle) (get :zoom board)))
		(some (lambda (spot)
			(bind '(kind sx sy &ignore) spot)
			(if (and (<= (abs (- x sx)) reach) (<= (abs (- y sy)) reach)) spot))
			(ifn (. this :spots) (list))))

	(defmethod :hit (x y)
		(if (. this :spot_at x y) :t))

	(defmethod :pointers (stage events)
		(raise :board)
		(each (lambda (event)
			(bind '(id kind buttons x y &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(when (and (defq spot (. this :spot_at x y)) (= (. this :held) 0))
						(. board :snapshot)
						(. board :float (get :selected board))
						;the spot, the frame and the ends as they were, where
						;the pointer went down, and each thing as it was
						(. this :set_state id (list spot (. this :frame) (. this :ends) x y
							(map (# (list %0 (elem-get %0 +cwb_m))) (. board :selected_items))))))
				(state
					(bind '((skind sx sy fx fy) frame ends dx dy things) state)
					(defq px (. board :snap_x x) py (. board :snap_y y) step (get :snap_angle board))
					(case skind
						(:end ;that end of the line goes to the pointer, the other stays
							(bind '(item (lx ly lx1 ly1)) ends)
							(defq m0 (second (first things)))
							(when (/= step 0.0)
								;the line is at an angle that is a step, as long as it is pulled
								(bind '(ox oy) (if (= fx 0) (cwb-mat-point m0 lx1 ly1) (cwb-mat-point m0 lx ly)))
								(defq vx (- px ox) vy (- py oy) len (sqrt (+ (* vx vx) (* vy vy)))
									angle (board-snap (path-angle vx vy) step))
								(setq px (+ ox (* len (cos angle))) py (+ oy (* len (sin angle)))))
							(bind '(qx qy) (cwb-mat-point (cwb-mat-invert m0) px py))
							(cwb-set item :d (if (= fx 0) (cwb-d-line qx qy lx1 ly1) (cwb-d-line lx ly qx qy))))
						(:turn ;about the middle of the frame
							(bind '(fm (bx by bx1 by1)) frame)
							(bind '(cx cy) (cwb-mat-point fm (* 0.5 (+ bx bx1)) (* 0.5 (+ by by1))))
							(defq angle (- (path-angle (- x cx) (- y cy)) (path-angle (- dx cx) (- dy cy))))
							(while (> angle +fp_pi) (setq angle (- angle +fp_2pi)))
							(while (<= angle (neg +fp_pi)) (setq angle (+ angle +fp_2pi)))
							(if (/= step 0.0) (setq angle (board-snap angle step)))
							(defq m (cwb-mat-turn angle cx cy))
							(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul m m0))) things))
						(:t ;sized in the frame: the pointer is taken into it, and the
							;side or corner across from the handle stays where it is
							(bind '(fm (bx by bx1 by1)) frame)
							(defq inv (cwb-mat-invert fm) cx (* 0.5 (+ bx bx1)) cy (* 0.5 (+ by by1)))
							(bind '(qx qy) (cwb-mat-point inv px py))
							(defq ax (cond ((< fx 0) bx1) ((> fx 0) bx) (:t cx))
								ay (cond ((< fy 0) by1) ((> fy 0) by) (:t cy))
								w0 (- (cond ((< fx 0) bx) ((> fx 0) bx1) (:t cx)) ax)
								h0 (- (cond ((< fy 0) by) ((> fy 0) by1) (:t cy)) ay)
								scx (if (or (= fx 0) (= w0 0.0)) 1.0 (/ (- qx ax) w0))
								scy (if (or (= fy 0) (= h0 0.0)) 1.0 (/ (- qy ay) h0)))
							;a corner keeps the shape of the box unless the board says not
							(if (and (/= fx 0) (/= fy 0) (get :keep_shape board))
								(setq scx (if (> (abs scx) (abs scy)) scx (* (abs scy) (if (< scx 0.0) -1.0 1.0)))
									scy (* (abs scx) (if (< scy 0.0) -1.0 1.0))))
							(if (= scx 0.0) (setq scx 0.001))
							(if (= scy 0.0) (setq scy 0.001))
							;in the frame, so out of it, sized, and back into it
							(defq m (cwb-mat-mul fm (cwb-mat-mul (cwb-mat-scale scx scy ax ay) inv)))
							(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul m m0))) things)))
					(. board :touch +board_dirty_overlay)
					(when (= buttons 0)
						(. this :set_state id :nil)
						(. board :float (list))))))
			events)
		this)
	)

;;;;;;;;;;;
; the board
;;;;;;;;;;;

(defclass Board (&optional doc) :nil
	; (Board [doc]) -> board
	(def this :doc (ifn doc (cwb-doc)) :stage (Stage) :bindings (bindings-default)
		:mode :pen :layer :nil :selected (list) :undo_stack (list) :redo_stack (list)
		:color 0xff000000 :fill 0 :width 3.0
		:text "" :font "fonts/OpenSans-Regular.ctf" :font_size 32
		:snap 0.0 :snap_angle 0.0 :keep_shape :t :zoom 1.0
		;the eraser: how far it reaches, on the screen, and :part, it rubs
		;out the part of a line it goes over, or :whole, the whole of it
		:rub_reach 8.0 :rub_mode :part :rubber :nil
		:temp (Fmap 11) :band :nil :pan :nil :dirty (+ +board_dirty_doc +board_dirty_overlay)
		:floating (list) :appended (list) :changes (list :all) :menu :nil :menu_colors :nil :menu_widths :nil :time 0)
	(def this :surface (Surface this) :handles (Handles this))
	(.-> (get :stage this) (:add (get :surface this)) (:add (get :handles this)))

	;a thing a board has, and a method of the same name, can not both be:
	;a method is found by its name in the board as what it has is
	(defmethod :get_doc () (get :doc this))
	(defmethod :get_stage () (get :stage this))
	(defmethod :get_bindings () (get :bindings this))

	(defmethod :set_doc (doc)
		; (. board :set_doc doc) -> board
		;another document, nothing of the last is kept
		(def this :doc doc :selected (list) :undo_stack (list) :redo_stack (list) :layer :nil :band :nil
			:floating (list) :appended (list))
		(. this :changed :all)
		(. (get :temp this) :empty)
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :touch (flags)
		; (. board :touch flags) -> board
		;something has changed that is drawn
		(def this :dirty (logior (get :dirty this) flags))
		this)

	(defmethod :dirty? (flags)
		; (. board :dirty? flags) -> :t | :nil
		;has that changed, and it is taken as drawn
		(defq dirty (get :dirty this))
		(def this :dirty (logand dirty (lognot flags)))
		(/= (logand dirty flags) 0))

	(defmethod :float (ids)
		; (. board :float ids) -> board
		;these items are being moved about. They are drawn over the
		;document, not in it, so that only they are drawn as they move
		(. this :changed (get :floating this))
		(def this :floating (cat ids))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :changed (ids)
		; (. board :changed ids) -> board
		;these items of the layers are not what they were, or are new, or
		;:all for anything might be. It is kept till it is asked for, for
		;what keeps a copy of the document in step with this one, the nodes
		;that draw it in stripes, lib/cwb/stripes.inc. An item that is gone
		;need not be said, the layers are told whole
		(if (eql ids :all)
			(def this :changes (list :all))
			(unless (find :all (get :changes this))
				(each (# (unless (find %0 (get :changes this)) (push (get :changes this) %0))) ids)))
		this)

	(defmethod :take_changes ()
		; (. board :take_changes) -> :all | ids
		;what has changed since this was last asked
		(defq changes (get :changes this))
		(def this :changes (list))
		(if (find :all changes) :all changes))

	(defmethod :take_appended ()
		; (. board :take_appended) -> items
		;the items put on top since this was last asked
		(defq items (get :appended this))
		(def this :appended (list))
		items)

	(defmethod :snap_x (x) (board-snap x (get :snap this)))
	(defmethod :snap_y (y) (board-snap y (get :snap this)))

	(defmethod :layer_index ()
		; (. board :layer_index) -> index
		;the layer that is drawn on, the top one unless one was chosen
		(defq layers (cwb-layers (get :doc this)) index (get :layer this))
		(if (and index (< index (length layers))) index (dec (length layers))))

	;;;;;;;;;;;;;;
	; undo and redo
	;;;;;;;;;;;;;;

	(defmethod :state ()
		; (. board :state) -> state
		;the layers as they are, to go back to
		(map (lambda ((name flags items)) (list name flags (board-copy-items items)))
			(cwb-layers (get :doc this))))

	(defmethod :snapshot ()
		; (. board :snapshot) -> board
		;what is about to be done can be undone
		(push (get :undo_stack this) (. this :state))
		(clear (get :redo_stack this))
		this)

	(defmethod :forget ()
		; (. board :forget) -> board
		;nothing was done after all, the last (:snapshot) is dropped
		(if (nempty? (get :undo_stack this)) (pop (get :undo_stack this)))
		this)

	(defmethod :put_state (state)
		(. (get :doc this) :insert :layers state)
		(. this :changed :all)
		(def this :selected (list) :floating (list) :appended (list))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :undo ()
		; (. board :undo) -> :t | :nil
		(when (nempty? (get :undo_stack this))
			(push (get :redo_stack this) (. this :state))
			(. this :put_state (pop (get :undo_stack this)))
			:t))

	(defmethod :redo ()
		; (. board :redo) -> :t | :nil
		(when (nempty? (get :redo_stack this))
			(push (get :undo_stack this) (. this :state))
			(. this :put_state (pop (get :redo_stack this)))
			:t))

	;;;;;;;;;;;;;;;;;;
	; what is selected
	;;;;;;;;;;;;;;;;;;

	(defmethod :select (ids)
		; (. board :select ids) -> board
		(def this :selected (cat ids))
		(. this :touch +board_dirty_overlay))

	(defmethod :get_selected () (get :selected this))

	(defmethod :selected_items ()
		; (. board :selected_items) -> items
		;the items that are selected, each one of a layer's own, in the
		;order they are in, the back one first
		(defq ids (get :selected this) out (list))
		(each (lambda ((name flags items))
			(each (# (if (find (elem-get %0 +cwb_id) ids) (push out %0))) items))
			(cwb-layers (get :doc this)))
		out)

	(defmethod :select_all ()
		; (. board :select_all) -> board
		(defq ids (list))
		(each (lambda ((name flags items))
			(unless (or (bits? flags 1) (bits? flags 2))
				(each (# (push ids (elem-get %0 +cwb_id))) items)))
			(cwb-layers (get :doc this)))
		(. this :select ids))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; what is being drawn, not yet kept
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :new_shape (event tool)
		; (. board :new_shape event tool) -> shape
		;a shape for a pointer to draw, with the colour and width that
		;pointer is bound to, or the board's. It is drawn over the
		;document till it is kept
		(defq bindings (get :bindings this)
			color (. bindings :value event :color (get :color this))
			width (. bindings :value event :width (get :width this))
			fill (. bindings :value event :fill (get :fill this))
			shape (cwb-shape "" :stroke color :width width :kind tool))
		(case tool
			((:frect :fellipse) (cwb-set shape :fill color :stroke 0))
			((:rect :ellipse) (cwb-set shape :fill fill :join :miter))
			(:arrow (cwb-set shape :cap1 :butt :cap2 :arrow))
			(:arrow2 (cwb-set shape :cap1 :arrow :cap2 :arrow)))
		(. (get :temp this) :insert (elem-get event +pev_id) shape)
		shape)

	(defmethod :drop_temp (id)
		(. (get :temp this) :erase id)
		(. this :touch +board_dirty_overlay))

	;;;;;;;;;;;;;;;;;;;;;;;;;;
	; changes to the document
	;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :commit (item)
		; (. board :commit item) -> item
		;put an item on the layer that is drawn on, as a step that can be undone
		(. this :snapshot)
		(cwb-add (get :doc this) item (. this :layer_index))
		(. this :changed (list (elem-get item +cwb_id)))
		;on the top layer it is on top of all there is, and can be drawn there
		(if (= (. this :layer_index) (dec (length (cwb-layers (get :doc this)))))
			(progn (push (get :appended this) item)
				(. this :touch (+ +board_dirty_append +board_dirty_overlay)))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		item)

	(defmethod :add (item)
		; (. board :add item) -> id
		;as (:commit), and the id of it, for a script
		(elem-get (. this :commit item) +cwb_id))

	(defmethod :remove (ids)
		; (. board :remove ids) -> board
		;take items out, no step is made, see (:delete)
		(each (# (cwb-remove (get :doc this) %0)) ids)
		(def this :selected (filter (# (not (find %0 ids))) (get :selected this)))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :rub (x y &optional r)
		; (. board :rub x y [r]) -> :t | :nil
		;rub out at a point, as the eraser does, r is how far it reaches,
		;default the board's by its zoom. Of a line that is near, the part
		;that is within r goes, and what is left of it is lines in its
		;place. If no line was touched, the thing on top at the point goes
		;whole. With :rub_mode :whole a line goes whole too. No step is
		;made. Was anything rubbed out
		(defq doc (get :doc this) r (n2f (ifn r (/ (get :rub_reach this) (get :zoom this))))
			x (n2f x) y (n2f y) reach (list (- x r) (- y r) (+ x r) (+ y r)) done :nil)
		(when (eql (get :rub_mode this) :part)
			(each (lambda ((name flags items))
				(unless (or (bits? flags 1) (bits? flags 2))
					(defq out (list) changed :nil)
					(each (lambda (item)
						(cond
							((and (cwb-rub? item)
									(defq box (cwb-bounds (list item)))
									(cwb-box-meet? reach (list (- (first box) 1.0) (- (second box) 1.0)
										(+ (third box) 1.0) (+ (elem-get box 3) 1.0)))
									(defq pieces (cwb-rub item x y r)))
								(setq changed :t done :t)
								(def this :selected (filter (# (/= %0 (elem-get item +cwb_id))) (get :selected this)))
								(each (# (push out (cwb-adopt doc %0))) pieces)
								(. this :changed (map (# (elem-get %0 +cwb_id)) pieces)))
							(:t (push out item))))
						items)
					(when changed
						;what is left is where it was, among the rest
						(clear items)
						(each (# (push items %0)) out))))
				(cwb-layers doc)))
		(when (and (not done) (defq hit (cwb-hit doc x y (/ +board_near (get :zoom this)))))
			(. this :remove (list (elem-get (first hit) +cwb_id)))
			(setq done :t))
		(if done (. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		done)

	(defmethod :rub_along (x y x1 y1 &optional r)
		; (. board :rub_along x y x1 y1 [r]) -> :t | :nil
		;rub out from one point to another, as the eraser goes, at every
		;step of half its reach
		(defq r (n2f (ifn r (/ (get :rub_reach this) (get :zoom this))))
			x (n2f x) y (n2f y) dx (- (n2f x1) x) dy (- (n2f y1) y)
			steps (max 1 (n2i (/ (sqrt (+ (* dx dx) (* dy dy))) (* 0.5 r)))) done :nil)
		(each (lambda (i)
			(defq f (/ (n2f i) (n2f steps)))
			(if (. this :rub (+ x (* dx f)) (+ y (* dy f)) r) (setq done :t)))
			(range 0 (inc steps)))
		done)

	(defmethod :delete ()
		; (. board :delete) -> board
		;take out what is selected
		(when (nempty? (get :selected this))
			(. this :snapshot)
			(. this :remove (cat (get :selected this))))
		this)

	(defmethod :fit (&optional pad)
		; (. board :fit [pad]) -> :nil | (dx dy)
		;the document is made the size of what is in it, (cwb-fit), as a
		;step that can be undone, and what is on the stage that has a
		;place, an instrument, is moved with it. How far it all moved
		(. this :snapshot)
		(cond
			((defq by (cwb-fit (get :doc this) pad))
				(bind '(dx dy) by)
				(each (lambda (actor)
					(when (defq origin (def? :origin actor))
						(def actor :origin (list (+ (first origin) dx) (+ (second origin) dy)))))
					(. (get :stage this) :get_actors))
				(. this :changed :all)
				(. this :touch (+ +board_dirty_doc +board_dirty_overlay))
				by)
			(:t (. this :forget) :nil)))

	(defmethod :crop ()
		; (. board :crop) -> num
		;take out every item that is nowhere on the paper, the document's
		;width and height from its top left, as a step that can be undone.
		;One that is partly on it stays, all of it. Those in a layer that
		;is locked stay. How many went
		(defq doc (get :doc this) gone (list)
			paper (list 0.0 0.0 (n2f (. doc :find :width)) (n2f (. doc :find :height))))
		(each (lambda ((name flags items))
			(unless (bits? flags 2)
				(each (lambda (item)
					(unless (and (defq box (cwb-bounds (list item))) (cwb-box-meet? paper box))
						(push gone (elem-get item +cwb_id))))
					items)))
			(cwb-layers doc))
		(when (nempty? gone)
			(. this :snapshot)
			(. this :remove gone)
			(. this :changed :all))
		(length gone))

	(defmethod :clear ()
		; (. board :clear) -> board
		;take out everything
		(. this :snapshot)
		(. this :changed :all)
		(each (lambda (layer) (elem-set layer 2 (list))) (cwb-layers (get :doc this)))
		(def this :selected (list))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :transform (m)
		; (. board :transform m) -> board
		;move what is selected by a matrix
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(each (# (elem-set %0 +cwb_m (cwb-mat-mul m (elem-get %0 +cwb_m)))) items)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :style (&rest key_vals)
		; (. board :style [key val] ...) -> board
		;set the colour, fill, width and so on of every shape that is
		;selected, those in groups too
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(cwb-walk items :nil (lambda (shape m) (apply (const cwb-set) (cat (list shape) key_vals))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :group ()
		; (. board :group) -> :nil | id
		;what is selected becomes one group, where the top one of them was
		(defq items (. this :selected_items))
		(when (> (length items) 1)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) top (last items) top_id (elem-get top +cwb_id)
				holder (third (cwb-find doc top_id))
				group (cwb-adopt doc (cwb-group items))
				kept (map (# (if (= (elem-get %0 +cwb_id) top_id) group %0)) holder))
			(clear holder)
			(each (# (push holder %0)) kept)
			(each (# (unless (= (elem-get %0 +cwb_id) top_id) (cwb-remove doc (elem-get %0 +cwb_id)))) items)
			;the group is in the list, the items are now only in the group
			(def this :selected (list (elem-get group +cwb_id)))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay))
			(elem-get group +cwb_id)))

	(defmethod :ungroup ()
		; (. board :ungroup) -> board
		;each group that is selected becomes its items, where it was, each
		;where it was on the board
		(defq groups (filter (const cwb-group?) (. this :selected_items)))
		(when (nempty? groups)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) ids (list))
			(each (lambda (group)
				(defq id (elem-get group +cwb_id) holder (third (cwb-find doc id))
					kept (reduce (lambda (out item)
						(if (= (elem-get item +cwb_id) id)
							(each (lambda (kid)
								(elem-set kid +cwb_m (cwb-mat-mul (elem-get group +cwb_m) (elem-get kid +cwb_m)))
								(push ids (elem-get kid +cwb_id))
								(push out kid)) (elem-get group +cwb_items))
							(push out item))
						out) holder (list)))
				(clear holder)
				(each (# (push holder %0)) kept))
				groups)
			(def this :selected (cat ids (filter (# (not (find %0 (map (# (elem-get %0 +cwb_id)) groups))))
				(get :selected this))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :order (front)
		; (. board :order front) -> board
		;what is selected goes to the front of its layer, or the back
		(when (nempty? (defq ids (get :selected this)))
			(. this :snapshot)
			(each (lambda (layer)
				(defq items (third layer)
					these (filter (# (find (elem-get %0 +cwb_id) ids)) items)
					rest_of (filter (# (not (find (elem-get %0 +cwb_id) ids))) items))
				(if (nempty? these)
					(elem-set layer 2 (if front (cat rest_of these) (cat these rest_of)))))
				(cwb-layers (get :doc this)))
			(. this :touch +board_dirty_doc))
		this)

	(defmethod :align (how)
		; (. board :align how) -> board
		;line up what is selected, each with the box round all of it. how is
		;:left :right :top :bottom, or :hcenter or :vcenter for the middles
		(defq items (. this :selected_items))
		(when (and (> (length items) 1) (defq all (cwb-bounds items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(bind '(ax ay ax1 ay1) all)
			(each (lambda (item)
				(when (defq box (cwb-bounds (list item)))
					(bind '(x y x1 y1) box)
					(defq dx (case how (:left (- ax x)) (:right (- ax1 x1))
							(:hcenter (- (* 0.5 (+ ax ax1)) (* 0.5 (+ x x1)))) (:t 0.0))
						dy (case how (:top (- ay y)) (:bottom (- ay1 y1))
							(:vcenter (- (* 0.5 (+ ay ay1)) (* 0.5 (+ y y1)))) (:t 0.0)))
					(elem-set item +cwb_m (cwb-mat-mul (cwb-mat-move dx dy) (elem-get item +cwb_m)))))
				items)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :duplicate (&optional dx dy)
		; (. board :duplicate [dx dy]) -> board
		;a copy of what is selected, moved a little, default 16 each way,
		;and the copy is what is selected
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(defq doc (get :doc this) move (cwb-mat-move (ifn dx 16) (ifn dy 16)) ids (list))
			(each (lambda (copy)
				;a copy and all in it has ids of its own
				(defq todo (list copy))
				(while (defq next (pop todo))
					(elem-set next +cwb_id 0)
					(if (cwb-group? next) (each (# (push todo %0)) (elem-get next +cwb_items))))
				(elem-set copy +cwb_m (cwb-mat-mul move (elem-get copy +cwb_m)))
				(cwb-add doc copy (. this :layer_index))
				(push ids (elem-get copy +cwb_id)))
				(board-copy-items items))
			(def this :selected ids)
			(. this :changed ids)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	;;;;;;;;;;
	; pointers
	;;;;;;;;;;

	(defmethod :pointers (events)
		; (. board :pointers events) -> board
		;a batch of pointer events, each in the space of the document
		(. (get :stage this) :pointers events)
		this)

	(defmethod :tick (time)
		; (. board :tick time) -> :t | :nil
		;the time is now that, microseconds, for what is on the stage that
		;moves by itself. Is any of it still moving, it is then to be
		;drawn again
		(defq moving :nil)
		(def this :time time)
		(each (# (if (. %0 :tick time) (setq moving :t))) (cat (. (get :stage this) :get_actors)))
		(if moving (. this :touch +board_dirty_overlay))
		moving)

	(defmethod :dismiss (event)
		; (. board :dismiss event) -> :t | :nil
		;a pointer has gone down on the surface, what is only there till
		;then goes. Did any
		(defq went :nil)
		(each (# (if (. %0 :dismiss event) (setq went :t))) (cat (. (get :stage this) :get_actors)))
		went)

	(defmethod :draw_actors (canvas &optional m)
		; (. board :draw_actors canvas [m]) -> board
		;what is on the stage and not of the document, a ruler, a palette,
		;the back one first. After (:draw_overlay), it is over that
		(each (# (. %0 :draw canvas m)) (. (get :stage this) :get_actors))
		this)

	;;;;;;;;;
	; drawing
	;;;;;;;;;

	(defmethod :draw (canvas &optional m clip)
		; (. board :draw canvas [m clip]) -> count
		;the document, on a canvas, by the matrix it is seen by
		(cwb-draw canvas (get :doc this) m clip (get :floating this)))

	(defmethod :draw_overlay (canvas &optional m)
		; (. board :draw_overlay canvas [m]) -> board
		;what is over the document and not of it: what is being drawn,
		;the box being dragged out, and the handles of what is selected
		(defq zoom (get :zoom this) thin (/ 1.0 zoom))
		(when (nempty? (defq floating (get :floating this)))
			(each (lambda ((name flags items))
				(cwb-draw-items canvas (filter (# (find (elem-get %0 +cwb_id) floating)) items) m))
				(cwb-layers (get :doc this))))
		(. (get :temp this) :each (lambda (id shape) (cwb-draw-items canvas (list shape) m)))
		(when (defq rubber (get :rubber this))
			;the eraser, where it is, as far as it reaches
			(bind '(x y r) rubber)
			(cwb-draw-items canvas (list (cwb-shape (cwb-d-ellipse x y r)
				:stroke 0xff808080 :width thin :fill 0x40ffffff)) m))
		(when (defq band (get :band this))
			(bind '(x y x1 y1) band)
			(cwb-draw-items canvas (list (cwb-shape (cwb-d-rect x y x1 y1)
				:stroke 0xff0080ff :width thin :fill 0x200080ff :join :miter)) m))
		(when (nempty? (defq items (. this :selected_items)))
			(defq handles (get :handles this))
			(cond
				;a line has only its two ends
				((. handles :ends))
				;the frame the handles are on, which may be turned
				((defq corners (. handles :corners))
					(cwb-draw-items canvas (list (cwb-shape (cwb-d-points corners :t)
						:stroke 0xff0080ff :width thin :join :miter)) m))
				((defq box (cwb-bounds items))
					(bind '(x y x1 y1) box)
					(cwb-draw-items canvas (list (cwb-shape (cwb-d-rect x y x1 y1)
						:stroke 0xff0080ff :width thin :join :miter)) m)))
			(when (defq spots (. handles :spots))
				(defq r (/ +board_handle zoom))
				(cwb-draw-items canvas (map (lambda ((kind sx sy &ignore))
					(cwb-shape (if (find kind '(:turn :end)) (cwb-d-ellipse sx sy r)
							(cwb-d-rect (- sx r) (- sy r) (+ sx r) (+ sy r)))
						:fill 0xffffffff :stroke 0xff0080ff :width thin :join :miter)) spots) m)))
		this)
	)
