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
(import "././math/polygon.inc")
(import "./pointer.inc")

;what has changed since it was last drawn
;the document, all of it. What is over it. And only that items were put on
;top of it, :appended, which can be drawn on what is there
(bits +board_dirty 0
	(bit doc overlay append))

(defq +board_handle 5.0 +board_near 4.0 +board_turn_gap 24.0)

;who made a thing, and who a device is: the teacher, the presenter, is 0,
;touch is anybody, and a student is a number from +board_first_student,
;each pen its own, see (. board :user_of)
(defq +board_teacher 0 +board_anybody 1 +board_first_student 2)

;a tap: a contact that lifts within this long, microseconds, and this near
;to where it landed, on the screen. Chris: "a short tap for select mode
;toggle is really quick tap. Not a press and hold." A tap on a thing takes
;it in or out of select mode, whatever the contact is bound to draw; on
;nothing it is what is drawn, a dot. Longer, or further, it is a draw
(defq +board_tap_time 200000 +board_tap_near +board_near)

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

(defun board-rub-look (paths zoom)
	; (board-rub-look paths zoom) -> shape
	;what an eraser is shown as, as far as it reaches, or the area it will
	;rub out: pale, with a grey line of dashes round it, 8 on and 4 off on
	;the screen whatever the zoom, a look nothing else has
	(defq thin (/ 1.0 zoom)
		dashes (if (nempty? paths) (apply (const cat) (map (# (path-dash %0 (* 8.0 thin) (* 4.0 thin) :t)) paths)) (list)))
	(cwb-group (list
		(cwb-shape (cwb-d-paths paths :t) :fill 0x40ffffff :stroke 0)
		(cwb-shape (cwb-d-paths dashes) :stroke 0xff808080 :width (* 1.5 thin) :fill 0))))

(defun board-none? (ink)
	; (board-none? ink) -> :t | :nil
	;an ink that is wholly see through is none, whatever its colour
	(= (logand ink 0xff000000) 0))

(defun board-rubs? (bindings event mode)
	; (board-rubs? bindings event mode) -> :t | :nil
	;does the pointer rub out: an eraser, or bound to, by its tool or by
	;its mode, the board's mode if it has none. Along an instrument's edge
	;what rubs out rubs out and all else draws, a finger too, Chris: "you
	;can hold an instrument with 2 fingers and draw your third or forth down
	;the edges, and they WILL draw !"
	(or (eql (elem-get event +pev_kind) :eraser)
		(eql (defq tool (. bindings :value event :tool :pen)) :eraser)
		(and (eql tool :pen) (eql (. bindings :value event :mode mode) :eraser))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a bite: what an eraser covers taken out of what it touches, exactly
;
; What the eraser does to anything that is not a line, and with
; :rub_mode :bite to a line too. What the eraser covers as it
; is moved is a polygon, (poly-sweep), and that is taken out of every
; thing it touches, (poly-difference), lib/math/polygon.inc. A thing that
; loses a bite is not what it was: it is flat, one shape for what was
; left of its fill and one for what was left of its line, each a filled
; outline. A thing not touched is left as it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun board-hull (item)
	; (board-hull item) -> :nil | path
	;the hull of all a shape draws, in its own space, a path with no dent
	;in it that the shape is inside. Kept with what else is kept of the
	;shape, (cwb-flat), and worked out only when first asked for
	(defq cache (cwb-flat item))
	(when (third cache)
		(if (< (length cache) 4) (push cache (poly-hull (cat (first cache) (second cache)))))
		(elem-get cache 3)))

(defun board-bite-shape (item full m swept &optional tol)
	; (board-bite-shape item full m swept [tol]) -> :nil | (shape ...)
	;a shape with what swept covers taken out of it. swept is paths in
	;the space of the document, full the matrix that puts the shape there,
	;m the one that puts the space the shape is in there. :nil if it is
	;not touched. What is to stand in its place, in the space it is in:
	;nothing if all of it went, or all but a sliver, and the shape is then
	;out of the document.
	;
	;What a line drawn by hand is drawn with has a great many points, ten
	;for each of its own, the round of every join: a long squiggle has
	;twenty thousand, and all that is done here is by how many there are.
	;So those that are within tol of the line between their neighbours
	;are left out first, a quarter of a pixel if not given, which is not
	;seen and is most of them
	(bind '(fills strokes &ignore) (cwb-flat item))
	(defq mode (if (eql (elem-get item +cwb_rule) :evenodd) 0 1) tol (ifn tol 0.25)
		thin (# (filter (# (>= (length %0) 6)) (map (# (if (> (length %0) 64) (path-filter tol %0 (path)) %0)) %0)))
		fills (thin (cwb-mat-paths full fills)) strokes (thin (cwb-mat-paths full strokes)))
	;and of a thing that is many rings, a line that has been bitten into
	;lengths, only the rings near what is swept are worked on. The rest go
	;as they are. That is so of a thing filled by odd and even, which is
	;what a bite leaves: each ring turns what is in it over, in or out,
	;and one that is nowhere near what is swept does that the same before
	;and after. Not of one filled the other way, its rings are not apart
	(bind '((bx by) (bx1 by1)) (vector-bounds-2d swept))
	(defq far (list))
	(when (and (= mode 0) (> (length fills) 1))
		(defq near (list))
		(each (lambda (ring)
			(bind '((x y) (x1 y1)) (vector-bounds-2d (list ring)))
			(push (if (and (< x bx1) (< bx x1) (< y by1) (< by y1)) near far) ring))
			fills)
		(setq fills near))
	;is it touched at all. Of a thing that is only rings filled by odd and
	;even, and those are apart, what is left is worked out and is less
	;than there was, or it is not: one sum, not two. Of any other, do
	;they share anything is asked first
	(defq whole (and (= mode 0) (empty? strokes) (nempty? fills)) first_left :nil)
	(when (if whole
			(progn (setq first_left (poly-difference fills swept 0 1))
				(< (poly-area first_left) (- (poly-area fills) 0.01)))
			(or (and (nempty? fills) (nempty? (poly-intersect fills swept mode 1)))
				(and (nempty? strokes) (nempty? (poly-intersect strokes swept 1 1)))))
		;what is left is in the shape's own space and has its matrix, so it
		;is turned and placed as it was and its handles are too: a bite
		;changes what it is, not how it lies
		(defq out (list) back (Mat3x2-invert full)
			;the hull of what it was has what is left of it in it, and
			;does for that: not worked out again for each bite
			hull (board-hull item))
		(each (lambda (polys mode color)
			(when (nempty? polys)
				(defq left (if (and first_left (eql polys fills)) first_left (poly-difference polys swept mode 1)))
				;with the rings that were not near
				(if (eql polys fills) (setq left (cat left far)))
				;what is left has to be something: a sliver of less than a
				;square unit of the document is not seen, and would stay
				;for ever as a shape that is not there
				(when (and (nempty? left) (>= (poly-area left) 1.0))
					(defq own (cwb-mat-paths back left)
						shape (cwb-shape (join (map (# (cwb-d-points %0 :t)) own) " ")
							:fill color :stroke 0 :rule :evenodd :name (cwb-get item :name) :user (elem-get item +cwb_user)))
					(elem-set shape +cwb_m (elem-get item +cwb_m))
					;what it is drawn with is what was just worked out: kept,
					;so its path is not read back from its text to find it
					(bind '((x y) (x1 y1)) (vector-bounds-2d own))
					(elem-set shape +cwb_cache (if hull (list own (list) (list x y x1 y1) hull) (list own (list) (list x y x1 y1))))
					(push out shape))))
			(list fills strokes) (list mode 1) (list (elem-get item +cwb_fill) (elem-get item +cwb_stroke)))
		;what is left of its fill and of its line are two things now, each
		;a polygon, no group of them
		out))

(defun board-bite (doc swept &optional lines but tol user)
	; (board-bite doc swept [lines but tol user]) -> :nil | (made gone swaps)
	;user, whose eraser it is: a shape another made is left as it is, but
	;by the teacher's, +board_teacher, or with no user said. Chris: "students
	;could not erase things the teacher or the other student had written"
	;swaps is what stands in the place of each thing that was bitten,
	;((id (shape ...)) ...), for whoever had hold of it
	;tol, how near a line a point of a thing may be and be left out, see
	;(board-bite-shape)
	;but, the ids of things that are left as they are whatever covers
	;them: what somebody has hold of is theirs till they let it go
	;with lines :t a line, a thing part of which can be rubbed out as a
	;line, (cwb-rub?), is left as it is, it is cut another way
	;what swept covers, paths in the space of the document, taken out of
	;every thing of the layers that can be changed. The ids of the shapes
	;that were made, and of those they stand in the place of, :nil if
	;nothing was touched. A group whose things have all gone goes. A list
	;of what is left to do, no function here calls itself
	(bind '((x y) (x1 y1)) (vector-bounds-2d swept))
	(defq reach (list x y x1 y1) made (list) gone (list) todo (list) seen (list) swaps (list)
		;what is swept, with no dent in it. A thing is found by its box,
		;then by its hull against this, (poly-convex-hit?), and only then
		;by all of its polygons, which is the dear one. Most things near
		;an eraser are not touched by it, a line that slants has a box
		;far bigger than it is
		swept_hull (poly-hull swept))
	(each (lambda ((name flags items))
		(unless (or (bits? flags 1) (bits? flags 2)) (push todo (list items :nil))))
		(cwb-layers doc))
	(while (defq job (pop todo))
		(bind '(items m) job)
		(defq out (list) changed :nil)
		(each (lambda (item)
			(defq pieces :nil)
			(cond
				((cwb-group? item)
					;what is in it, in its own space, later
					(push todo (list (elem-get item +cwb_items) (Mat3x2-mul m (elem-get item +cwb_m)))))
				((and lines (cwb-rub? item)))
				((and but (find (elem-get item +cwb_id) but)))
				((and user (/= user +board_teacher) (/= user (elem-get item +cwb_user))))
				((defq box (third (cwb-flat item)))
					(defq full (Mat3x2-mul m (elem-get item +cwb_m)))
					(and (vector-collide-boxes-2d reach (Mat3x2-box box full))
							(poly-convex-hit? (first (cwb-mat-paths full (list (board-hull item)))) swept_hull)
						(setq pieces (board-bite-shape item full m swept tol)))))
			(cond
				(pieces
					(setq changed :t)
					(push gone (elem-get item +cwb_id))
					(push swaps (list (elem-get item +cwb_id) pieces))
					(each (# (push out (cwb-adopt doc %0)) (push made (elem-get %0 +cwb_id))) pieces))
				(:t (push out item))))
			items)
		(when changed
			;what is left is where it was, among the rest
			(clear items)
			(each (# (push items %0)) out))
		(push seen items))
	;the groups with nothing left in them, the innermost first
	(reach (lambda (items)
		(when (some (# (and (cwb-group? %0) (empty? (elem-get %0 +cwb_items)))) items)
			(defq out (filter (# (not (and (cwb-group? %0) (empty? (elem-get %0 +cwb_items))))) items))
			(each (# (and (cwb-group? %0) (empty? (elem-get %0 +cwb_items)) (push gone (elem-get %0 +cwb_id)))) items)
			(clear items)
			(each (# (push items %0)) out))) seen)
	(if (nempty? gone) (list made gone swaps)))

(defun board-rub-group (doc group x y r)
	; (board-rub-group doc group x y r) -> :nil | items
	;the items of a group with what of its lines is within r of a point,
	;of the space the group is in, rubbed out. :nil if none was touched.
	;What is in it that is not a line, or is a group itself, is left
	(defq m (elem-get group +cwb_m) size 1.0 touched :nil out (list))
	(when m
		(setq size (max 0.001 (sqrt (abs (- (* (elem-get m 0) (elem-get m 4)) (* (elem-get m 1) (elem-get m 3))))))))
	(bind '(lx ly) (Mat3x2-point (Mat3x2-invert m) x y))
	(each (lambda (item)
		(cond
			((and (cwb-rub? item) (defq pieces (cwb-rub item lx ly (/ (n2f r) size))))
				(setq touched :t)
				(each (# (push out (cwb-adopt doc %0))) pieces))
			(:t (push out item))))
		(elem-get group +cwb_items))
	(if touched out))

(defun board-item-corners (item)
	; (board-item-corners item) -> :nil | (x y x y x y x y)
	;the four corners of the box of an item, its own box by its own
	;matrix, where they are in the space it is in. :nil if it draws nothing
	(when (defq box (if (cwb-group? item) (cwb-bounds (elem-get item +cwb_items)) (third (cwb-flat item))))
		(bind '(x y x1 y1) box)
		(defq m (elem-get item +cwb_m))
		(cat (Mat3x2-point m x y) (Mat3x2-point m x1 y) (Mat3x2-point m x1 y1) (Mat3x2-point m x y1))))

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
	(Mat3x2-mul (Mat3x2-translate m1x m1y)
		(Mat3x2-mul (Mat3x2-rot angle)
			(Mat3x2-mul (Mat3x2-scale scale) (Mat3x2-translate (neg m0x) (neg m0y))))))

;;;;;;;;;;;;;
; the surface
;;;;;;;;;;;;;

;what is kept for a pointer that is down on the surface, a list
;again is that what a hand went down on was selected already, (:end);
;solo that it was not, and is picked up only while the hand has it
(enums +stroke 0
	(enum tool x y points item moved last_x last_y snap box again began buttons grow solo))

(defclass Surface (board) (Actor)
	; (Surface board) -> surface
	;the canvas, the actor at the back, every point is on it: what no
	;other actor takes is its. A stroke is drawn, a shape dragged out, words
	;put down, a box dragged out round things, a tap is the board's menu if
	;it has one, and an eraser rubs out what it goes over. It knows nothing
	;of what is held, (Things). A finger that goes down here puts away
	;nobody's palette
	(def this :board board :grab :nil)

	(defmethod :hit (x y &optional event) :t)

	(defmethod :rub_flush ()
		; (. surface :rub_flush) -> :t | :nil
		;the bite of every way an eraser has gone since the last. Was
		;anything rubbed out
		(raise :board)
		(defq todo (get :rub_todo board) ways (list) done :nil)
		(. todo :each (lambda (id (way reach user)) (push ways (list id way reach user))))
		(each (lambda ((id way reach user))
			(. todo :erase id)
			(when (. board :bite way reach :nil user)
				(setq done :t)
				(if (defq state (. this :state id)) (elem-set state +stroke_moved :t))))
			ways)
		done)

	(defmethod :tick (time)
		; (. surface :tick time) -> :nil
		(. this :rub_flush)
		:nil)

	(defmethod :drawing? ()
		; (. surface :drawing?) -> :t | :nil
		;is any pointer drawing, or rubbing out: it has a tool that is not
		;the hand. A hand that holds a thing or drags a box out is not
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(unless (find (elem-get state +stroke_tool) '(:hand :idle)) (setq found :t))))
		found)

	(defmethod :begin (event)
		; (. surface :begin event) -> state
		;a pointer has gone down
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(defq tool (board-tool-of board event)
			;what was only there till the next pointer went down goes. Not
			;for a finger: one of many, it is not to put away the palette
			;of another. A tap of one on nothing does, (:end)
			shut (if (eql kind :touch) :nil (. board :dismiss event))
			state (list tool x y (list x y) :nil :nil x y :nil :nil :nil (get :time board) buttons 0.0 :nil)
			holding (. (get :things board) :holding))
		;what lands on a thing is the thing's, (Things), what is here is on
		;nothing: the canvas knows nothing of things, Chris: "The canvas
		;shouldn't be looking or even know if shapes or instruments are
		;held/selected/being dragged or not ! All it cares about is a tap will
		;bring up the menu, or a stroke will add a stroke or drag out a shape"
		(case tool
			(:hand
				;a box dragged out, or a tap. With nothing selected, and nothing
				;put away by it, it may be a tap, see (:end). What other hands
				;have hold of is selected, and is left so; what this user
				;picked up is put down, not what others did
				(and (not shut) (or holding (empty? (get :selected board)))
					(elem-set state +stroke_snap :tap))
				(unless holding (. board :select (. board :others_of (. board :user_of event)))))
			;what is selected stays so, and what is left of it after a
			;bite, (:bite), with its handles, Chris: "I can select a shape
			;and then erase though it, but the select controls vanish as
			;the erase takes a bite"
			(:eraser (. board :snapshot))
			(:text
				;the words the board has are put down where the pointer is,
				;they sit on the line through it, and that is all it does
				(elem-set state +stroke_tool :idle)
				(when (nempty? (defq words (get :text board)))
					(. board :commit (cwb-text words (. board :snap_x x id) (. board :snap_y y id)
						:fill (. board :ink_of event)
						:font (get :font board) :font_size (get :font_size board))
						(. board :user_of event))))
			(:t ;something is drawn. What is picked up stays so, another may be
				;drawing beside what one holds
				(elem-set state +stroke_x (. board :snap_x x id))
				(elem-set state +stroke_y (. board :snap_y y id))
				(elem-set state +stroke_points (list (elem-get state +stroke_x) (elem-get state +stroke_y)))
				;when it was begun, for what is written, (:written)
				(elem-set state +stroke_snap (get :time board))
				(elem-set state +stroke_item (. board :new_shape event tool))
				;a filled shape of one ink in both pens is a fill as far out
				;as its outline would be, (:shape_d)
				(when (find tool '(:frect :fellipse))
					(bind '(line fill) (. board :ink_pair event))
					(and (= line fill) (not (board-none? line))
						(elem-set state +stroke_grow (* 0.5 (n2f (. (get :bindings board) :value event :width (get :width board)))))))))
		state)

	(defmethod :shape_d (state x y)
		; (. surface :shape_d state x y) -> d
		;the path of what a pointer is drawing, now it is at a point
		(bind '(tool sx sy points &ignore) state)
		;a filled shape of one ink is as big as it would be with an outline
		(defq grow (elem-get state +stroke_grow))
		(case tool
			((:line :arrow :arrow2) (cwb-d-line sx sy x y))
			(:rect (cwb-d-rect sx sy x y))
			(:frect (cwb-d-rect (- (min sx x) grow) (- (min sy y) grow) (+ (max sx x) grow) (+ (max sy y) grow)))
			((:ellipse :fellipse)
				(cwb-d-ellipse (* 0.5 (+ sx x)) (* 0.5 (+ sy y)) (+ (* 0.5 (abs (- x sx))) grow) (+ (* 0.5 (abs (- y sy))) grow)))
			(:t (board-pen-d points))))

	(defmethod :step (event state)
		; (. surface :step event state) -> state
		;a pointer that is down has moved, or come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved &ignore) state)
		(case tool
			(:idle)
			(:hand ;a box is dragged out round things, this pointer's own
				(. (get :bands board) :insert id (list (min sx x) (min sy y) (max sx x) (max sy y)))
				(. board :touch +board_dirty_overlay))
			(:eraser
				;from where it was to where it is, and it is shown there.
				;A bite is not taken for each move of the hand: on a thing
				;of many points one takes longer than the next move is in
				;coming, and the hand is then left behind. The way it has
				;gone is kept, and one bite of all of it is taken when
				;there is time, (:rub_flush): at the next tick, or as the
				;eraser lifts. However far behind, the eraser is where the
				;hand is
				(bind '(ox oy) points)
				;how far it reaches is the width this pointer draws with:
				;thin, it rubs out a little, thick, a lot. Chris: "The
				;eraser mode, should use the current pen size ?"
				(defq reach (. board :reach (. (get :bindings board) :value event :width (get :width board))))
				(cond
					((eql (get :rub_mode board) :bite)
						(defq todo (get :rub_todo board) going (. todo :find id))
						(unless going (. todo :insert id (setq going (list (path ox oy) reach (. board :user_of event)))))
						(push (first going) x y))
					((. board :rub_along ox oy x y reach) (elem-set state +stroke_moved :t)))
				(elem-set state +stroke_points (list x y))
				(def board :rubber (list x y reach))
				(. board :touch +board_dirty_overlay))
			(:t ;something is drawn. A line from where it began, at a step of
				;angle if angles snap, (:snap_end)
				(bind '(x y) (if (find tool '(:line :arrow :arrow2)) (. board :snap_end sx sy x y id)
					(list (. board :snap_x x id) (. board :snap_y y id))))
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
				(progn (defq bands (get :bands board) band (. bands :find id) near (/ +board_near (get :zoom board))
							holding (. (get :things board) :holding))
						(. bands :erase id)
						(cond
							((and (eql snap :tap) (defq menu (get :menu board))
									(<= (abs (- x sx)) near) (<= (abs (- y sy)) near))
								;a tap on nothing, the board's menu, if it has
								;one, lib/cwb/palette.inc. A tap of a finger
								;puts away one that is open, and opens one if
								;there was none to put away
								(unless (and (eql kind :touch) (. board :dismiss event))
									(menu board x y event)))
							;what the box touches is selected, it need not be
							;all round a thing: a long line, or a thing at
							;the edge of the paper, can not be got round. With
							;what other hands have hold of, which stays so
							(band (defq found (map (# (elem-get (first %0) +cwb_id)) (cwb-touch-box (get :doc board) band))
									;and what other users picked up, a box is this
									;one's, and a tap on nothing puts down what this
									;user picked up
									user (. board :user_of event)
									kept (if holding (get :selected board) (. board :others_of user)))
								;what it takes is that user's, as a tap's is
								(defq new (filter (# (not (find %0 kept))) found))
								(. board :select (cat kept new))
								(each (# (. board :pick_up %0 user)) new)))
						(. board :touch +board_dirty_overlay)))
			(:eraser
				(if (. this :rub_flush) (setq moved :t))
				(setq moved (or moved (elem-get state +stroke_moved)))
				(unless moved (. board :forget))
				(def board :rubber :nil)
				;a pen rubs out with the button on its side held, and it has
				;no other way to its menu: a tap of that on nothing, that
				;rubbed nothing out, is the board's menu, as a tap of a hand
				;is. Not the other end of a pen, which is an eraser and no more
				(defq near (/ +board_near (get :zoom board)))
				(and (not moved) (eql kind :pen) (defq menu (get :menu board))
						(<= (abs (- x sx)) near) (<= (abs (- y sy)) near)
					(menu board x y event))
				(. board :touch +board_dirty_overlay))
			(:t ;what was drawn goes into the document, if it is anything
				(. board :drop_temp id)
				(cond
					((and (find tool '(:pen)) (>= (length points) 2))
						;a line drawn by hand has the points that matter, and is curved
						(defq kept (if (> (length points) 4) (path-filter 0.5 (apply (const path) points) (path)) points))
						(cwb-set item :d (board-pen-d (map (const identity) kept)))
						(. board :commit item (. board :user_of event))
						;one written soon after the last is of the same word
						(. board :written item id snap))
					((and (/= (length points) 2) (third (cwb-flat item)))
						(. board :commit item (. board :user_of event)))))))

	(defmethod :tap? (state x y)
		; (. surface :tap? state x y) -> :t | :nil
		;did a contact lift as a tap: soon, and near where it landed. A
		;board that is told no time, (:tick), goes by the distance alone
		(raise :board)
		(bind '(& sx sy &ignore) state)
		(defq near (/ +board_tap_near (get :zoom board)))
		(and (<= (abs (- x sx)) near) (<= (abs (- y sy)) near)
			(< (- (get :time board) (elem-get state +stroke_began)) +board_tap_time)))

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
					(. this :step event state))
				(state
					;the last step is of the buttons it went down with: one
					;that has come up has none, and is bound as no button is,
					;an eraser of another width took one more bite as it
					;lifted, Chris: "When I release the button on an erase
					;action, I see a blip of change"
					(defq last (cat event))
					(elem-set last +pev_buttons (elem-get state +stroke_buttons))
					(. this :step last state)
					(. this :end event state)
					(. this :set_state id :nil))))
			events)
		this)

	)

;;;;;;;;;;;;
; the things
;;;;;;;;;;;;

(defun board-tool-of (board event)
	; (board-tool-of board event) -> tool
	;what a pointer is bound to do: its tool, :hand :eraser :pan, or a pen
	;of what its mode draws, the board's if it has none, and select mode the
	;hand, so a mouse alone can do everything
	(defq bindings (get :bindings board) tool (. bindings :value event :tool :pen))
	(if (eql tool :pen) (setq tool (. bindings :value event :mode (get :mode board))))
	(if (eql tool :select) :hand tool))

(defun board-theirs? (board id user)
	; (board-theirs? board id user) -> :t | :nil
	;held by a user other than that one. What no one holds, anyone may
	;take
	(and (defq who (. board :holder id)) (not (eql who user))))

(defun board-tap? (board state x y)
	; (board-tap? board state x y) -> :t | :nil
	;did a contact lift as a tap: soon, and near where it landed. A board
	;that is told no time, (:tick), goes by the distance alone
	(defq near (/ +board_tap_near (get :zoom board)))
	(and (<= (abs (- x (elem-get state +stroke_x))) near) (<= (abs (- y (elem-get state +stroke_y))) near)
		(< (- (get :time board) (elem-get state +stroke_began)) +board_tap_time)))

(defclass Things (board) (Actor)
	; (Things board) -> things
	;the things of the document, on the stage in front of the canvas, each
	;picking itself up. Chris: "The shape picks itself up, that's the
	;insight about oops !" A contact is theirs by their own rule, asked as it
	;lands, (:hit): one of the hand, or of select mode, on a thing's own
	;ink, never only in a box round it; one that draws, on what its user
	;holds, which a drag of it moves, Chris:
	;"When a shape/group is selected, drag always moves it". One that draws
	;on a thing no one holds is theirs till it is a tap, which picks the
	;thing up, Chris: "single tap to select a shape, in any mode"; moved
	;further it is the canvas's, given it as it was, (. stage :pass), and
	;draws over the thing. What rubs out is never theirs, it bites through.
	;
	;A hand that goes down on a thing has hold of that thing, and of no
	;other, and what another hand does elsewhere is nothing to it. Two
	;hands that go down on the same thing move, turn and size it between
	;them
	(def this :board board :grab :nil)

	(defmethod :holder (item_id)
		; (. surface :holder item_id) -> :nil | (id state)
		;the hand that has hold of an item, the pointer it is and what is
		;kept for it
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(and (eql (elem-get state +stroke_tool) :hand) (defq things (elem-get state +stroke_item))
					(some (# (= (elem-get (first %0) +cwb_id) item_id)) things)
				(setq found (list id state)))))
		found)

	(defmethod :swapped (swaps)
		; (. surface :swapped swaps) -> surface
		;things have been made other things, ((id (shape ...)) ...), a
		;bite out of each. A hand that had hold of one has hold of what
		;stands in its place, from where things are now: it goes on moving
		;them, and two hands on them go on turning them, with no jump. If
		;nothing is left of what it held it holds nothing, and does nothing
		(defq any :nil)
		(. (get :states this) :each (lambda (id state)
			(when (and (eql (elem-get state +stroke_tool) :hand) (defq things (elem-get state +stroke_item))
					(some (lambda ((thing m0)) (some (# (= (first %0) (elem-get thing +cwb_id))) swaps)) things))
				(setq any :t)
				(defq now (list))
				(each (lambda ((thing m0))
					(if (defq swap (some (# (if (= (first %0) (elem-get thing +cwb_id)) %0)) swaps))
						(each (# (push now (list %0 (elem-get %0 +cwb_m)))) (second swap))
						(push now (list thing (elem-get thing +cwb_m)))))
					things)
				(elem-set state +stroke_x (elem-get state +stroke_last_x))
				(elem-set state +stroke_y (elem-get state +stroke_last_y))
				(elem-set state +stroke_box :nil)
				(cond
					((empty? now) (elem-set state +stroke_item :nil) (elem-set state +stroke_tool :idle))
					(:t (elem-set state +stroke_item now))))))
		;two that had hold of them together go on from where they and the
		;things are now, with what is left: begun again here, before
		;either of them moves again, or that move would be lost
		(when (and any (defq grab (get :grab this)))
			(bind '(ida idb) (elem-get grab 3))
			(defq a (. this :state ida) b (. this :state idb))
			(def this :grab (and a b (elem-get a +stroke_item) (elem-get b +stroke_item)
				(list (list (elem-get a +stroke_last_x) (elem-get a +stroke_last_y))
					(list (elem-get b +stroke_last_x) (elem-get b +stroke_last_y))
					(map (# (list (first %0) (elem-get (first %0) +cwb_m))) (elem-get a +stroke_item))
					(list ida idb)))))
		this)

	(defmethod :held_ids ()
		; (. surface :held_ids) -> (id ...)
		;every item some hand has hold of now
		(defq out (list))
		(. (get :states this) :each (lambda (id state)
			(and (eql (elem-get state +stroke_tool) :hand) (defq things (elem-get state +stroke_item))
				(each (# (push out (elem-get (first %0) +cwb_id))) things))))
		out)

	(defmethod :same_things (a b)
		; (. surface :same_things things things) -> :t | :nil
		;do two hands have hold of the same things
		(and a b (nempty? a) (= (length a) (length b))
			(= (elem-get (first (first a)) +cwb_id) (elem-get (first (first b)) +cwb_id))))

	(defmethod :things ()
		; (. surface :things) -> ((item matrix) ...)
		;what is selected, each with where it is now, to be moved from there
		(map (# (list %0 (elem-get %0 +cwb_m))) (. (get :board this) :selected_items)))

	(defmethod :holding ()
		; (. surface :holding) -> :nil | id
		;a pointer that has hold of what is selected
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item)
				(setq found id))))
		found)

	(defmethod :holders ()
		; (. surface :holders) -> num
		;how many pointers have hold of things
		(defq n 0)
		(. (get :states this) :each (lambda (id state)
			(and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item) (++ n))))
		n)

	(defmethod :hit (x y &optional event)
		; (. things :hit x y [event]) -> :t | :nil
		;is the contact theirs, see above. Asked with no contact, a point
		;that is only over the board, it is not
		(raise :board)
		(when event
			(defq tool (board-tool-of board event))
			(unless (find tool '(:eraser :pan :idle))
				(defq doc (get :doc board) near (/ +board_near (get :zoom board))
					user (. board :user_of event) mine (. board :selected_by user))
				;on a thing's own ink, and nowhere else: a box round
				;things is a picture, not a place to take hold of them
				(and (cwb-hit doc x y near) (or (eql tool :hand) (not (eql tool :text))
						(find (elem-get (first (cwb-hit doc x y near)) +cwb_id) mine))
					:t))))

	(defmethod :begin (event)
		; (. things :begin event) -> state
		;a pointer has gone down on them
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(defq tool (board-tool-of board event) doc (get :doc board) near (/ +board_near (get :zoom board))
			user (. board :user_of event) mine_ids (. board :selected_by user)
			hit (cwb-hit doc x y near)
			state (list :hand x y (list x y) :nil :nil x y :nil :nil :nil (get :time board) buttons 0.0 :nil)
			holding (. this :holding))
		;one that draws, on a thing its user does not hold, may be a tap.
		;What another user holds is theirs till they put it down: no tap
		;or hand of anyone else's takes it or puts it down
		(cond
			((and hit (board-theirs? board (elem-get (first hit) +cwb_id) user))
				(elem-set state +stroke_tool :idle))
			((and (not (eql tool :hand)) hit (not (find (elem-get (first hit) +cwb_id) mine_ids)))
				(elem-set state +stroke_tool :maybe)
				(elem-set state +stroke_item (elem-get (first hit) +cwb_id))
				(elem-set state +stroke_points event))
			(:t (when hit
				(cond
					((and hit (defq other (. this :holder (elem-get (first hit) +cwb_id))))
						;what another has hold of. First come first served:
						;a thing is the one's who took it, till they let it
						;go, and nobody else's hand does anything to it.
						;Unless it is the same one's other hand, a second
						;finger of the device that has it: then the two of
						;them move it, see (:two_hands), it has the same
						;things. An id is its device and which of its
						;contacts, lib/cwb/pointer.inc
						;two contacts of one touch device: Chris, "The only
						;device that is able to do multi contact
						;drag/zoom/rotate is the touch device"
						(if (and (eql kind :touch) (= (ptr-device id) (ptr-device (first other))))
							(elem-set state +stroke_item (elem-get (second other) +stroke_item))
							(elem-set state +stroke_tool :idle)))
					((board-theirs? board (elem-get (first hit) +cwb_id) user)
						(elem-set state +stroke_tool :idle))
					(hit (defq id_hit (elem-get (first hit) +cwb_id) selected (get :selected board) mine :nil)
						;on what was selected already, and no other hand at work.
						;Not by the right button alone: that sends a thing to the
						;back, below, and is to do that and no more. Chris: "i'd
						;like to keep the right button push to back action. We
						;have esc or min to get out of jail"
						(and (not holding) (find id_hit selected) (/= buttons +pev_right)
							(elem-set state +stroke_again id_hit))
						(cond
							(holding
								;other hands have hold of other things. This
								;one has its own: it is selected as well as
								;theirs, and it alone is this hand's
								(unless (find id_hit selected) (. board :select (cat selected (list id_hit))))
								(setq mine (list (first hit))))
							;its user's, and what other users picked up stays
							;so, Chris: "2 users can select seperate item".
							;Pressed on one it does not hold, the hand has
							;that one alone, to move it or send it front or
							;back, and what the user holds stays as it is. It
							;is picked up while the hand has it, so it is in
							;flight, and is put down again as it lets go
							;unless that was a tap, which selects it, (:end).
							;On one it holds, the hand has all it holds
							((not (find id_hit mine_ids))
								(. board :pick_up id_hit user)
								(elem-set state +stroke_solo id_hit)
								(setq mine (list (first hit))))
							(:t (setq mine (filter (# (find (elem-get %0 +cwb_id) mine_ids)) (. board :selected_items)))))
						(. board :snapshot)
						(elem-set state +stroke_snap :t)
						;what is taken hold of comes to the front of its layer, and
						;taken by the right button alone it goes to the back, as an
						;instrument does among the instruments: it is how what is
						;under it is got at. Where that changes what is in front of
						;what, it is a change, though nothing is moved
						(defq ids (cwb-id-set (map (# (elem-get %0 +cwb_id)) mine)) back (= buttons +pev_right))
						(each (lambda (layer)
							(defq items (third layer)
								these (cwb-pick items ids)
								others (cwb-pick items ids :t)
								now (if back (cat these others) (cat others these)))
							(unless (or (empty? these) (every (const eql) now items))
								(elem-set layer 2 now)
								(elem-set state +stroke_moved :t)
								(. board :touch_order)))
							(cwb-layers (get :doc board)))
						(elem-set state +stroke_item (map (# (list %0 (elem-get %0 +cwb_m))) mine))
						;the box round it as it is picked up, a corner of which
						;is what goes to the grid
						(elem-set state +stroke_box (cwb-bounds mine))
						;what is selected is in flight, (:touch), and its flight is
						;in front of the document or behind it as it was taken,
						;and stays there till it is taken again. Each hand says for
						;what it has: two people can be pulling one thing to the
						;front and pushing another to the back at once, Chris: "we
						;will have to have 2 inflight layers, back/front and the
						;committed layer in the middle"
						(. board :fly (map (# (elem-get %0 +cwb_id)) mine) back)
						(. board :touch +board_dirty_overlay))))))
		state)

	(defmethod :step (event state)
		; (. things :step event state) -> state
		;a pointer that is down has moved: what it holds goes with it. With
		;snap, the top left of the box round it goes to the grid, not the
		;pointer, so things line up with it and each other
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item &ignore) state)
		(when (and (eql tool :hand) item)
			(defq mx (- x sx) my (- y sy) box (elem-get state +stroke_box))
			(when box
				(setq mx (- (. board :snap_x (+ (first box) mx) id) (first box))
					my (- (. board :snap_y (+ (second box) my) id) (second box))))
			(defq move (Mat3x2-translate mx my))
			(each (lambda ((thing m0)) (elem-set thing +cwb_m (Mat3x2-mul move m0))) item)
			(if (or (/= x sx) (/= y sy)) (elem-set state +stroke_moved :t))
			(. board :touch +board_dirty_overlay))
		state)

	(defmethod :end (event state)
		; (. things :end event state)
		;a pointer has come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved lx ly snap &ignore) state)
		(case tool
			;a tap of what draws on a thing picks it up, or puts it down
			(:maybe (. board :toggle_select item (. board :user_of event)))
			(:hand
				(when item (and (eql snap :t) (not moved) (. board :forget))
						;a tap on what was picked up already puts it down, as a
						;tap of a pen on it does, and only it. One that was
						;not, held only while it was moved, is put down again:
						;a drag moves a thing and selects nothing, a tap does
						(defq tap (. this :tap? state x y))
						(and (defq again (elem-get state +stroke_again)) tap
							(. board :toggle_select again (. board :user_of event)))
						(and (defq solo (elem-get state +stroke_solo)) (not tap)
							(. board :select (filter (# (/= %0 solo)) (get :selected board))))
						;put down, it is selected still, and in flight still.
						;Unless other hands have hold of other things: then
						;what this one had is put down and is no longer
						;selected, theirs is. If another has hold of the same
						;things it has them still
						(defq others :nil shared :nil)
						(. (get :states this) :each (lambda (oid ostate)
							(and (/= oid id) (eql (elem-get ostate +stroke_tool) :hand)
									(defq theirs (elem-get ostate +stroke_item))
								(if (. this :same_things item theirs) (setq shared :t) (setq others :t)))))
						(when (and others (not shared))
							(defq mine (map (# (elem-get (first %0) +cwb_id)) item))
							(. board :select (filter (# (not (find %0 mine))) (get :selected board))))
						(. board :touch +board_dirty_overlay)))))

	(defmethod :tap? (state x y)
		; (. things :tap? state x y) -> :t | :nil
		(board-tap? (get :board this) state x y))

	(defmethod :pointers (stage events)
		; (. things :pointers stage events) -> things
		;two fingers, or two of anything that moves things, down on what is
		;selected, move it, turn it and size it together. One that may have
		;been a tap and is moved further is the canvas's, (. stage :pass)
		(raise :board)
		(each (lambda (event)
			(bind '(id kind buttons x y &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(. this :set_state id (. this :begin event)))
				((and state (eql (elem-get state +stroke_tool) :maybe))
					(cond
						((= buttons 0)
							(. this :set_state id :nil)
							;held too long to be a tap, it was a dot
							(if (board-tap? board state x y) (. this :end event state)
								(. stage :pass this id (list (elem-get state +stroke_points) event))))
						((not (board-tap? board state x y))
							(. this :set_state id :nil)
							(. stage :pass this id (list (elem-get state +stroke_points) event)))))
				((and state (/= buttons 0))
					(elem-set state +stroke_last_x (elem-get event +pev_x))
					(elem-set state +stroke_last_y (elem-get event +pev_y))
					(unless (. this :grabbed id) (. this :step event state)))
				(state
					(. this :end event state)
					(. this :set_state id :nil))))
			events)
		(. this :two_hands)
		this)

	(defmethod :grabbed (id)
		; (. surface :grabbed id) -> :nil | found
		;is a pointer one of two that have hold of the same things, (:two_hands)
		(and (defq grab (get :grab this)) (find id (elem-get grab 3))))

	(defmethod :two_hands ()
		; (. surface :two_hands)
		;if two pointers have hold of the same things, the two of them
		;move them, turn them and size them, by where they were when the
		;second took hold and where they are now. While they do, neither
		;moves them alone. When one lets go the other carries on from where
		;things are. Hands that have hold of other things go on as they were
		(raise :board)
		(defq hands (list) grab (get :grab this) pair :nil)
		(. (get :states this) :each (lambda (id state)
			(and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item)
				(push hands (list id state)))))
		;in the order they came, a contact that lands later has the bigger
		;number. What is kept of them comes in no order, and the two must be
		;the same way round each time: the other way round they are a half
		;turn from where they began
		(sort hands (# (- (first %0) (first %1))))
		;the two of a grab there is, if both are still down, or the first two
		;that have hold of the same things
		(when grab
			(defq still (filter (# (find (first %0) (elem-get grab 3))) hands))
			(if (= (length still) 2) (setq pair still)))
		(unless (or pair grab)
			(each (lambda ((ida sa))
				(each (lambda ((idb sb))
					(and (not pair) (/= ida idb)
							(. this :same_things (elem-get sa +stroke_item) (elem-get sb +stroke_item))
						(setq pair (list (list ida sa) (list idb sb)))))
					hands))
				hands))
		(cond
			((not pair)
				(when grab
					(def this :grab :nil)
					;the one of them that is left carries on from where things are
					(each (lambda ((id state))
						(when (find id (elem-get grab 3))
							(elem-set state +stroke_x (elem-get state +stroke_last_x))
							(elem-set state +stroke_y (elem-get state +stroke_last_y))
							(elem-set state +stroke_item
								(map (# (list (first %0) (elem-get (first %0) +cwb_m))) (elem-get state +stroke_item)))
							(elem-set state +stroke_box :nil)))
						hands)))
			(:t (defq a (second (first pair)) b (second (second pair))
					pa (list (elem-get a +stroke_last_x) (elem-get a +stroke_last_y))
					pb (list (elem-get b +stroke_last_x) (elem-get b +stroke_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb
						(map (# (list (first %0) (elem-get (first %0) +cwb_m))) (elem-get a +stroke_item))
						(list (first (first pair)) (first (second pair)))))))
				(bind '(pa0 pb0 things &ignore) grab)
				;one thing is moved, turned and sized; several, a loose
				;group, are not a thing, and are only moved, by the middle
				;of the two
				(defq move (if (= (length things) 1) (board-two-point-mat pa0 pb0 pa pb)
					(Mat3x2-translate
						(* 0.5 (- (+ (first pa) (first pb)) (first pa0) (first pb0)))
						(* 0.5 (- (+ (second pa) (second pb)) (second pa0) (second pb0))))))
				(each (lambda ((thing m0)) (elem-set thing +cwb_m (Mat3x2-mul move m0))) things)
				(each (lambda ((id state)) (elem-set state +stroke_moved :t)) pair)
				(. board :touch +board_dirty_overlay))))
	)

;;;;;;;;;;;;;
; the handles
;;;;;;;;;;;;;

(defclass Handles (board) (Actor)
	; (Handles board) -> handles
	;what is selected is sized and turned by these, in any mode. They
	;are in front of the surface, and a point is on them only where one is.
	;
	;One thing selected has them in its own frame: the box of what it is,
	;moved, turned and sized as it is. So a box that has been turned has its
	;handles at its own corners, and is sized along its own sides, and
	;stays a box. Several things, a loose group, are not a thing: they have
	;the box round them all, square to the board, and no handles. They are
	;moved together, and sized or turned once they are a group.
	;
	;A line has none of that. It has a point at each end, and each is
	;dragged to where that end is to be.
	(def this :board board)

	(defmethod :shown ()
		; (. handles :shown) -> :t | :nil
		;no more than one hand has hold of things
		(raise :board)
		(< (. (get :things board) :holders) 2))

	(defmethod :box ()
		; (. handles :box) -> :nil | (x y x1 y1)
		;the box round what is selected, :nil if nothing is. In any mode,
		;Chris: "single tap to select a shape, in any mode, does not bring up
		;the shape controls ?". Not while hands have hold of things each,
		;each has its own line, (:shown)
		(raise :board)
		(and (. this :shown) (nempty? (get :selected board))
			(cwb-bounds (. board :selected_items))))

	(defmethod :frame ()
		; (. handles :frame) -> :nil | (matrix box)
		;the box the handles are on, and the matrix that puts it where it
		;is. Of one thing, its own box and its own matrix. Of several, the
		;box round them, square to the board
		(raise :board)
		(and (. this :shown) (nempty? (defq items (. board :selected_items)))
			(cond
				((= (length items) 1)
					(defq item (first items)
						box (if (cwb-group? item) (cwb-bounds (elem-get item +cwb_items)) (third (cwb-flat item))))
					(if box (list (ifn (elem-get item +cwb_m) (Mat3x2)) box)))
				(:t ;the box round the corners of each: round the line drawn
					;round each, not only its ink, which a turned one's box
					;reaches past
					(defq pts (list))
					(each (lambda (item)
						(if (defq corners (board-item-corners item)) (each (# (push pts %0)) (partition corners 2))))
						items)
					(when (nempty? pts)
						(defq xs (map (const first) pts) ys (map (const second) pts))
						(list (Mat3x2) (list (reduce min xs) (reduce min ys) (reduce max xs) (reduce max ys))))))))

	(defmethod :ends ()
		; (. handles :ends) -> :nil | (item (x y x1 y1))
		;if what is selected is one line, it, and its two ends in its own space
		(raise :board)
		(when (and (. this :shown) (= (length (defq items (. board :selected_items))) 1)
				(cwb-shape? (defq item (first items)))
				(find (elem-get item +cwb_kind) '(:line :arrow :arrow2)))
			(defq parts (split (elem-get item +cwb_d) " "))
			(and (= (length parts) 6) (eql (first parts) "M") (eql (elem-get parts 3) "L")
				(list item (map (# (n2f (str-to-num (elem-get parts %0)))) '(1 2 4 5))))))

	(defmethod :corners ()
		; (. handles :corners) -> :nil | (x y x y x y x y)
		;the four corners of the frame, where they are on the board
		(when (defq frame (. this :frame))
			(bind '(m (x y x1 y1)) frame)
			(cat (Mat3x2-point m x y) (Mat3x2-point m x1 y) (Mat3x2-point m x1 y1) (Mat3x2-point m x y1))))

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
				(list (cat (list :end) (Mat3x2-point m x y) (list 0 0))
					(cat (list :end) (Mat3x2-point m x1 y1) (list 1 0))))
			;a loose group has none, (Handles)
			((and (= (length (. board :selected_items)) 1) (defq frame (. this :frame)))
				(bind '(m (x y x1 y1)) frame)
				(defq mx (* 0.5 (+ x x1)) my (* 0.5 (+ y y1))
					spots (map (lambda ((lx ly fx fy)) (cat (list :size) (Mat3x2-point m lx ly) (list fx fy)))
						(list (list x y -1 -1) (list mx y 0 -1) (list x1 y 1 -1)
							(list x my -1 0) (list x1 my 1 0)
							(list x y1 -1 1) (list mx y1 0 1) (list x1 y1 1 1))))
				;the one to turn it by is out from the middle of its top side,
				;the way that side faces
				(bind '(tx ty) (Mat3x2-point m mx y))
				(bind '(cx cy) (Mat3x2-point m mx my))
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
			(and (<= (abs (- x sx)) reach) (<= (abs (- y sy)) reach) spot))
			(ifn (. this :spots) (list))))

	(defmethod :zones_key ()
		; (. handles :zones_key) -> str
		;where the handles are, and the zoom, which how far one reaches is of
		(raise :board)
		(str (get :zoom board) (. this :spots)))

	(defmethod :zones ()
		; (. handles :zones) -> ((name polygons) ...)
		;a square about each handle, as far as it reaches, named for its kind
		(raise :board)
		(defq reach (/ (* 1.6 +board_handle) (get :zoom board)))
		(map (lambda ((kind sx sy &ignore))
				(list kind (list (list (- sx reach) (- sy reach) (+ sx reach) (- sy reach)
					(+ sx reach) (+ sy reach) (- sx reach) (+ sy reach)))))
			(ifn (. this :spots) (list))))

	(defmethod :pointers (stage events)
		(raise :board)
		(each (lambda (event)
			(bind '(id kind buttons x y &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(when (and (defq spot (. this :spot_at x y)) (= (. this :held) 0))
						(. board :snapshot)
						;the spot, the frame and the ends as they were, where
						;the pointer went down, and each thing as it was
						(. this :set_state id (list spot (. this :frame) (. this :ends) x y
							(map (# (list %0 (elem-get %0 +cwb_m))) (. board :selected_items))))))
				(state
					(bind '((skind sx sy fx fy) frame ends dx dy things) state)
					(defq px (. board :snap_x x id) py (. board :snap_y y id) step (. board :angle_step id))
					(case skind
						(:end ;that end of the line goes to the pointer, the other stays
							(bind '(item (lx ly lx1 ly1)) ends)
							(defq m0 (second (first things)))
							;from the other end, as a line is drawn, (:snap_end)
							(bind '(ox oy) (if (= fx 0) (Mat3x2-point m0 lx1 ly1) (Mat3x2-point m0 lx ly)))
							(bind '(px py) (. board :snap_end ox oy x y id))
							(bind '(qx qy) (Mat3x2-point (Mat3x2-invert m0) px py))
							(cwb-set item :d (if (= fx 0) (cwb-d-line qx qy lx1 ly1) (cwb-d-line lx ly qx qy))))
						(:turn ;about the middle of the frame
							(bind '(fm (bx by bx1 by1)) frame)
							(bind '(cx cy) (Mat3x2-point fm (* 0.5 (+ bx bx1)) (* 0.5 (+ by by1))))
							(defq angle (- (path-angle (- x cx) (- y cy)) (path-angle (- dx cx) (- dy cy))))
							(while (> angle +fp_pi) (setq angle (- angle +fp_2pi)))
							(while (<= angle (neg +fp_pi)) (setq angle (+ angle +fp_2pi)))
							(if (/= step 0.0) (setq angle (board-snap angle step)))
							(defq m (Mat3x2-rot angle cx cy))
							(each (lambda ((thing m0)) (elem-set thing +cwb_m (Mat3x2-mul m m0))) things))
						(:t ;sized in the frame: the pointer is taken into it, and the
							;side or corner across from the handle stays where it is
							(bind '(fm (bx by bx1 by1)) frame)
							(defq inv (Mat3x2-invert fm) cx (* 0.5 (+ bx bx1)) cy (* 0.5 (+ by by1)))
							(bind '(qx qy) (Mat3x2-point inv px py))
							(defq ax (cond ((< fx 0) bx1) ((> fx 0) bx) (:t cx))
								ay (cond ((< fy 0) by1) ((> fy 0) by) (:t cy))
								w0 (- (cond ((< fx 0) bx) ((> fx 0) bx1) (:t cx)) ax)
								h0 (- (cond ((< fy 0) by) ((> fy 0) by1) (:t cy)) ay)
								scx (if (or (= fx 0) (= w0 0.0)) 1.0 (/ (- qx ax) w0))
								scy (if (or (= fy 0) (= h0 0.0)) 1.0 (/ (- qy ay) h0)))
							;a corner keeps the shape of the box unless the board says not
							(and (/= fx 0) (/= fy 0) (. board :keep_shape? id)
								(setq scx (if (> (abs scx) (abs scy)) scx (* (abs scy) (if (< scx 0.0) -1.0 1.0)))
									scy (* (abs scx) (if (< scy 0.0) -1.0 1.0))))
							(if (= scx 0.0) (setq scx 0.001))
							(if (= scy 0.0) (setq scy 0.001))
							;in the frame, so out of it, sized, and back into it
							(defq m (Mat3x2-mul fm (Mat3x2-mul (Mat3x2-scale scx scy ax ay) inv)))
							(each (lambda ((thing m0)) (elem-set thing +cwb_m (Mat3x2-mul m m0))) things)))
					(. board :touch +board_dirty_overlay)
					(when (= buttons 0)
						(. this :set_state id :nil)))))
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
		:color 0xff000000 :width 3.0
		:text "" :font "fonts/OpenSans-Regular.ctf" :font_size 32
		:snap 0.0 :snap_angle 0.0 :keep_shape :t :snap_gap 0.0 :settings (Fmap 11) :zoom 1.0
		;the eraser: how far it reaches, on the screen, and :part, it rubs
		;out the part of a line it goes over, or :whole, the whole of it
		:rub_reach 8.0 :rub_mode :bite :rubber :nil :rub_todo (Fmap 11)
		;a line drawn by hand that is begun within this long of the end of
		;the last, by the same pointer, is put in a group with it, a word
		;that is written is one thing. Microseconds, 0 for not at all. It
		;goes by the time the board is told, (:tick), one that is told
		;none does none of it
		:auto_group 1500000 :last_stroke :nil
		:temp (Fmap 11) :bands (Fmap 11) :pan :nil :dirty (+ +board_dirty_doc +board_dirty_overlay)
		:contacts (Fmap 11) :show_contacts :t
		;a pen or a mouse that is near and not down, where it is, and
		;:hover_edges: it is shown the catch of the edge of an instrument
		;that would have it if it went down, those are :hover_shown
		:hovers (Fmap 11) :hover_edges :t :hover_shown (list)
		;what is in flight behind the document, by id, the rest is in front,
		;and what was behind as it landed, (:fly). :float_back is if any is
		:back_ids (list) :was_back (list)
		:floating (list) :float_back :nil :damage (list) :damage_full :t :appended (list) :changes (list :all) :menu :nil :menu_colors :nil :menu_widths :nil :time 0
		;who each device is, by its kind and number, see (:user_of)
		:users (Fmap 11) :next_user +board_first_student :selectors (Fmap 11)
		;the pens, each an ink, and the one held when nothing else is: a
		;slot holds a pen by its number, :pen, (:ink_of). :nil, none, a slot
		;holds an ink, :color
		:pens :nil :pen :nil)
	;the canvas at the back, the things in front of it, their handles in
	;front of them
	(def this :surface (Surface this) :things (Things this) :handles (Handles this))
	(.-> (get :stage this) (:add (get :surface this)) (:add (get :things this)) (:add (get :handles this)))

	;a thing a board has, and a method of the same name, can not both be:
	;a method is found by its name in the board as what it has is
	(defmethod :get_doc () (get :doc this))
	(defmethod :get_stage () (get :stage this))

	(defmethod :release ()
		; (. board :release) -> board
		;let go of what holds the board as the board holds it: its stage
		;holds its actors and each of them the board, round, which the count
		;of who holds a thing never lets go of, Chris: "if it points back at
		;itself it needs to break that cycle when it quits ! else it will
		;leak". Each actor lets go of the board, the stage of them all
		(defq stage (get :stage this))
		(each (# (undef %0 :board)) (. stage :get_actors))
		(each (# (if (get :board %0) (undef %0 :board))) (list (get :surface this) (get :things this) (get :handles this)))
		(def stage :actors (list) :owners (Fmap 11) :over (Fmap 11))
		this)
	(defmethod :get_bindings () (get :bindings this))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; users, who a device is, rights
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :user_of (event)
		; (. board :user_of event) -> user
		;who the device of a pointer is. The mouse is the teacher's, the
		;presenter's, Chris: "mouse was assumed to be the teacher/presenter".
		;Touch is anybody, "touch is anybody, we couldn't tell". The eraser
		;is the teacher's, "the eraser device input was always assumed to be
		;under the control of the teacher". A pen is a student, each its
		;own, numbered as it is first seen, unless the teacher has made it
		;the teacher's, (:appoint), and a pen bound to rub out is that pen's
		;user. SDL3 gives the other end of a pen as an :eraser, so here that
		;is the teacher's whoever's pen it is
		;A board that joined another's is a student's: what is the teacher
		;here is that board's own number, :user_base, see lib/cwb/share.inc
		(bind '(id kind &ignore) event)
		(defq users (get :users this) key (ptr-device id)
			user (cond
				((eql kind :eraser) +board_teacher)
				((. users :find key))
				((eql kind :mouse) +board_teacher)
				((eql kind :touch) +board_anybody)
				(:t (. users :insert key (defq user (get :next_user this)))
					(def this :next_user (inc user))
					user)))
		(if (or (= user +board_anybody) (not (defq base (get :user_base this)))) user (+ base user)))

	(defmethod :device_slot (kind id &optional buttons)
		; (. board :device_slot kind id [buttons]) -> :nil | 0 to 11
		;the place of a device and its button in the squares of the bars and
		;the menus. Its column, and its colour, the slot mod 6, Chris: "mouse
		;first, then touch, then 4 pens (teacher then 3 students)". Its row,
		;the slot / 6, the main button, the left, a pen's tip or touch, 0,
		;the second, the right, a pen's barrel or its other end, 1, Chris:
		;"We need to distinguish between the buttons of a device if they can
		;be bound to different modes". :nil for the middle of the mouse, not
		;shown. A pen not seen yet is given its user, a student counted as
		;it is first seen
		(defq buttons (ifn buttons +pev_left)
			row (cond
				((eql kind :eraser) 1)
				((eql kind :touch) 0)
				((/= (logand buttons +pev_right) 0) 1)
				((/= (logand buttons +pev_middle) 0) :nil)
				(0)))
		(when row
			(defq col (cond
				((eql kind :mouse) 0)
				((eql kind :touch) 1)
				(:t (defq users (get :users this) device (ptr-device id))
					(unless (. users :find device) (. this :user_of (ptr-event id :pen 0 0 0)))
					(defq user (. users :find device))
					(if (= user +board_teacher) 2 (min 5 (+ 3 (- user +board_first_student)))))))
			(+ (* row 6) col)))

	(defmethod :appoint (event)
		; (. board :appoint event) -> board
		;the device of this pointer is the teacher's from now on, a pack of
		;pens has one that is the teacher's. Who may do it is the caller's
		;to say, only a device that is the teacher's already
		(bind '(id &ignore) event)
		(. (get :users this) :insert (ptr-device id) +board_teacher)
		this)

	(defmethod :set_doc (doc)
		; (. board :set_doc doc) -> board
		;another document, nothing of the last is kept
		(def this :doc doc :undo_stack (list) :redo_stack (list) :layer :nil
			:floating (list) :appended (list))
		(. this :select (list))
		(. this :changed :all)
		(. (get :temp this) :empty)
		(. (get :bands this) :empty)
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :touch (flags)
		; (. board :touch flags) -> board
		;something has changed that is drawn.
		;
		;What is selected is in flight: it is not drawn with the document,
		;it is drawn by itself, (:draw_flight), in front of the document or
		;behind it, :float_back, and the document is not drawn again while
		;it is moved, turned or sized, however long that goes on. It lands
		;when it is no longer selected. So here, where every change comes,
		;what is in flight is made what is selected, and if that is not
		;what it was the document is to be drawn again, once
		;
		;Not all of it need be. When a few things go into flight or come
		;out of it the picture of the document changes only where they
		;are: :damage is the box round each, and (:take_damage) gives them
		;to what draws. Anything else that changes the document is told
		;here with +board_dirty_doc, and then it is all of it
		(if (bits? flags +board_dirty_doc) (def this :damage_full :t))
		(defq selected (get :selected this) floating (get :floating this))
		(unless (and (= (length selected) (length floating)) (every (const =) selected floating))
			(defq was (cwb-id-set floating) is (cwb-id-set selected)
				moved (cat (filter (# (not (cwb-id? was %0))) selected) (filter (# (not (cwb-id? is %0))) floating)))
			(cond
				((> (length moved) 32) (def this :damage_full :t))
				((not (get :damage_full this))
					(each (lambda ((name flags items))
						(each (# (if (defq box (cwb-bounds (list %0))) (push (get :damage this) box)))
							(cwb-pick items moved)))
						(cwb-layers (get :doc this)))))
			(. this :changed floating)
			(. this :changed selected)
			(def this :floating (cat selected))
			;a thing that leaves the flight leaves its side of it, and what was
			;behind is said so while it lands; a thing new to it is in front
			(defq back (get :back_ids this))
			(def this :was_back (filter (# (not (cwb-id? is %0))) back)
				:back_ids (filter (# (cwb-id? is %0)) back))
			(def this :float_back (nempty? (get :back_ids this)))
			(setq flags (logior flags +board_dirty_doc +board_dirty_overlay)))
		(def this :dirty (logior (get :dirty this) flags))
		this)

	(defmethod :touch_order ()
		; (. board :touch_order) -> board
		;the order of things in flight has changed and nothing else. The
		;picture of the document is as it was, they are not in it, but
		;what keeps a copy of the document is to be told
		(def this :dirty (logior (get :dirty this) +board_dirty_doc))
		this)

	(defmethod :take_damage ()
		; (. board :take_damage) -> :all | boxes
		;what of the document's picture is to be drawn again, since this
		;was last asked: all of it, or only these boxes of it, which may
		;be none
		(defq out (if (get :damage_full this) :all (get :damage this)))
		(def this :damage (list) :damage_full :nil)
		out)

	(defmethod :dirty? (flags)
		; (. board :dirty? flags) -> :t | :nil
		;has that changed, and it is taken as drawn
		(defq dirty (get :dirty this))
		(def this :dirty (logand dirty (lognot flags)))
		(/= (logand dirty flags) 0))

	(defmethod :float (ids &optional back)
		; (. board :float ids [back]) -> board
		;these items are selected, and so in flight, in front of the
		;document or with back behind it
		(. this :select ids)
		(. this :fly ids back)
		(. this :touch +board_dirty_overlay))

	(defmethod :fly (ids back)
		; (. board :fly ids back) -> board
		;these, in flight, are behind the document, or in front of it
		(defq set (cwb-id-set ids))
		(def this :back_ids (cat (filter (# (not (cwb-id? set %0))) (get :back_ids this)) (if back ids (list))))
		(def this :float_back (nempty? (get :back_ids this)))
		(. this :touch +board_dirty_overlay))

	(defmethod :back? (id)
		; (. board :back? id) -> :t | :nil
		;is it in flight behind the document, or was it as it landed
		(or (find id (get :back_ids this)) (find id (get :was_back this))))

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
		;and for the boards this one is shared with, lib/cwb/share.inc
		(when (defq log (get :share_log this))
			(if (eql ids :all) (push log :all) (each (# (push log %0)) ids)))
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

	;Snap to the grid, snap of angles and keeping the shape are each
	;device's, bound as its colour and width are, Chris: "per device, same
	;as ink and width". A device bound to none of its own has the board's,
	;:snap and :snap_angle a step, 0.0 for none, and :keep_shape. What a
	;pointer has is found as it goes down and kept till it comes up, so
	;all it does between is done by the one rule

	(defmethod :pointer_settings (event)
		; (. board :pointer_settings event) -> (snap angle keep_shape)
		;the snap step, the angle step and keep shape of a pointer, as it
		;is bound. Snap on is to the grid, :snap_gap, or the document's
		(defq bindings (get :bindings this)
			snap (. bindings :value event :snap :none) angle (. bindings :value event :snap_angle :none))
		(list (cond ((eql snap :none) (get :snap this))
				(snap (if (> (get :snap_gap this) 0.0) (get :snap_gap this) (n2f (. (get :doc this) :find :grid))))
				(0.0))
			(cond ((eql angle :none) (get :snap_angle this)) (angle (const (/ +fp_pi 12.0))) (0.0))
			(. bindings :value event :keep_shape (get :keep_shape this))))

	(defmethod :selecting? ()
		; (. board :selecting?) -> :t | :nil
		;is anything in select mode, the board or a device bound to it
		(or (eql (get :mode this) :select)
			(some (lambda ((& & & what))
					(and (defq at (ptr-at what :mode)) (eql (elem-get what at) :select)))
				(get :rules (get :bindings this)))))

	(defmethod :settle_select ()
		; (. board :settle_select) -> board
		;a mode has changed: what a box or a hand selected, no one's, is let
		;go of once nothing at all is in select mode. What a tap picked up
		;stays so in any mode, till it is tapped again or a tap of its user
		;on nothing in select mode puts it down, Chris: "single tap to select
		;a shape, in any mode", "select mode just means you draging a box to
		;multi select ?"
		(unless (. this :selecting?)
			(. this :select (filter (# (. this :holder %0)) (get :selected this))))
		this)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; a pick, of the toolbar, a ring or the swatches
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :pick (event what val)
		; (. board :pick event what val) -> board
		;what a pick does, wherever it was made, by what pressed, a pointer
		;event, or :nil for no device, a key or a script, which is the
		;board's own. With things its user holds, a colour, a pen or a width
		;is theirs, the main role the outline, the second the fill, (ptr-part),
		;and the slot is as it was; with none it is the slot's. Chris: "If you
		;have no selected items than I think you are setting the ink of the
		;pen, but if you have selected items you changing there colours".
		;	:pen n		a pen, by its number in :pens
		;	:color c	an ink of its own, not a pen's
		;	:width w
		;	:mode m		what a pen draws, :select to pick things up
		;	:switch k	:snap :snap_angle or :keep_shape, turned over
		;	:ink c		the swatches: the held pen filled with c, as see
		;	:alpha a	through as it was; how see through, a 0 to 255; or
		;	:none		none. With things held, each part made so
		(defq mine (if event (. this :selected_by (. this :user_of event)) (get :selected this))
			part (if event (ptr-part event) :stroke))
		(case what
			((:pen :color)
				(defq ink (if (eql what :pen) (elem-get (get :pens this) val) val))
				(if (nempty? mine) (. this :paint mine part ink) (. this :hold event what val)))
			(:width
				(if (nempty? mine) (. this :style_ids mine :width val) (. this :hold event :width val)))
			(:mode
				(. this :hold event :mode val)
				(. this :settle_select))
			(:switch
				(bind '(snap angle keep) (if event (. this :pointer_settings (ptr-main event))
					(list (get :snap this) (get :snap_angle this) (get :keep_shape this))))
				(cond
					(event (. this :hold event val (not (case val (:snap (/= snap 0.0)) (:snap_angle (/= angle 0.0)) (:t keep)))))
					((eql val :snap) (def this :snap (if (= snap 0.0)
						(if (> (get :snap_gap this) 0.0) (get :snap_gap this) (n2f (. (get :doc this) :find :grid))) 0.0)))
					((eql val :snap_angle) (def this :snap_angle (if (= angle 0.0) (const (/ +fp_pi 12.0)) 0.0)))
					((def this :keep_shape (not keep)))))
			((:ink :alpha :none)
				(defq held (if event (ptr-main event)) at (if held (. this :pen_of held) (get :pen this))
					was (if held (. this :ink_of held) (get :color this))
					alpha (if (board-none? was) 0xff000000 (logand was 0xff000000))
					ink (case what
						(:ink (logior (logand val 0xffffff) alpha))
						(:alpha (logior (logand was 0xffffff) (<< val 24)))
						(:t 0)))
				(if (board-none? ink) (setq ink 0))
				(cond
					;things held take it, each part from what it was: a colour
					;at how see through each was, how see through in the colour
					;each was. The lambdas find the value by a name of its own
					((nempty? mine)
						(defq board_pick_val val)
						(. this :paint mine part 0 (case what
							(:ink (lambda (old) (logior (logand board_pick_val 0xffffff) (if (board-none? old) 0xff000000 (logand old 0xff000000)))))
							(:alpha (lambda (old) (if (board-none? (defq c (logior (logand old 0xffffff) (<< board_pick_val 24)))) 0 c)))
							(:t (lambda (old) 0)))))
					;the pen it holds is filled, Chris: "that value become the
					;ink in the current pen/marker"
					((num? at) (. this :fill_pen at ink))
					((. this :hold event :color ink)))))
		(. this :touch +board_dirty_overlay)
		this)

	(defmethod :hold (event key val)
		; (. board :hold event key val) -> board
		;the slot of what pressed has it, each role of the press, (ptr-roles),
		;or no device, the board's own
		(if event
			(progn (bind '(id kind buttons &ignore) event)
				(. (get :bindings this) :set kind (ptr-device id) buttons key val))
			(def this key val))
		this)

	(defmethod :fill_pen (at ink)
		; (. board :fill_pen at ink) -> board
		;a pen has another ink, every slot that holds it draws with it, and
		;what shows the pens is told, :on_pens
		(elem-set (get :pens this) at ink)
		(if (defq on_pens (get :on_pens this)) (on_pens))
		this)

	(defmethod :switch_of (what key)
		; (. board :switch_of what key) -> :t | :nil
		;is a switch, :snap :snap_angle or :keep_shape, on for a device bound
		;to what: its own if it has one, else the board's, as it draws
		(if (defq at (ptr-at what key))
			(elem-get what at)
			(case key
				(:snap (/= (get :snap this) 0.0))
				(:snap_angle (/= (get :snap_angle this) 0.0))
				(:t (get :keep_shape this)))))

	(defmethod :snap_step (&optional id)
		; (. board :snap_step [id]) -> step
		(if (defq kept (. (get :settings this) :find id)) (first kept) (get :snap this)))

	(defmethod :angle_step (&optional id)
		; (. board :angle_step [id]) -> step
		(if (defq kept (. (get :settings this) :find id)) (second kept) (get :snap_angle this)))

	(defmethod :keep_shape? (&optional id)
		; (. board :keep_shape? [id]) -> :t | :nil
		(if (defq kept (. (get :settings this) :find id)) (third kept) (get :keep_shape this)))

	(defmethod :snap_x (x &optional id) (board-snap x (. this :snap_step id)))
	(defmethod :snap_y (y &optional id) (board-snap y (. this :snap_step id)))

	(defmethod :snap_end (ox oy x y &optional id)
		; (. board :snap_end ox oy x y [id]) -> (x y)
		;the end of a line from ox oy, as the pointer at x y has it. With
		;angles that snap it is at a step from ox oy, as long as the pointer
		;is far. With the grid too the grid comes first, Chris: "snap to grid
		;has to to priority": the end is on the grid, and of a square grid
		;only every 45 can be had, so the step is 45 and the end the point of
		;the grid along it nearest the pointer
		(defq grid (. this :snap_step id) step (. this :angle_step id))
		(cond
			((= step 0.0) (list (board-snap x grid) (board-snap y grid)))
			((= grid 0.0)
				(defq vx (- x ox) vy (- y oy) len (sqrt (+ (* vx vx) (* vy vy)))
					angle (board-snap (path-angle vx vy) step))
				(list (+ ox (* len (cos angle))) (+ oy (* len (sin angle)))))
			(:t (defq angle (board-snap (path-angle (- x ox) (- y oy)) (const (/ +fp_pi 4.0)))
					dx (board-snap (cos angle) 1.0) dy (board-snap (sin angle) 1.0)
					along (/ (+ (* (- x ox) dx) (* (- y oy) dy)) (+ (* dx dx) (* dy dy)))
					k (board-snap along grid))
				(list (+ ox (* k dx)) (+ oy (* k dy))))))

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
		;the layers as they are, and the size of the paper, to go back to
		(defq doc (get :doc this))
		(list (map (lambda ((name flags items)) (list name flags (board-copy-items items)))
			(cwb-layers doc)) (. doc :find :width) (. doc :find :height)))

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
		(bind '(layers w h) state)
		(.-> (get :doc this) (:insert :layers layers) (:insert :width w) (:insert :height h))
		(. this :changed :all)
		;an undo is of the document, strong groups and all, not of the
		;instruments, which are not in it, Chris: "undo/redo do not and
		;should not undo instrument or selection controls". What was
		;selected does not outlast it: everything is put down, every user's
		(. this :select (list))
		(def this :floating (list) :appended (list))
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
		;who picked up what is kept only for what is selected still
		(def this :selected (cat ids))
		(defq selectors (get :selectors this) gone (list))
		(. selectors :each (lambda (id &ignore) (unless (find id ids) (push gone id))))
		(each (# (. selectors :erase %0)) gone)
		(. this :touch +board_dirty_overlay))

	(defmethod :toggle_select (id user)
		; (. board :toggle_select id user) -> board
		;a thing in or out of select mode, by a tap. It is that user's
		;selection, an action on a selection is on a user's own, see
		;(:selected_by)
		(if (find id (get :selected this))
			(. this :select (filter (# (/= %0 id)) (get :selected this)))
			(. this :pick_up id user)))

	;who picked up what: the selected things, each with the user that holds
	;it, or no one's, a box's, which is anyone's. Kept only by these and
	;(:select), so the two can not differ
	(defmethod :holder (id)
		; (. board :holder id) -> :nil | user
		(. (get :selectors this) :find id))

	(defmethod :pick_up (id user)
		; (. board :pick_up id user) -> board
		;selected, and that user's
		(unless (find id (get :selected this)) (. this :select (cat (get :selected this) (list id))))
		(. (get :selectors this) :insert id user)
		this)

	(defmethod :others_of (user)
		; (. board :others_of user) -> ids
		;what users other than that one hold
		(filter (# (and (defq who (. this :holder %0)) (not (eql who user)))) (get :selected this)))

	(defmethod :selected_by (user)
		; (. board :selected_by user) -> ids
		;what that user put in select mode by a tap. Chris: an action on a
		;selection is on "the user's own selections". A thing selected by
		;no tap, a hand or a box, says no one, and is anyone's
		(filter (# (or (not (defq who (. this :holder %0))) (eql who user))) (get :selected this)))

	(defmethod :get_selected () (get :selected this))

	(defmethod :selected_items (&optional of)
		; (. board :selected_items [ids]) -> items
		;the items that are selected, or of those ids that are, each one of a
		;layer's own, in the order they are in, the back one first. What is
		;done to a selection is done to those of one user, Chris: "2 users
		;can select seperate item", what a press gives, (:selected_by)
		(defq ids (cwb-id-set (if of (filter (# (find %0 of)) (get :selected this)) (get :selected this))))
		(reduce (lambda (out (name flags items)) (cat out (cwb-pick items ids)))
			(cwb-layers (get :doc this)) (list)))

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

	(defmethod :ink_pair (event)
		; (. board :ink_pair event) -> (outline fill)
		;the inks of the pens of the device's main and second buttons. Touch
		;has one pen, both
		(bind '(id kind &ignore) event)
		(if (eql kind :touch)
			(list (defq c (. this :ink_of event)) c)
			(map (# (. this :ink_of (ptr-event id kind %0 0 0))) (list +pev_left +pev_right))))

	(defmethod :pen_of (event)
		; (. board :pen_of event) -> :nil | num
		;the number of the pen the slot of the pointer holds, its own or the
		;board's, :nil if it holds an ink of its own or there are no pens
		(when (get :pens this)
			(defq what (ifn (. (get :bindings this) :find event) (list)))
			(cond
				((defq at (ptr-at what :pen)) (elem-get what at))
				((ptr-at what :color) :nil)
				((get :pen this)))))

	(defmethod :ink_of (event)
		; (. board :ink_of event) -> ink
		;the ink a pointer draws with: of the pen its slot holds, Chris: "We
		;have a limited number of pens/marker ... We also have the ability to
		;fill those pens with any ink"; or an ink its slot holds itself, as a
		;script sets one; or the board's
		(if (num? (defq pen (. this :pen_of event)))
			(elem-get (get :pens this) pen)
			(. (get :bindings this) :value event :color (get :color this))))

	(defmethod :new_shape (event tool)
		; (. board :new_shape event tool) -> shape
		;a shape for a pointer to draw, with the colour and width that
		;pointer is bound to, or the board's. It is drawn over the
		;document till it is kept
		(defq bindings (get :bindings this)
			color (. this :ink_of event)
			width (. bindings :value event :width (get :width this))
			shape (cwb-shape "" :stroke color :width width :kind tool))
		(case tool
			;filled, the outline is the ink of the pen of the device's main
			;button and the fill that of its second, whichever drew it, Chris:
			;"we use the left button bound pen to outline and the right button
			;bound pen to fill". Touch has one pen, it fills. One ink for both,
			;a pen bound by the middle of the mouse, is a fill only, as far out
			;as the outline would be, (:shape_d): a see through outline over a
			;fill of the same ink is a band darker than either, Chris:
			;"transparent colours need a little extra thought there"
			;An ink that is wholly see through is none: no outline, the fill
			;is inside it; no fill, the outline is all
			((:frect :fellipse)
				(bind '(line fill) (. this :ink_pair event))
				(cwb-set shape :join :miter :fill (if (board-none? fill) 0 fill)
					:stroke (if (or (= line fill) (board-none? line)) 0 line)))
			((:rect :ellipse) (cwb-set shape :fill 0 :join :miter))
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

	(defmethod :commit (item &optional user)
		; (. board :commit item [user]) -> item
		;put an item on the layer that is drawn on, as a step that can be
		;undone, made by that user, the teacher's if not said
		(if user (elem-set item +cwb_user user))
		(. this :snapshot)
		(cwb-add (get :doc this) item (. this :layer_index))
		(. this :changed (list (elem-get item +cwb_id)))
		;on the top layer it is on top of all there is, and can be drawn there
		(if (= (. this :layer_index) (dec (length (cwb-layers (get :doc this)))))
			(progn (push (get :appended this) item)
				(. this :touch (+ +board_dirty_append +board_dirty_overlay)))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		item)

	(defmethod :written (item who began)
		; (. board :written item who began) -> board
		;a line drawn by hand has just been kept, on top of its layer, by
		;that pointer, begun at that time. If the last such was ended a
		;moment before, and is still under it, the two are a group, or it
		;joins the group the last was put in, if that has not been moved.
		;It is part of the step of keeping the line, and nothing is seen
		;to change: they are where they were, in the order they were
		(defq now (get :time this) before (get :last_stroke this) window (get :auto_group this)
			items (cwb-items (get :doc this) (. this :layer_index)) target (elem-get item +cwb_id))
		(when (and before (/= now 0) (> window 0) (= (elem-get before 2) who)
				(<= (- began (elem-get before 0)) window) (>= (length items) 2)
				(eql (elem-get items -2) item)
				(= (elem-get (defq under (elem-get items -3)) +cwb_id) (elem-get before 1)))
			(cond
				((and (cwb-group? under) (eql (elem-get under +cwb_name) "written")
						(or (not (defq gm (elem-get under +cwb_m))) (every (const =) gm (Mat3x2))))
					(pop items)
					(push (elem-get under +cwb_items) item)
					(setq target (elem-get under +cwb_id)))
				((cwb-shape? under)
					(pop items) (pop items)
					(push items (defq group (cwb-adopt (get :doc this) (cwb-group (list under item) :name "written"))))
					(setq target (elem-get group +cwb_id))))
			(. this :changed (list target)))
		(def this :last_stroke (list now target who))
		this)

	(defmethod :add (item)
		; (. board :add item) -> id
		;as (:commit), and the id of it, for a script
		(elem-get (. this :commit item) +cwb_id))

	(defmethod :remove (ids)
		; (. board :remove ids) -> board
		;take items out, no step is made, see (:delete)
		(cwb-remove-ids (get :doc this) ids)
		(defq gone (cwb-id-set ids))
		(. this :select (filter (# (not (cwb-id? gone %0))) (get :selected this)))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :rub (x y &optional r lines)
		; (. board :rub x y [r lines]) -> :t | :nil
		;rub out at a point, as the eraser does, r is how far it reaches,
		;default the board's by its zoom. Of a line that is near, the part
		;that is within r goes, and what is left of it is lines in its
		;place, still lines. Anything else it touches, a box, a filled
		;shape, words, has a bite taken out of it, (:bite). That is
		;:rub_mode :part. With :lines only a line is
		;cut, and if none was touched the thing on top at the point goes
		;whole. With :whole a line goes whole too. With :bite a line has a
		;bite out as the rest have, and is a line no more. With lines :t
		;only the lines are cut, whatever the mode, (:rub_along) takes the
		;bite of all its way at once. No step is made. Was anything
		;rubbed out
		(defq doc (get :doc this) r (n2f (ifn r (/ (get :rub_reach this) (get :zoom this))))
			x (n2f x) y (n2f y) reach (list (- x r) (- y r) (+ x r) (+ y r)) done :nil
			mode (get :rub_mode this))
		;a bite is another thing altogether
		(and (eql mode :bite) (not lines) (setq done (. this :bite (path x y) r)))
		(when (find mode '(:part :lines))
			(each (lambda ((name flags items))
				(unless (or (bits? flags 1) (bits? flags 2))
					(defq out (list) changed :nil)
					(each (lambda (item)
						(cond
							((and (cwb-rub? item)
									(defq box (cwb-bounds (list item)))
									(vector-collide-boxes-2d reach (list (- (first box) 1.0) (- (second box) 1.0)
										(+ (third box) 1.0) (+ (elem-get box 3) 1.0)))
									(defq pieces (cwb-rub item x y r)))
								(setq changed :t done :t)
								(. this :select (filter (# (/= %0 (elem-get item +cwb_id))) (get :selected this)))
								(each (# (push out (cwb-adopt doc %0))) pieces)
								(. this :changed (map (# (elem-get %0 +cwb_id)) pieces)))
							;a group: the lines in it are rubbed as lines are, in its
							;own space, so a word that was written is rubbed a letter
							;at a time, not taken whole. A group with nothing left goes
							((and (cwb-group? item)
									(defq box (cwb-bounds (list item)))
									(vector-collide-boxes-2d reach (list (- (first box) 1.0) (- (second box) 1.0)
										(+ (third box) 1.0) (+ (elem-get box 3) 1.0)))
									(defq left (board-rub-group doc item x y r)))
								(setq changed :t done :t)
								(cond
									((empty? left)
										(. this :select (filter (# (/= %0 (elem-get item +cwb_id))) (get :selected this))))
									(:t (elem-set item +cwb_items left)
										(. this :changed (list (elem-get item +cwb_id)))
										(push out item))))
							(:t (push out item))))
						items)
					(when changed
						;what is left is where it was, among the rest
						(clear items)
						(each (# (push items %0)) out))))
				(cwb-layers doc)))
		;what is not a line has a bite out
		(and (eql mode :part) (not lines) (. this :bite (path x y) r) (setq done :t))
		(when (and (not done) (not lines) (find mode '(:lines :whole))
				(defq hit (cwb-hit doc x y (/ +board_near (get :zoom this)))))
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
		(defq mode (get :rub_mode this))
		(unless (eql mode :bite)
			;the lines, a step at a time. And with them what is not a line,
			;if it goes whole
			(each (lambda (i)
				(defq f (/ (n2f i) (n2f steps)))
				(if (. this :rub (+ x (* dx f)) (+ y (* dy f)) r (eql mode :part)) (setq done :t)))
				(range 0 (inc steps))))
		;the bite, all of the way at once, what it covers from the one to the other
		(and (find mode '(:part :bite)) (. this :bite (path x y (n2f x1) (n2f y1)) r) (setq done :t))
		done)

	(defmethod :bite (points &optional r shape user)
		; (. board :bite points [r shape user]) -> :t | :nil
		;what an eraser covers as it goes along a line, x y x y ..., taken
		;out of every thing it touches, each of which is then flat: a
		;filled outline of what was left of it. The eraser is a path about
		;0 0 with no dent in it, default a circle of r, default the
		;board's reach by its zoom. No step is made. Was anything touched
		(defq r (n2f (ifn r (/ (get :rub_reach this) (get :zoom this)))))
		(. this :bite_area (poly-sweep (ifn shape (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) points) user))

	(defmethod :bite_area (swept &optional user)
		; (. board :bite_area swept [user]) -> :t | :nil
		;what is in the polygons swept, the board's space, taken out of every
		;thing they touch, as (:bite) does with what an eraser covers. No step
		;is made. Was anything touched
		;a line is left to be cut as a line unless all is to be bitten
		;What a hand has hold of is bitten as anything is, and what is left
		;of it is in the hand still, and selected still: a thing can be
		;held, and turned, and shaped with the eraser as it is. Chris: "I
		;had the shape/grouop you are holding can be erased by yourself !
		;That was a nice feeling UI, you could pick up and hold somthing,
		;turn it and shape it with the eraser... that REALLY impressed
		;people !" Whose eraser it is is not known, a pen and a finger
		;are two devices and may be one person, so anybody's does
		;A user's eraser leaves what another made, the teacher's takes from
		;all, see (board-bite)
		(when (and (nempty? swept) (defq did (board-bite (get :doc this) swept (not (eql (get :rub_mode this) :bite))
				:nil (/ 0.25 (get :zoom this)) user)))
			(bind '(made gone swaps) did)
			;what is left of a thing picked up is held by who held it
			(defq carried (list))
			(. this :select (reduce (lambda (out id)
					(cond
						((defq swap (some (# (if (= (first %0) id) %0)) swaps))
							(defq who (. this :holder id))
							(each (# (push out (elem-get %0 +cwb_id)) (if who (push carried (list (elem-get %0 +cwb_id) who)))) (second swap)))
						((find id gone))
						(:t (push out id)))
					out) (get :selected this) (list)))
			(each (lambda ((id who)) (. this :pick_up id who)) carried)
			(. (get :things this) :swapped swaps)
			(. this :changed made)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay))
			:t))

	(defmethod :delete (&optional ids)
		; (. board :delete [ids]) -> board
		;take out what is selected, or of those ids
		(when (nempty? (defq ids (map (# (elem-get %0 +cwb_id)) (. this :selected_items ids))))
			(. this :snapshot)
			(. this :remove ids))
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
					(unless (and (defq box (cwb-bounds (list item))) (vector-collide-boxes-2d paper box))
						(push gone (elem-get item +cwb_id))))
					items)))
			(cwb-layers doc))
		(when (nempty? gone)
			(. this :snapshot)
			(. this :remove gone)
			(. this :changed :all))
		(length gone))

	(defmethod :new (w h)
		; (. board :new width height) -> board
		;an empty board of that size, as a step that can be undone: an
		;hour's work is not lost to one press in the wrong place
		(. this :clear)
		(.-> (get :doc this) (:insert :width w) (:insert :height h))
		this)

	(defmethod :resize (w h)
		; (. board :resize width height) -> board
		;the paper is that size, what is on it stays, and it can be undone
		(defq doc (get :doc this))
		(unless (and (= w (. doc :find :width)) (= h (. doc :find :height)))
			(. this :snapshot)
			(.-> doc (:insert :width w) (:insert :height h))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :clear ()
		; (. board :clear) -> board
		;take out everything
		(. this :snapshot)
		(. this :changed :all)
		(each (lambda (layer) (elem-set layer 2 (list))) (cwb-layers (get :doc this)))
		(. this :select (list))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :transform (m)
		; (. board :transform m) -> board
		;move what is selected by a matrix
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(each (# (elem-set %0 +cwb_m (Mat3x2-mul m (elem-get %0 +cwb_m)))) items)
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

	(defmethod :style_ids (ids &rest key_vals)
		; (. board :style_ids ids [key val] ...) -> board
		;as (:style), of those selected things only
		(defq items (filter (# (find (elem-get %0 +cwb_id) ids)) (. this :selected_items)))
		(when (nempty? items)
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(cwb-walk items :nil (lambda (shape m) (apply (const cwb-set) (cat (list shape) key_vals))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :paint (ids key color &optional with)
		; (. board :paint ids key color [with]) -> board
		;the outline, :stroke, the fill, :fill, both, :both, or what a
		;thing shows, :ink, its fill if it has no outline, of those selected
		;things, all that is selected if ids is :t. The ink is as it is, an
		;argb, none is 0, or with a function, (with old) -> ink, made from
		;what each was. Words have only a fill, their colour. See
		;(ptr-part)
		(defq items (if (eql ids :t) (. this :selected_items)
			(filter (# (find (elem-get %0 +cwb_id) ids)) (. this :selected_items))))
		(when (nempty? items)
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(cwb-walk items :nil (lambda (shape m)
				(defq part (cond
					((eql (elem-get shape +cwb_kind) :text) :fill)
					((eql key :ink) (if (and (= (ifn (cwb-get shape :stroke) 0) 0) (/= (ifn (cwb-get shape :fill) 0) 0)) :fill :stroke))
					(key)))
				(if (find part '(:fill :both)) (cwb-set shape :fill (if with (with (ifn (cwb-get shape :fill) 0)) color)))
				(if (find part '(:stroke :both)) (cwb-set shape :stroke (if with (with (ifn (cwb-get shape :stroke) 0)) color)))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :group (&optional ids)
		; (. board :group [ids]) -> :nil | id
		;what is selected, or of those ids, becomes one group, where the top
		;one of them was
		(defq items (. this :selected_items ids))
		(when (> (length items) 1)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) top (last items) top_id (elem-get top +cwb_id)
				holder (third (cwb-find doc top_id))
				group (cwb-adopt doc (cwb-group items))
				kept (map (# (if (= (elem-get %0 +cwb_id) top_id) group %0)) holder))
			(clear holder)
			(each (# (push holder %0)) kept)
			(cwb-remove-ids doc (filter (# (/= %0 top_id)) (map (# (elem-get %0 +cwb_id)) items)))
			;the group is in the list, the items are now only in the group,
			;held by who held the top of them
			(defq who (. this :holder top_id) parts (map (# (elem-get %0 +cwb_id)) items))
			(. this :select (cat (filter (# (not (find %0 parts))) (get :selected this)) (list (elem-get group +cwb_id))))
			(if who (. this :pick_up (elem-get group +cwb_id) who))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay))
			(elem-get group +cwb_id)))

	(defmethod :ungroup (&optional ids)
		; (. board :ungroup [ids]) -> board
		;each group that is selected, or of those ids, becomes its items,
		;where it was, each where it was on the board
		(defq groups (filter (const cwb-group?) (. this :selected_items ids)))
		(when (nempty? groups)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) ids (list) carried (list))
			(each (lambda (group)
				(defq id (elem-get group +cwb_id) who (. this :holder id) holder (third (cwb-find doc id))
					kept (reduce (lambda (out item)
						(if (= (elem-get item +cwb_id) id)
							(each (lambda (kid)
								(elem-set kid +cwb_m (Mat3x2-mul (elem-get group +cwb_m) (elem-get kid +cwb_m)))
								(push ids (elem-get kid +cwb_id))
								(if who (push carried (list (elem-get kid +cwb_id) who)))
								(push out kid)) (elem-get group +cwb_items))
							(push out item))
						out) holder (list)))
				(clear holder)
				(each (# (push holder %0)) kept))
				groups)
			(. this :select (cat ids (filter (# (not (find %0 (map (# (elem-get %0 +cwb_id)) groups))))
				(get :selected this))))
			;the items of a group are held by who held it
			(each (lambda ((id who)) (. this :pick_up id who)) carried)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :order (front &optional of)
		; (. board :order front [ids]) -> board
		;what is selected, or of those ids, goes to the front of its layer,
		;or the back
		(when (nempty? (defq ids (map (# (elem-get %0 +cwb_id)) (. this :selected_items of))))
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

	(defmethod :align (how &optional ids)
		; (. board :align how [ids]) -> board
		;line up what is selected, or of those ids, each with the box round
		;all of it. how is :left :right :top :bottom, or :hcenter or :vcenter
		;for the middles
		(defq items (. this :selected_items ids))
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
					(elem-set item +cwb_m (Mat3x2-mul (Mat3x2-translate dx dy) (elem-get item +cwb_m)))))
				items)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :duplicate (&optional dx dy of)
		; (. board :duplicate [dx dy ids]) -> board
		;a copy of what is selected, or of those ids, moved a little, default
		;16 each way, and the copy is selected in its place
		(when (nempty? (defq items (. this :selected_items of)))
			(. this :snapshot)
			(defq doc (get :doc this) move (Mat3x2-translate (ifn dx 16) (ifn dy 16)) ids (list)
				whos (map (# (. this :holder (elem-get %0 +cwb_id))) items))
			(each (lambda (copy)
				;a copy and all in it has ids of its own
				(defq todo (list copy))
				(while (defq next (pop todo))
					(elem-set next +cwb_id 0)
					(if (cwb-group? next) (each (# (push todo %0)) (elem-get next +cwb_items))))
				(elem-set copy +cwb_m (Mat3x2-mul move (elem-get copy +cwb_m)))
				(cwb-add doc copy (. this :layer_index))
				(push ids (elem-get copy +cwb_id)))
				(board-copy-items items))
			;the copies are held by who held each, in their place
			(defq originals (map (# (elem-get %0 +cwb_id)) items))
			(. this :select (cat (filter (# (not (find %0 originals))) (get :selected this)) ids))
			(each (lambda (id who) (if who (. this :pick_up id who))) ids whos)
			(. this :changed ids)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	;;;;;;;;;;
	; pointers
	;;;;;;;;;;

	(defmethod :busy? ()
		; (. board :busy?) -> :t | :nil
		;is anybody drawing: a line not yet done, along an instrument or
		;free, or the eraser going. Many work at a board at once, and what
		;is done to all of it, a move of the view, waits for that. Chris:
		;"lock the view while anyone is drawing, that onlt applie to the
		;global actions !" It does not wait for a hand that holds a thing:
		;"If I hold a shape, and somone else moves the canvas, my shape
		;comes with me ! It is 'in flight'", what shows the board sees to
		;that
		(some (# (. %0 :drawing?)) (. (get :stage this) :get_actors)))

	(defmethod :reach (&optional width)
		; (. board :reach [width]) -> r
		;how far an eraser reaches, in the document, for a pointer that
		;draws that wide, default the board's width: three times the
		;width, so 9, 18 and 36 for the three widths of the app, and not
		;less than :rub_reach. It is that many pixels on the screen
		;whatever the zoom, as an instrument is the size it is
		(/ (max (n2f (get :rub_reach this)) (* 3.0 (n2f (ifn width (get :width this))))) (get :zoom this)))

	(defmethod :pointer_free? (id)
		; (. board :pointer_free? id) -> :t | :nil
		;is a pointer down on the board on nothing: it has the hand, and
		;no thing in it. It is dragging a box out, or may be a tap
		(and (defq state (. (get :surface this) :state id))
			(eql (elem-get state +stroke_tool) :hand) (not (elem-get state +stroke_item))))

	(defmethod :pointer_idle (id)
		; (. board :pointer_idle id) -> board
		;what a pointer that is down on the board was doing is given up:
		;its box is not dragged out, and when it comes up it was no tap.
		;What shows the board says so when it takes two fingers that are
		;on nothing to move its view by
		(when (defq state (. (get :surface this) :state id))
			(elem-set state +stroke_tool :idle)
			(. (get :bands this) :erase id)
			(. this :touch +board_dirty_overlay))
		this)

	(defmethod :pointers (events)
		; (. board :pointers events) -> board
		;a batch of pointer events, each in the space of the document
		;where each finger that is down is, is kept, to be shown, (:draw_actors)
		(defq contacts (get :contacts this) seen :nil)
		(each (lambda (event)
			(when (eql (elem-get event +pev_kind) :touch)
				(setq seen :t)
				(if (ptr-down? event)
					(. contacts :insert (elem-get event +pev_id) (list (elem-get event +pev_x) (elem-get event +pev_y)))
					(. contacts :erase (elem-get event +pev_id)))))
			events)
		(and seen (get :show_contacts this) (. this :touch +board_dirty_overlay))
		;what each pointer that goes down is bound to snap and keep, kept
		;till it comes up
		(defq settings (get :settings this))
		(each (lambda (event)
				(and (ptr-down? event) (not (. settings :find (elem-get event +pev_id)))
					(. settings :insert (elem-get event +pev_id) (. this :pointer_settings event))))
			events)
		(. (get :stage this) :pointers events)
		(each (# (unless (ptr-down? %0) (. settings :erase (elem-get %0 +pev_id)))) events)
		(. this :hovering events)
		this)

	(defmethod :hovering (events)
		; (. board :hovering events) -> board
		;a pen or a mouse with nothing held is near, not down: its kind
		;and where it is are kept. If it went down where it is, and was
		;had by an instrument, held to an edge of it, to draw along that
		;edge, then the catch of that edge is shown, (:draw_actors), and
		;not till then. :hover_shown is those, instrument edge ... A
		;finger is never near, it is down or it is not there
		(defq hovers (get :hovers this) was (get :hover_shown this) now (list)
			stage (get :stage this) bindings (get :bindings this))
		(each (lambda (event)
			(unless (eql (elem-get event +pev_kind) :touch)
				(if (ptr-down? event)
					(. hovers :erase (elem-get event +pev_id))
					(. hovers :insert (elem-get event +pev_id)
						(list (elem-get event +pev_kind) (elem-get event +pev_x) (elem-get event +pev_y))))))
			events)
		(when (get :hover_edges this)
			(. hovers :each (lambda (id (kind x y))
				;the one it would go down on, the one in front there: what
				;goes down by an edge follows it, to draw or to rub out
				(when (and (defq actor (. stage :hit x y)) (def? :edges actor))
					(bind '(lx ly) (. actor :to_own x y))
					(when (defq near (. actor :edge_near lx ly))
						(defq edge (first near))
						(unless (some (lambda ((a e)) (and (eql a actor) (eql e edge))) (partition now 2))
							(push now actor edge)))))))
		(unless (and (= (length now) (length was)) (every (const eql) now was))
			(def this :hover_shown now)
			(. this :touch +board_dirty_overlay))
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

	(defmethod :draw_actors (canvas &optional m pick)
		; (. board :draw_actors canvas [m pick]) -> board
		;what is on the stage and not of the document, a ruler, a palette,
		;the back one first. After (:draw_overlay), it is over that. With
		;pick, a function of an actor, only those it says: an app that has
		;some of them drawn another way, the Whiteboard its instruments
		(each (# (if (or (not pick) (pick %0)) (. %0 :draw canvas m))) (. (get :stage this) :get_actors))
		;the catch of an edge that would have a pen that is near, if it went down
		(each (lambda ((actor edge))
			(and (find actor (. (get :stage this) :get_actors)) (defq shape (. actor :catch_shape edge))
				(cwb-draw-items canvas (list shape) (Mat3x2-mul m (. actor :matrix)))))
			(partition (get :hover_shown this) 2))
		;and over them all, a ring where each finger is on the board,
		;:show_contacts. A finger hides what is under it on a screen, and
		;is not on the screen at all on a pad beside it: this is where the
		;board has it
		(when (get :show_contacts this)
			(defq rings (list) r (/ 16.0 (get :zoom this)) thin (/ 2.0 (get :zoom this)))
			(. (get :contacts this) :each (lambda (id (x y))
				(push rings (cwb-shape (cwb-d-ellipse x y r) :stroke 0xc0ff4000 :width thin :fill 0x30ff4000)
					(cwb-shape (cwb-d-ellipse x y (* r 0.15)) :fill 0xc0ff4000 :stroke 0))))
			(if (nempty? rings) (cwb-draw-items canvas rings m)))
		this)

	;;;;;;;;;
	; drawing
	;;;;;;;;;

	(defmethod :draw (canvas &optional m clip)
		; (. board :draw canvas [m clip]) -> count
		;the document, on a canvas, by the matrix it is seen by
		(cwb-draw canvas (get :doc this) m clip (get :floating this)))

	(defmethod :in_flight? ()
		; (. board :in_flight?) -> :t | :nil
		;is anything in flight: being moved about, or being drawn
		(or (nempty? (get :floating this)) (> (. (get :temp this) :size) 0)
			(and (defq remote (get :remote_temp this)) (> (. remote :size) 0))))

	(defmethod :draw_flight (canvas &optional m side)
		; (. board :draw_flight canvas [m side]) -> board
		;what is in flight and nothing else: the items being moved about,
		;and what is being drawn and not yet kept. Of one side, :front or
		;:back, if said: what is being drawn is in front
		(defq floating (get :floating this) back (cwb-id-set (get :back_ids this)))
		(case side
			(:front (setq floating (filter (# (not (cwb-id? back %0))) floating)))
			(:back (setq floating (filter (# (cwb-id? back %0)) floating))))
		(when (nempty? floating)
			(setq floating (cwb-id-set floating))
			(each (lambda ((name flags items))
				(cwb-draw-items canvas (cwb-pick items floating) m))
				(cwb-layers (get :doc this))))
		(unless (eql side :back)
			(. (get :temp this) :each (lambda (id shape) (cwb-draw-items canvas (list shape) m)))
			;what is being drawn on the boards this is shared with
			(if (defq remote (get :remote_temp this))
				(. remote :each (lambda (k (from shape)) (cwb-draw-items canvas (list shape) m)))))
		this)

	(defmethod :draw_overlay (canvas &optional m no_flight no_selected)
		; (. board :draw_overlay canvas [m no_flight no_selected]) -> board
		;what is over the document and not of it: what is in flight,
		;the box being dragged out, and the handles of what is selected.
		;With no_selected the last is left out, whoever shows the board
		;draws it where what is selected is, (:draw_selected)
		(defq zoom (get :zoom this) thin (/ 1.0 zoom))
		;what is in flight, unless it is not wanted here: whoever shows the
		;board may have a picture of its own for it, to put in front of
		;the document or behind
		(unless no_flight (. this :draw_flight canvas m))
		(when (defq rubber (get :rubber this))
			;the eraser, where it is, as far as it reaches
			(bind '(x y r) rubber)
			(cwb-draw-items canvas (list (board-rub-look (list (path-gen-arc x y 0.0 +fp_2pi r (path))) zoom)) m))
		;a box that is being dragged out, each pointer's own
		(. (get :bands this) :each (lambda (id (x y x1 y1))
			(cwb-draw-items canvas (list (cwb-shape (cwb-d-rect x y x1 y1)
				:stroke 0xff0080ff :width thin :fill 0x200080ff :join :miter)) m)))
		(unless no_selected (. this :draw_selected canvas m))
		this)

	(defmethod :draw_selected (canvas &optional m)
		; (. board :draw_selected canvas [m]) -> board
		;the lines round what is selected, and its handles. They go with
		;what they are of: a thing taken to the back has its box and its
		;handles at the back with it, behind what it is behind, and is
		;still turned and sized by them there
		(defq zoom (get :zoom this) thin (/ 1.0 zoom))
		(when (nempty? (defq items (. this :selected_items)))
			(defq handles (get :handles this))
			;of several things selected, each has a line round it, its own
			;box, so that it is seen they are several and which they are: a
			;group that was broken up is its parts, each one selected.
			;They are drawn 32 boxes to a shape: a shape each is a
			;thousand shapes, and one shape of them all, once they are
			;turned and lie over one another, is a thousand edges on
			;every row for the fill to keep in order, six times as long.
			;While a hand moves or turns a great many of them they are
			;not drawn, and are again when it lets go
			(defq each_one (and (> (length items) 1)
					(or (< (length items) 200)
						(not (or (. (get :things this) :holding) (/= (. handles :held) 0))))))
			(if each_one
				(cwb-draw-items canvas (map (# (cwb-shape (join %0 " ") :stroke 0xa00080ff :width thin :join :miter))
					(partition (reduce (lambda (out item)
						(if (defq corners (board-item-corners item))
							(push out (cwb-d-points corners :t)))
						out) items (list)) 32)) m))
			(cond
				;a line has only its two ends
				((. handles :ends))
				;the frame the handles are on, which may be turned
				((defq corners (. handles :corners))
					(cwb-draw-items canvas (list (cwb-shape (cwb-d-points corners :t)
						:stroke 0xff0080ff :width thin :join :miter)) m))
				;while hands each hold things there are no handles. One thing
				;has its own box, turned as it is turned:
				;the box round that, square to the board, is a bigger and
				;looser one that does not turn with it and looks wrong
				((and (= (length items) 1) (defq corners (board-item-corners (first items))))
					(cwb-draw-items canvas (list (cwb-shape (cwb-d-points corners :t)
						:stroke 0xff0080ff :width thin :join :miter)) m))
				;several things, each in its own hand: each has its own line
				;round it, and that is all. A box round them all says they
				;are one thing to take hold of, and they are not, each may
				;be in a different hand. Only when the lines round each are
				;left out, a great many being moved, is there the one box
				((and (not each_one) (defq box (cwb-bounds items)))
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
