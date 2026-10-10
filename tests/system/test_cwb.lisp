(report-header "Whiteboard: a document of shapes, the file of it, and a board that is driven by pointers")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/board.inc")

(defun wb-near? (a b &optional tol)
	;two numbers, or two lists of them, the same to within tol, default 0.01
	(setd tol 0.01)
	(if (list?? a)
		(and (list?? b) (= (length a) (length b)) (every (# (<= (abs (- (n2f %0) (n2f %1))) tol)) a b))
		(<= (abs (- (n2f a) (n2f b))) tol)))

(defun wb-ids (doc)
	(map (# (elem-get %0 +cwb_id)) (cwb-items doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the angle of a vector, and the arc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;there was no way to ask the angle of a vector, and the A of an SVG path,
;an arc of an ellipse from one point to another, threw as not done.
;gui/path/lisp.inc has both now
(assert-true "the angle of a vector, round from the x axis to the y axis"
	(wb-near? (map (lambda ((x y)) (path-angle x y))
			'((1.0 0.0) (1.0 1.0) (0.0 1.0) (-1.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (300.0 400.0)))
		'(0.0 0.7854 1.5708 2.3562 3.1416 -1.5708 -0.7854 0.9273) 0.0002))
(assert-eq "of no vector at all, none" 0.0 (path-angle 0.0 0.0))

;from (100 0) to (0 100) with a radius of 100 there are four arcs, two
;circles and two ways round each, and the middle of each is known
(defun wb-arc-mid (large sweep)
	(defq pts (partition (path-gen-earc 100.0 0.0 100.0 100.0 0.0 large sweep 0.0 100.0 (path 100.0 0.0)) 2))
	(map (const identity) (elem-get pts (/ (length pts) 2))))
(assert-true "the small arc the way the angle grows" (wb-near? (wb-arc-mid 0.0 1.0) '(70.71 70.71) 0.05))
(assert-true "the small arc the other way, about the other centre" (wb-near? (wb-arc-mid 0.0 0.0) '(29.29 29.29) 0.05))
(assert-true "the large arc the way the angle grows" (wb-near? (wb-arc-mid 1.0 1.0) '(170.71 170.71) 0.05))
(assert-true "the large arc the other way" (wb-near? (wb-arc-mid 1.0 0.0) '(-70.71 -70.71) 0.05))
(defq wb_arc (path-gen-earc -500.0 0.0 500.0 500.0 0.0 0.0 1.0 500.0 0.0 (path -500.0 0.0)))
(assert-true "every point of half a circle of 500 is within a fiftieth of a pixel of it"
	(every (lambda ((x y)) (<= (abs (- 500.0 (sqrt (+ (* x x) (* y y))))) 0.02)) (partition wb_arc 2)))
(assert-list-eq "and it ends on the point it was to" '(500.0 0.0) (map (const identity) (slice wb_arc -3 -1)))
(assert-list-eq "radii too small to reach are made as big as is needed, the end is still the end" '(70.0 80.0)
	(map (const identity) (slice (path-gen-earc 0.0 0.0 5.0 6.0 0.5 1.0 0.0 70.0 80.0 (path 0.0 0.0)) -3 -1)))
(assert-list-eq "an arc of no radius is a line" '(0.0 0.0 7.0 8.0)
	(map (const identity) (path-gen-earc 0.0 0.0 0.0 6.0 0.0 1.0 0.0 7.0 8.0 (path 0.0 0.0))))
(defq wb_paths (path-gen-paths "M 100 0 A 100 100 0 0 1 0 100 a 100 100 0 0 1 -100 -100 Z"))
(assert-eq "a path with an A and an a in it is one closed line" 1 (length wb_paths))
(assert-true "that goes through the three points" (and (first (first wb_paths))
	(some (lambda ((x y)) (wb-near? (list x y) '(0.0 100.0))) (partition (second (first wb_paths)) 2))
	(some (lambda ((x y)) (wb-near? (list x y) '(-100.0 0.0))) (partition (second (first wb_paths)) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-true "a move, then twice the size about the origin"
	(wb-near? (cwb-mat-point (cwb-mat-mul (cwb-mat-scale 2) (cwb-mat-move 10 5)) 1 1) '(22.0 12.0)))
(assert-true "a quarter turn about a point" (wb-near? (cwb-mat-point (cwb-mat-turn +fp_hpi 10 10) 20 10) '(10.0 20.0) 0.001))
(defq wb_m (cwb-mat-mul (cwb-mat-turn 0.7 3 4) (cwb-mat-scale 2.5 0.5 1 1)))
(assert-true "a matrix and the one that undoes it"
	(wb-near? (apply (const cwb-mat-point) (cat (list (cwb-mat-invert wb_m)) (cwb-mat-point wb_m 12 -7))) '(12.0 -7.0) 0.001))
(assert-eq "no matrix and a matrix is the matrix" wb_m (cwb-mat-mul :nil wb_m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; paths of common shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-list-eq "a number in a path has no more of it than there is" '("10" "2.5" "-0.125" "0" "1000.75")
	(map (const cwb-num) (list 10 2.5 -0.125 0 1000.75)))
(assert-eq "a box" "M 10 20 L 110 20 110 70 10 70 Z" (cwb-d-rect 10 20 110 70))
(assert-eq "a box from its other corner is the same box" (cwb-d-rect 10 20 110 70) (cwb-d-rect 110 70 10 20))
(assert-eq "a line through points, closed" "M 0 0 L 10 0 10 10 Z" (cwb-d-points '(0 0 10 0 10 10) :t))
(assert-true "a box with round corners has four arcs" (= 4 (length (substr (cwb-d-rect 0 0 100 50 10) "A"))))
(assert-true "an arc all the way round is a circle" (eql (cwb-d-arc 5 5 3 0 7) (cwb-d-ellipse 5 5 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq wb_doc (cwb-doc 200 100)
	wb_a (cwb-add wb_doc (cwb-shape (cwb-d-rect 10 10 60 40) :fill 0xffff0000 :stroke 0xff000000 :width 4 :join :miter))
	wb_b (cwb-add wb_doc (cwb-shape (cwb-d-line 0 0 100 0) :m (cwb-mat-move 50 80)))
	wb_g (cwb-add wb_doc (cwb-group (list (cwb-shape (cwb-d-rect 0 0 10 10) :fill 0xff00ff00 :stroke 0)
		(cwb-group (list (cwb-shape (cwb-d-rect 20 0 30 10) :fill 0xff0000ff :stroke 0)) :name "inner"))
		:m (cwb-mat-move 150 50) :name "outer")))
(assert-list-eq "each item of a layer has an id" '(1 2 3) (wb-ids wb_doc))
(assert-list-eq "those in a group too, in the order they are in" '(4 5 6)
	(list (elem-get (first (cwb-get wb_g :items)) +cwb_id) (elem-get (second (cwb-get wb_g :items)) +cwb_id)
		(elem-get (first (cwb-get (second (cwb-get wb_g :items)) :items)) +cwb_id)))
(assert-list-eq "a shape is flattened to one polygon to fill and two for its edge" '(1 2)
	(map (const length) (most (cwb-flat wb_a))))
(assert-true "the box round it is its box and half its width more" (wb-near? (third (cwb-flat wb_a)) '(8 8 62 42)))
(assert-true "what it is flattened to is kept" (eql (cwb-flat wb_a) (cwb-flat wb_a)))
(cwb-set wb_a :name "a box")
(assert-true "and still is when only its name is set" (elem-get wb_a +cwb_cache))
(cwb-set wb_a :width 6)
(assert-eq "and is let go of when its width is" :nil (elem-get wb_a +cwb_cache))
(cwb-set wb_a :width 4)
(assert-true "the box round a group is where its matrix puts it" (wb-near? (cwb-bounds (list wb_g)) '(150 50 180 60)))
(assert-true "the box round everything" (wb-near? (cwb-bounds (cwb-items wb_doc)) '(8 8 180 81)))
(assert-eq "an item by its id, in a group in a group" "inner" (elem-get (first (cwb-find wb_doc 5)) +cwb_name))

;what is hit
(assert-eq "a point in the box is on the box" 1 (elem-get (first (cwb-hit wb_doc 30 20)) +cwb_id))
(assert-eq "a point on the line is on the line" 2 (elem-get (first (cwb-hit wb_doc 100 80.5)) +cwb_id))
(assert-eq "a point 4 from the line is not" :nil (cwb-hit wb_doc 100 84))
(assert-eq "unless 4 is near enough" 2 (elem-get (first (cwb-hit wb_doc 100 84 4)) +cwb_id))
(assert-list-eq "a point on a shape in a group is on the group, and the shape is said" '(3 6)
	(progn (defq wb_hit (cwb-hit wb_doc 175 55)) (list (elem-get (first wb_hit) +cwb_id) (elem-get (third wb_hit) +cwb_id))))
(assert-eq "a point on nothing" :nil (cwb-hit wb_doc 190 5))
(assert-list-eq "what is wholly in a box" '(2 3) (map (# (elem-get (first %0) +cwb_id)) (cwb-in-box wb_doc '(40.0 45.0 200.0 100.0))))

;a file
(defq wb_stream (string-stream (cat "")))
(cwb-save wb_doc wb_stream)
(defq wb_text (str wb_stream) wb_doc2 (cwb-load (string-stream wb_text)))
(assert-true "a document saved is text that says what it is" (and (found? wb_text ":version") (found? wb_text "M 10 10 L 60 10 60 40 10 40 Z")))
(assert-list-eq "loaded, it has the items it had" '(1 2 3) (wb-ids wb_doc2))
(assert-list-eq "and its size" '(200 100) (list (. wb_doc2 :find :width) (. wb_doc2 :find :height)))
(assert-true "the group in the group is where it was" (eql "inner" (elem-get (first (cwb-find wb_doc2 5)) +cwb_name)))
(assert-true "and everything is where it was" (wb-near? (cwb-bounds (cwb-items wb_doc2)) (cwb-bounds (cwb-items wb_doc))))
(defq wb_stream2 (string-stream (cat "")))
(cwb-save wb_doc2 wb_stream2)
(assert-true "saved again it is the same text" (eql wb_text (str wb_stream2)))
(assert-eq "an item added to it has the next id" 7 (elem-get (cwb-add wb_doc2 (cwb-shape (cwb-d-line 0 0 1 1))) +cwb_id))
(assert-eq "an item taken out is given back" 2 (elem-get (cwb-remove wb_doc2 2) +cwb_id))
(assert-list-eq "and is out" '(1 3 7) (wb-ids wb_doc2))

;a file written by hand, with no ids and only what matters said
(defq wb_hand (cwb-load (string-stream (cat
	"((:Emap 1) :version 4 :width 64 :height 48 :layers ((:list 1) ((:list 3) \q[mine]\q 0"
	" ((:list 2) ((:list 3) :shape :d \q[M 0 0 L 10 10]\q) ((:list 5) :shape :d \q[M 5 5 L 9 9]\q :width 1.0)))))"))))
(assert-true "a file written by hand loads" wb_hand)
(when wb_hand
	(assert-list-eq "its items are given ids" '(1 2) (wb-ids wb_hand))
	(assert-list-eq "what it did not say is what a shape has anyway" '(0 0xff000000 2.0)
		(list (cwb-get (first (cwb-items wb_hand)) :fill) (cwb-get (first (cwb-items wb_hand)) :stroke)
			(cwb-get (first (cwb-items wb_hand)) :width))))

;the file shown in docs/ai_digest/whiteboard.md is a file
(defq wb_page (list) wb_on :nil)
(lines! (lambda (line)
	(cond
		((starts-with "((:Emap 1)" line) (setq wb_on :t) (push wb_page line))
		((and wb_on (starts-with "```" line)) (setq wb_on :nil))
		(wb_on (push wb_page line))) :nil)
	(file-stream "docs/ai_digest/whiteboard.md"))
(defq wb_shown (cwb-load (string-stream (join wb_page (ascii-char 10)))))
(assert-true "the file shown in the page about this loads" wb_shown)
(when wb_shown
	(assert-list-eq "and is what the page says it is" '(640 420 (1 2) "M 40 360 L 220 360" "Start")
		(list (. wb_shown :find :width) (. wb_shown :find :height) (wb-ids wb_shown)
			(cwb-get (first (cwb-items wb_shown)) :d) (cwb-get (second (cwb-items wb_shown)) :text))))

;words are as big as they say, and can be put by their middle
(defq wb_w24 (cwb-bounds (list (cwb-text "Hxg" 0 0 :font_size 24))) wb_w48 (cwb-bounds (list (cwb-text "Hxg" 0 0 :font_size 48))))
(assert-true "words of 24 are about 24 from the top of an H to the bottom of a g"
	(wb-near? (- (elem-get wb_w24 3) (second wb_w24)) 23.0 2.0))
(assert-true "and words of 48 are twice that" (wb-near? (- (elem-get wb_w48 3) (second wb_w48)) (* 2.0 (- (elem-get wb_w24 3) (second wb_w24))) 0.5))
(bind '(x y x1 y1) (cwb-bounds (list (cwb-text-mid "Label" 200 100 :font_size 30))))
(assert-true "words put by their middle have their middle there" (wb-near? (list (* 0.5 (+ x x1)) (* 0.5 (+ y y1))) '(200 100) 0.05))

;what is not a document is not loaded as one
(assert-eq "text that is not a tree is not a document" :nil (cwb-load (string-stream "7 touch 1 300 330")))
(assert-eq "nor a tree of something else" :nil (cwb-load (string-stream "((:list 2) 1 2)")))
(assert-eq "nor nothing" :nil (cwb-load (string-stream "")))

;a file of before this, polygons in groups
(defq wb_stream (string-stream (cat "")))
(tree-save wb_stream (scatter (Emap) :version 3 :groups (list
	(list (list (path 0.0 0.0) (path 10.0 10.0))
		(list (list 0xff112233 (list (path 0.0 0.0 10.0 0.0 10.0 10.0)) (list (path 0.0 0.0) (path 10.0 10.0)))) 0)
	(list (list (path 20.0 0.0) (path 40.0 10.0))
		(list (list 0xff000000 (list (path 20.0 0.0 30.0 0.0 30.0 10.0)) :nil)
			(list 0xffffffff (list (path 30.0 0.0 40.0 0.0 40.0 10.0)) :nil)) 0))))
(defq wb_old (cwb-load (string-stream (str wb_stream))))
(assert-true "a file of the version before, polygons in groups, is made a document of this one" wb_old)
(when wb_old
	(assert-eq "a group of one polygon is a shape, a group of two is a group" '(:shape :group)
		(map (# (cwb-get %0 :type)) (cwb-items wb_old)))
	(assert-eq "a polygon is a shape that is filled with its colour" 0xff112233 (cwb-get (first (cwb-items wb_old)) :fill))
	(assert-true "the document is the size of what is in it and a margin"
		(wb-near? (list (. wb_old :find :width) (. wb_old :find :height)) '(60 30))))

;as an image
(save wb_text "tests/scratch/test_cwb.cwb")
(assert-list-eq "a .cwb is an image of the size it says it is" '(200 100 32) (canvas-info "tests/scratch/test_cwb.cwb"))
(defq wb_canvas (canvas-load "tests/scratch/test_cwb.cwb" +load_flag_noswap))
(assert-list-eq "and loads as one" '(200 100) (if wb_canvas (. wb_canvas :pref_size)))
(defun wb-pixel (canvas x y)
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(bind '(w h) (. canvas :pref_size))
	(defq d (read-blk stream 10000000))
	(get-uint d (+ (- (length d) (* w h 4)) (* 4 (+ (* y w) x)))))
(when wb_canvas
	(assert-eq "the middle of the box is its fill" 0xffff0000 (wb-pixel wb_canvas 30 25))
	(assert-eq "its edge is its stroke" 0xff000000 (wb-pixel wb_canvas 10 25))
	(assert-eq "the box in the group in the group is where it should be" 0xff0000ff (wb-pixel wb_canvas 175 55))
	(assert-eq "and where there is nothing there is nothing, what is behind shows" 0 (wb-pixel wb_canvas 190 5)))
(pii-remove "tests/scratch/test_cwb.cwb")

;words are a shape
(defq wb_words (cwb-text "Hi" 40 50 :font_size 30))
(assert-true "words sit on the line they were put on, and to the right of where"
	(progn (bind '(x y x1 y1) (cwb-bounds (list wb_words))) (and (>= x 40.0) (< x 50.0) (wb-near? y1 50.0 1.0) (< y 30.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pointers and a stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq wb_bind (bindings-default))
(assert-eq "a pen draws" :pen (. wb_bind :value (ptr-event 1 :pen 1 0 0) :tool))
(assert-eq "its other end rubs out" :eraser (. wb_bind :value (ptr-event 2 :eraser 1 0 0) :tool))
(assert-eq "a finger moves things" :hand (. wb_bind :value (ptr-event 3 :touch 1 0 0) :tool))
(assert-eq "the mouse draws with its left button" :pen (. wb_bind :value (ptr-event 0 :mouse +pev_left 0 0) :tool))
(assert-eq "and moves things with its right" :hand (. wb_bind :value (ptr-event 0 :mouse +pev_right 0 0) :tool))
(. wb_bind :bind :pen 9 :nil '(:tool :pen :color 0xffff0000 :width 8.0))
(assert-eq "one pen of them all can be bound to a colour" 0xffff0000 (. wb_bind :value (ptr-event 9 :pen 1 0 0) :color))
(assert-eq "and the rest are not" :nil (. wb_bind :value (ptr-event 1 :pen 1 0 0) :color))

;a stage of two actors, a box at the front and all of it behind
(defclass Wb-actor (x y x1 y1) (Actor)
	(def this :box (list x y x1 y1) :got (list) :left (list))
	(defmethod :hit (x y)
		(bind '(bx by bx1 by1) (get :box this))
		(and (>= x bx) (>= y by) (< x bx1) (< y by1)))
	(defmethod :pointers (stage events)
		(push (get :got this) (map (# (list (elem-get %0 +pev_id) (elem-get %0 +pev_buttons))) events))
		this)
	(defmethod :leave (stage id) (push (get :left this) id) this))
(defq wb_stage (Stage) wb_back (Wb-actor 0.0 0.0 1000.0 1000.0) wb_front (Wb-actor 100.0 100.0 200.0 200.0))
(.-> wb_stage (:add wb_back) (:add wb_front))
(assert-true "a point on the front actor is the front actor's" (eql wb_front (. wb_stage :hit 150.0 150.0)))
(assert-true "one that is not is the back one's" (eql wb_back (. wb_stage :hit 50.0 50.0)))
;two pointers at once, one on each
(. wb_stage :pointers (list (ptr-event 1 :pen 1 150 150) (ptr-event 2 :touch 1 50 50)))
(assert-list-eq "two pointers down, each actor is given its own" '(((1 1)) ((2 1)))
	(list (last (get :got wb_front)) (last (get :got wb_back))))
;the pen moves off the front actor, still down, it is still the front actor's
(. wb_stage :pointers (list (ptr-event 1 :pen 1 50 60)))
(assert-list-eq "a pointer that is down stays with the actor it went down on" '((1 1)) (last (get :got wb_front)))
(. wb_stage :pointers (list (ptr-event 1 :pen 0 50 60)))
(assert-list-eq "and comes up on it" '((1 0)) (last (get :got wb_front)))
(. wb_stage :pointers (list (ptr-event 1 :pen 0 50 61)))
(assert-list-eq "then it is over the back one, and that is told" '((1 0)) (last (get :got wb_back)))
(assert-list-eq "and the front one is told it has left" '(1) (get :left wb_front))
;two fingers on one actor come as one batch
(. wb_stage :pointers (list (ptr-event 5 :touch 1 110 110) (ptr-event 6 :touch 1 190 190)))
(assert-list-eq "two fingers on one actor are given to it together" '((5 1) (6 1)) (last (get :got wb_front)))
(. wb_stage :sub wb_front)
(assert-true "an actor taken off has nothing" (eql wb_back (. wb_stage :hit 150.0 150.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a board, driven
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq wb_board (Board (cwb-doc 400 300)))
(defun wb-bids () (wb-ids (. wb_board :get_doc)))
(defun wb-drag (id kind buttons pts)
	;a pointer goes down at the first point, through the rest, and comes up at the last
	(each (lambda ((x y)) (. wb_board :pointers (list (ptr-event id kind buttons x y)))) pts)
	(bind '(x y) (last pts))
	(. wb_board :pointers (list (ptr-event id kind 0 x y))))
(defun wb-box () (cwb-bounds (. wb_board :selected_items)))

(wb-drag 1 :pen 1 '((10 10) (20 15) (30 30) (60 40) (90 20)))
(assert-list-eq "a pen draws a line, and it is in the document" '(1) (wb-bids))
(assert-eq "it starts and ends where the pen did, and is curved between"
	"M 10 10 Q 20 15 25 22.5 Q 30 30 45 35 Q 60 40 75 30 L 90 20"
	(cwb-get (first (cwb-items (. wb_board :get_doc))) :d))
(def wb_board :mode :rect)
(wb-drag 0 :mouse 1 '((100 100) (150 130) (200 180)))
(assert-eq "set to boxes, the mouse draws a box from where it went down to where it came up"
	"M 100 100 L 200 100 200 180 100 180 Z" (cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(wb-drag 0 :mouse 1 '((300 50)))
(assert-list-eq "a click that does not move draws nothing" '(1 2) (wb-bids))
(def wb_board :mode :fellipse :color 0xffff0000)
(wb-drag 0 :mouse 1 '((250 200) (350 260)))
(assert-eq "a filled ellipse is filled with the board's colour" 0xffff0000 (cwb-get (last (cwb-items (. wb_board :get_doc))) :fill))
(def wb_board :mode :arrow)
(wb-drag 0 :mouse 1 '((20 280) (80 280)))
(assert-list-eq "an arrow has a head at the end it was drawn to" '(:butt :arrow)
	(list (cwb-get (last (cwb-items (. wb_board :get_doc))) :cap1) (cwb-get (last (cwb-items (. wb_board :get_doc))) :cap2)))
(. wb_board :undo)
(assert-list-eq "and is undone" '(1 2 3) (wb-bids))

;a finger picks the box up and moves it
(. wb_board :pointers (list (ptr-event 7 :touch 1 150 100)))
(assert-list-eq "a finger on the box selects it" '(2) (. wb_board :get_selected))
(. wb_board :pointers (list (ptr-event 7 :touch 1 170 120)))
(. wb_board :pointers (list (ptr-event 7 :touch 0 170 120)))
(assert-true "and moves it" (wb-near? (wb-box) '(118.5 118.5 221.5 201.5)))
(. wb_board :undo)
(assert-true "undone, it is back" (wb-near? (cwb-bounds (list (second (cwb-items (. wb_board :get_doc))))) '(98.5 98.5 201.5 181.5)))
(. wb_board :redo)
(. wb_board :select (list 2))
(assert-true "done again, it is moved" (wb-near? (wb-box) '(118.5 118.5 221.5 201.5)))

;two fingers on it, moved to twice as far apart about the same middle
(. wb_board :pointers (list (ptr-event 7 :touch 1 130 130)))
(. wb_board :pointers (list (ptr-event 8 :touch 1 210 190)))
(. wb_board :pointers (list (ptr-event 7 :touch 1 90 100) (ptr-event 8 :touch 1 250 220)))
(assert-true "two fingers that move apart to twice as far make it twice the size, about their middle"
	(wb-near? (wb-box) '(67 77 273 243) 0.05))
(. wb_board :pointers (list (ptr-event 8 :touch 0 250 220)))
(. wb_board :pointers (list (ptr-event 7 :touch 1 100 100)))
(assert-true "one lets go, the other carries on from where it is and moves it" (wb-near? (wb-box) '(77 77 283 243) 0.05))
(. wb_board :pointers (list (ptr-event 7 :touch 0 100 100)))
;two fingers that turn about their middle turn it
(defq wb_box0 (wb-box))
(. wb_board :pointers (list (ptr-event 7 :touch 1 130 160)))
(. wb_board :pointers (list (ptr-event 8 :touch 1 230 160)))
(. wb_board :pointers (list (ptr-event 7 :touch 1 180 110) (ptr-event 8 :touch 1 180 210)))
(. wb_board :pointers (list (ptr-event 7 :touch 0 180 110) (ptr-event 8 :touch 0 180 210)))
(assert-true "two fingers that turn a quarter turn about their middle turn it a quarter turn"
	(progn (bind '(x y x1 y1) wb_box0) (bind '(nx ny nx1 ny1) (wb-box))
		(and (wb-near? (- nx1 nx) (- y1 y) 0.1) (wb-near? (- ny1 ny) (- x1 x) 0.1))))
(. wb_board :undo)

;a finger in a box that is not filled is on the box, a mouse there is on nothing
(. wb_board :select (list))
(. wb_board :pointers (list (ptr-event 0 :mouse +pev_right 150 150)))
(. wb_board :pointers (list (ptr-event 0 :mouse 0 150 150)))
(assert-list-eq "the mouse, in a box that is not filled, is on nothing" '() (. wb_board :get_selected))
(. wb_board :pointers (list (ptr-event 7 :touch 1 150 150)))
(. wb_board :pointers (list (ptr-event 7 :touch 0 150 150)))
(assert-list-eq "a finger there is on the box" '(2) (. wb_board :get_selected))
(. wb_board :select (list))

;a pen draws while a finger holds something else, two owners at once
(def wb_board :mode :pen)
(. wb_board :pointers (list (ptr-event 7 :touch 1 300 230)))
(. wb_board :pointers (list (ptr-event 1 :pen 1 20 250) (ptr-event 7 :touch 1 300 235)))
(. wb_board :pointers (list (ptr-event 1 :pen 1 60 270) (ptr-event 7 :touch 1 300 240)))
(. wb_board :pointers (list (ptr-event 1 :pen 0 60 270) (ptr-event 7 :touch 0 300 240)))
(assert-list-eq "a pen draws while a finger moves the ellipse, both at once" '(1 2 3 5) (wb-bids))
(assert-list-eq "and what the finger was on is what is selected" '(3) (. wb_board :get_selected))

;a pen bound to a colour and a width draws with them, whatever the board is set to
(. (. wb_board :get_bindings) :bind :pen 9 :nil '(:tool :pen :color 0xff00ff00 :width 9.0))
(wb-drag 9 :pen 1 '((300 20) (340 30)))
(assert-list-eq "a pen that is bound to a colour and a width draws with them" '(0xff00ff00 9.0)
	(list (cwb-get (last (cwb-items (. wb_board :get_doc))) :stroke) (cwb-get (last (cwb-items (. wb_board :get_doc))) :width)))
(. wb_board :undo)

;the other end of the pen rubs the first line out, set to take a line whole
(def wb_board :rub_mode :whole)
(wb-drag 2 :eraser 1 '((20 15) (30 30)))
(assert-list-eq "the eraser, set to take lines whole, takes out the line it goes over" '(2 3 5) (wb-bids))
(wb-drag 2 :eraser 1 '((390 5) (395 8)))
(assert-list-eq "and nothing where there is nothing" '(2 3 5) (wb-bids))
(def wb_board :rub_mode :part)

;select mode, so that a mouse alone has the handles
(def wb_board :mode :select)
(. wb_board :select (list 2))
(defq wb_handles (get :handles wb_board))
(assert-eq "what is selected has eight handles to size it and one to turn it" 9 (length (. wb_handles :spots)))
(bind '(x y x1 y1) (wb-box))
(wb-drag 0 :mouse 1 (list (list x1 y1) (list (+ x1 20.0) (+ y1 20.0))))
(assert-true "dragged by a corner it is sized, the corner across from it stays, and it keeps its shape"
	(progn (bind '(nx ny nx1 ny1) (wb-box))
		(and (wb-near? (list nx ny) (list x y) 0.05) (> nx1 x1) (> ny1 y1)
			(wb-near? (/ (- nx1 nx) (- ny1 ny)) (/ (- x1 x) (- y1 y)) 0.02))))
(def wb_board :keep_shape :nil)
(bind '(x y x1 y1) (wb-box))
(wb-drag 0 :mouse 1 (list (list x1 (* 0.5 (+ y y1))) (list (+ x1 30.0) (* 0.5 (+ y y1)))))
(assert-true "dragged by the middle of a side only that side moves"
	(wb-near? (wb-box) (list x y (+ x1 30.0) y1) 0.05))
(bind '(x y x1 y1) (wb-box))
(defq wb_cx (* 0.5 (+ x x1)) wb_cy (* 0.5 (+ y y1)) wb_top (- y +board_turn_gap))
(wb-drag 0 :mouse 1 (list (list wb_cx wb_top) (list (+ wb_cx (- wb_cy wb_top)) wb_cy)))
(assert-true "dragged a quarter turn by the round handle it is turned a quarter turn"
	(progn (bind '(nx ny nx1 ny1) (wb-box))
		(and (wb-near? (- nx1 nx) (- y1 y) 0.2) (wb-near? (- ny1 ny) (- x1 x) 0.2))))
(def wb_board :snap_angle (/ +fp_pi 12.0))
(defq wb_before (cwb-get (first (. wb_board :selected_items)) :m))
;turned, the handle it is turned by is out from what was its top, which now faces the side
(defq wb_tx (second (last (. wb_handles :spots))) wb_ty (third (last (. wb_handles :spots))))
(assert-true "the handle it is turned by has gone round with it, to its side"
	(progn (bind '(x y x1 y1) (wb-box))
		(and (> wb_tx x1) (wb-near? wb_ty (* 0.5 (+ y y1)) 0.2))))
(wb-drag 0 :mouse 1 (list (list wb_tx wb_ty) (list wb_tx (+ wb_ty 2.0))))
(assert-true "with angles that snap, a small turn is no turn" (wb-near? (map (const identity) (cwb-get (first (. wb_board :selected_items)) :m)) (map (const identity) wb_before) 0.001))
(def wb_board :snap_angle 0.0)

;one thing has its handles in its own frame. A box turned an eighth of a turn
(defq wb_doc2 (. wb_board :get_doc) wb_turned (. wb_board :add (cwb-shape (cwb-d-rect 0 0 100 40) :fill 0xff00ff00 :stroke 0
	:m (cwb-mat-mul (cwb-mat-move 400 400) (cwb-mat-turn (/ +fp_pi 4.0))))))
(. wb_board :select (list wb_turned))
(defq wb_spots (. wb_handles :spots) wb_r (/ 100.0 (sqrt 2.0)))
(assert-true "turned, its handles are at its own corners, not those of the box round it"
	(and (wb-near? (slice (first wb_spots) 1 3) '(400 400) 0.05)
		(wb-near? (slice (third wb_spots) 1 3) (list (+ 400.0 wb_r) (+ 400.0 wb_r)) 0.05)))
(assert-true "and the frame drawn round it is its own four corners"
	(wb-near? (slice (. wb_handles :corners) 0 4) (list 400 400 (+ 400.0 wb_r) (+ 400.0 wb_r)) 0.05))
;pulled by the middle of its far end, along itself, to twice as long
(defq wb_ex (second (elem-get wb_spots 4)) wb_ey (third (elem-get wb_spots 4)))
(wb-drag 0 :mouse 1 (list (list wb_ex wb_ey) (list (+ wb_ex wb_r) (+ wb_ey wb_r))))
(defq wb_m (cwb-get (first (. wb_board :selected_items)) :m))
(defq wb_a (elem-get wb_m 0) wb_b (elem-get wb_m 1) wb_c (elem-get wb_m 3) wb_d (elem-get wb_m 4))
(assert-true "pulled by the middle of an end it is twice as long along itself"
	(wb-near? (sqrt (+ (* wb_a wb_a) (* wb_c wb_c))) 2.0 0.01))
(assert-true "no wider" (wb-near? (sqrt (+ (* wb_b wb_b) (* wb_d wb_d))) 1.0 0.01))
(assert-true "and still a box: its two sides are still square to each other"
	(wb-near? (+ (* wb_a wb_b) (* wb_c wb_d)) 0.0 0.01))
(assert-true "the end across from the one pulled has stayed where it was"
	(wb-near? (slice (first (. wb_handles :spots)) 1 3) '(400 400) 0.05))
(. wb_board :undo) (. wb_board :undo)

;a line has an end to drag at each end, and nothing else
(def wb_board :mode :line)
(wb-drag 0 :mouse 1 '((500 100) (600 100)))
(def wb_board :mode :select)
(defq wb_line (last (cwb-items (. wb_board :get_doc))))
(. wb_board :select (list (elem-get wb_line +cwb_id)))
(assert-list-eq "a line that is selected has two handles, one at each end" '((:end 500 100 0) (:end 600 100 1))
	(map (lambda ((kind x y which &ignore)) (list kind (n2i x) (n2i y) which)) (. wb_handles :spots)))
(wb-drag 0 :mouse 1 '((600 100) (620 150) (640 180)))
(assert-eq "one end dragged, the line goes from where it did to where that end now is" "M 500 100 L 640 180" (cwb-get wb_line :d))
(wb-drag 0 :mouse 1 '((500 100) (520 60)))
(assert-eq "and the other" "M 520 60 L 640 180" (cwb-get wb_line :d))
(def wb_board :snap_angle (/ +fp_pi 12.0))
(wb-drag 0 :mouse 1 '((640 180) (700 64)))
(assert-true "with angles that snap it is level when it is pulled to nearly level, and as long as it was pulled"
	(progn (defq wb_parts (map (const str-to-num) (filter (# (not (find %0 '("M" "L")))) (split (cwb-get wb_line :d) " "))))
		(and (wb-near? (elem-get wb_parts 3) 60.0 0.05) (wb-near? (elem-get wb_parts 2) (+ 520.0 (sqrt (+ (* 180.0 180.0) 16.0))) 0.05))))
(def wb_board :snap_angle 0.0)
(. wb_board :transform (cwb-mat-move 0 100))
(wb-drag 0 :mouse 1 (list (slice (second (. wb_handles :spots)) 1 3) '(700 300)))
(assert-true "a line that has been moved has its end go to where the pointer is, not that far in its own space"
	(wb-near? (slice (second (. wb_handles :spots)) 1 3) '(700 300) 0.05))
(. wb_board :undo) (. wb_board :undo) (. wb_board :undo) (. wb_board :undo) (. wb_board :undo) (. wb_board :undo)

;moved with a grid, the corner of the box round it goes to the grid
(def wb_board :mode :select :snap 16.0)
(defq wb_snapped (. wb_board :add (cwb-shape (cwb-d-rect 403 203 443 233) :fill 0xff0000ff :stroke 0)))
(. wb_board :select (list wb_snapped))
(wb-drag 0 :mouse 1 '((420 220) (440 229)))
(assert-list-eq "moved with a grid of 16, the top left of what is moved is on the grid" '(416 208)
	(map (const n2i) (slice (wb-box) 0 2)))
(wb-drag 0 :mouse 1 '((430 220) (433 222)))
(assert-list-eq "moved a little more it stays there" '(416 208) (map (const n2i) (slice (wb-box) 0 2)))
(def wb_board :snap 0.0 :mode :line)
(. wb_board :undo) (. wb_board :undo) (. wb_board :undo)
(. wb_board :select (list))

;a grid that points snap to
(def wb_board :mode :line :snap 16.0)
(wb-drag 0 :mouse 1 '((21 39) (99 71)))
(assert-eq "with a grid of 16 a line goes from and to the nearest points of it" "M 16 32 L 96 64"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(def wb_board :snap 0.0)
(. wb_board :undo)

;what a hand can do without a hand
(. wb_board :select_all)
(assert-list-eq "all is selected" '(2 3 5) (. wb_board :get_selected))
(defq wb_group (. wb_board :group))
(assert-list-eq "grouped, the three are one" (list wb_group) (wb-bids))
(. wb_board :ungroup)
(assert-list-eq "ungrouped, they are three again, and all selected" '((2 3 5) (2 3 5))
	(list (wb-bids) (sort (cat (. wb_board :get_selected)) (const -))))
(defq wb_before (map (# (cwb-bounds (list %0))) (. wb_board :selected_items)))
(. wb_board :group) (. wb_board :ungroup)
(assert-true "and each is where it was" (every (# (wb-near? %0 %1 0.05)) wb_before (map (# (cwb-bounds (list %0))) (. wb_board :selected_items))))
(. wb_board :align :left)
(assert-true "lined up on the left, their left sides are the one"
	(progn (defq wb_lefts (map (# (first (cwb-bounds (list %0)))) (. wb_board :selected_items)))
		(every (# (wb-near? %0 (first wb_lefts) 0.05)) wb_lefts)))
(. wb_board :select (list 2))
(. wb_board :order :t)
(assert-eq "brought to the front it is last" 2 (last (wb-bids)))
(. wb_board :order :nil)
(assert-eq "sent to the back it is first" 2 (first (wb-bids)))
(. wb_board :duplicate)
(assert-eq "a copy has an id of its own, and is what is selected" (last (wb-bids)) (first (. wb_board :get_selected)))
(assert-eq "there is one more" 4 (length (wb-bids)))
(. wb_board :style :stroke 0xff0000ff :width 5.0)
(assert-list-eq "the style of what is selected is set" '(0xff0000ff 5.0)
	(list (cwb-get (last (cwb-items (. wb_board :get_doc))) :stroke) (cwb-get (last (cwb-items (. wb_board :get_doc))) :width)))
(. wb_board :delete)
(assert-eq "deleted, it is gone" 3 (length (wb-bids)))
(defq wb_steps 0)
(while (. wb_board :undo) (++ wb_steps))
(assert-list-eq "every step can be undone, back to nothing" '() (wb-bids))
(assert-true "and there were many" (> wb_steps 15))
(while (. wb_board :redo))
(assert-eq "and done again" 3 (length (wb-bids)))

;it draws
(defq wb_canvas (Canvas 400 300 1))
(.-> wb_canvas (:set_canvas_flags +canvas_flag_antialias) (:fill 0))
(assert-eq "the board draws its shapes on a canvas" 3 (. wb_board :draw wb_canvas))
(assert-eq "and none of them in a clip that none is in" 0 (. wb_board :draw wb_canvas :nil '(390.0 0.0 400.0 4.0)))
(. wb_board :select_all)
(. wb_board :draw_overlay wb_canvas)
(assert-true "what has changed is said once" (and (. wb_board :dirty? +board_dirty_doc) (not (. wb_board :dirty? +board_dirty_doc))))

;rubbing out part of a line, as the eraser does unless it is set not to
(defq rb_board (Board (cwb-doc 400 300)) rb_doc (. rb_board :get_doc))
(defun rb-ds () (map (# (cwb-get %0 :d)) (cwb-items rb_doc)))
(assert-list-eq "a line of points cut by a circle is what is outside it, cut where it meets it"
	'((0 0 40 0) (60 0 100 0)) (map (# (map (const n2i) %0)) (cwb-rub-points (path 0.0 0.0 100.0 0.0) 50.0 0.0 10.0)))
(assert-eq "one the circle is nowhere near is not cut" :nil (cwb-rub-points (path 0.0 0.0 100.0 0.0) 50.0 30.0 10.0))
(assert-list-eq "one all inside it has nothing left" '() (cwb-rub-points (path 45.0 0.0 55.0 0.0) 50.0 0.0 10.0))
(assert-list-eq "a line of three points, cut at the middle one, is its two ends"
	'((0 0 40 0) (50 10 50 50)) (map (# (map (const n2i) %0)) (cwb-rub-points (path 0.0 0.0 50.0 0.0 50.0 50.0) 50.0 0.0 10.0)))
(. rb_board :add (cwb-shape (cwb-d-line 0 100 200 100) :width 4 :cap1 :butt :cap2 :arrow :kind :arrow))
(. rb_board :add (cwb-shape (cwb-d-rect 250 50 350 150) :fill 0xffff0000))
(. rb_board :take_changes)
(assert-list-eq "what can be rubbed out in part is a line, not a filled box" '(:t :nil) (map (const cwb-rub?) (cwb-items rb_doc)))
(assert-eq "rubbed at its middle" :t (. rb_board :rub 100 100 10))
(assert-list-eq "a line is two lines, the gap as wide as the eraser and the line's own width"
	(list "M 0 100 L 88 100" "M 112 100 L 200 100" (cwb-d-rect 250 50 350 150)) (rb-ds))
(assert-list-eq "they are new items, where the line was among the rest" '(3 4 2) (map (# (elem-get %0 +cwb_id)) (cwb-items rb_doc)))
(assert-list-eq "an end that was the line's own is as it was, an arrow still, and an end that was cut is round"
	'((:butt :round) (:round :arrow)) (map (# (list (cwb-get %0 :cap1) (cwb-get %0 :cap2))) (slice (cwb-items rb_doc) 0 2)))
(assert-list-eq "what keeps a copy in step is told of the new ones" '(3 4) (. rb_board :take_changes))
(assert-eq "rubbed where there is nothing, nothing is" :nil (. rb_board :rub 100 200 10))
(. rb_board :rub 0 100 10)
(assert-eq "rubbed at an end, the end is shorter" "M 12 100 L 88 100" (first (rb-ds)))
(. rb_board :rub_along 12 100 88 100 10)
(assert-list-eq "rubbed all along, it is gone" (list "M 112 100 L 200 100" (cwb-d-rect 250 50 350 150)) (rb-ds))
(assert-list-eq "on the box, with no line there, the box goes whole" '(:t 1) (list (. rb_board :rub 300 100 10) (length (cwb-items rb_doc))))
;a line that has been moved and made twice the size
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-line 0 0 100 0) :width 2
	:m (cwb-mat-mul (cwb-mat-move 50 200) (cwb-mat-scale 2.0))))
(. rb_board :rub 150 200 10)
(assert-list-eq "a line that is twice the size is cut in its own space, half as far" '("M 0 0 L 44 0" "M 56 0 L 100 0") (rb-ds))
(assert-true "and what is left is where it was on the board"
	(wb-near? (cwb-bounds (cwb-items rb_doc)) '(48 198 252 202) 0.1))
;a line drawn by hand
(. rb_board :clear)
(. rb_board :add (cwb-shape (board-pen-d '(20 50 80 20 140 80 200 50)) :width 3 :kind :pen))
(. rb_board :rub 110 50 8)
(assert-eq "a curved line rubbed in the middle is two" 2 (length (cwb-items rb_doc)))
(assert-true "one each side of where it was rubbed"
	(and (< (third (cwb-bounds (slice (cwb-items rb_doc) 0 1))) 108.0) (> (first (cwb-bounds (slice (cwb-items rb_doc) 1 2))) 112.0)))
;a box that is not filled is not a line with an end
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-rect 100 100 200 200)))
(. rb_board :rub 100 150 10)
(assert-eq "a box, though not filled, goes whole" 0 (length (cwb-items rb_doc)))
;the eraser itself, a pointer
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-line 100 20 100 280) :width 4))
(. rb_board :add (cwb-shape (cwb-d-line 200 20 200 280) :width 4))
(defq rb_steps (length (get :undo_stack rb_board)))
(. rb_board :pointers (list (ptr-event 2 :eraser 1 40 150)))
(. rb_board :pointers (list (ptr-event 2 :eraser 1 260 150)))
(assert-true "while it rubs, the eraser is to be drawn where it is" (wb-near? (get :rubber rb_board) '(260 150 8) 0.01))
(. rb_board :pointers (list (ptr-event 2 :eraser 0 260 150)))
(assert-eq "the eraser dragged across two lines in one move cuts both, it rubs all the way it went" 4 (length (cwb-items rb_doc)))
(assert-eq "and is not drawn when it is up" :nil (get :rubber rb_board))
(assert-eq "it is one step" (inc rb_steps) (length (get :undo_stack rb_board)))
(. rb_board :undo)
(assert-list-eq "that can be undone" (list (cwb-d-line 100 20 100 280) (cwb-d-line 200 20 200 280)) (rb-ds))
(. rb_board :pointers (list (ptr-event 2 :eraser 1 300 250)))
(. rb_board :pointers (list (ptr-event 2 :eraser 0 300 250)))
(assert-eq "an eraser that rubbed nothing out is no step" rb_steps (length (get :undo_stack rb_board)))
(def rb_board :zoom 2.0)
(. rb_board :rub 100 150)
(assert-list-eq "at twice the size it reaches half as far on the board"
	(list "M 100 20 L 100 144" "M 100 156 L 100 280" (cwb-d-line 200 20 200 280)) (rb-ds))
(def rb_board :zoom 1.0 :rub_mode :whole)
(. rb_board :rub 200 150)
(assert-eq "set to take a line whole, it does" 2 (length (cwb-items rb_doc)))

;an arc is written as arcs of no more than a third of a turn, each is on its circle
(defun rb-arc-off (a0 sweep)
	;the furthest a point of the arc from a0 round by sweep, about 400 300 of radius 180, is off its circle
	(defq worst 0.0)
	(each (lambda ((closed p))
		(each (lambda ((x y)) (setq worst (max worst (abs (- (sqrt (+ (* (- x 400.0) (- x 400.0)) (* (- y 300.0) (- y 300.0)))) 180.0)))))
			(partition p 2)))
		(path-gen-paths (cwb-d-arc 400 300 180 a0 (+ a0 sweep))))
	worst)
(assert-list-eq "an arc of a quarter turn is one A, of half a turn two, of nearly all the way round three" '(1 2 3)
	(map (# (length (filter (# (eql %0 "A")) (split (cwb-d-arc 0 0 100 0.5 (+ 0.5 %0)) " ")))) (list 1.5708 3.14159 6.2)))
(assert-true "an arc all but a five hundredth of a turn round, its ends a third of a pixel apart, is on its circle to a twentieth of a pixel"
	(< (reduce (# (max %0 (rb-arc-off (* (n2f %1) 0.0731) (- +fp_2pi 0.002)))) (range 0 40) 0.0) 0.05))
(assert-true "and so is one of half a turn, and of a little more"
	(< (reduce (# (max %0 (rb-arc-off (* (n2f %1) 0.0731) 3.14159) (rb-arc-off (* (n2f %1) 0.0731) 3.2832))) (range 0 40) 0.0) 0.05))
(assert-true "a slice of pie is its two lines and those arcs"
	(progn (defq rb_pie (cwb-d-arc 400 300 180 0 4.0 :t)) (and (starts-with "M 400 300 L 580 300 A" rb_pie) (ends-with "Z" rb_pie))))

;the paper made the size of what is on it
(defq ft_doc (cwb-doc 800 600))
(assert-eq "a document with nothing in it is left as it is" :nil (cwb-fit ft_doc))
(cwb-add ft_doc (cwb-shape (cwb-d-rect 300 200 400 260) :fill 0xffff0000 :stroke 0))
(cwb-add ft_doc (cwb-group (list (cwb-shape (cwb-d-rect 0 0 50 40) :fill 0xff00ff00 :stroke 0)) :m (cwb-mat-move 500 300)))
(assert-list-eq "fitted, all of it is moved so the box round it is 16 in from the top left" '(-284 -184) (map (const n2i) (cwb-fit ft_doc)))
(assert-list-eq "the box round it is then 16 in" '(16 16 266 156) (map (const n2i) (cwb-bounds (cwb-items ft_doc))))
(assert-list-eq "and the paper is that box and 16 all round" '(282 172) (list (. ft_doc :find :width) (. ft_doc :find :height)))
(assert-eq "a shape is moved by its matrix, it is the shape it was" (cwb-d-rect 300 200 400 260) (cwb-get (first (cwb-items ft_doc)) :d))
(assert-list-eq "fitted again it is not moved" '(0 0) (map (const n2i) (cwb-fit ft_doc)))
(cwb-add ft_doc (cwb-shape (cwb-d-line -40 -30 20 20) :width 4))
(cwb-fit ft_doc 0)
(assert-list-eq "what is off the top left is brought on, with no room round it if none is asked for" '(0 0 308 188)
	(cat (map (const n2i) (slice (cwb-bounds (cwb-items ft_doc)) 0 2)) (list (. ft_doc :find :width) (. ft_doc :find :height))))
;a board, with an instrument on it, as a step that can be undone
(defq ft_board (Board (cwb-doc 800 600)))
(. ft_board :add (cwb-shape (cwb-d-rect 300 200 400 260) :fill 0xffff0000 :stroke 0))
(import "lib/cwb/tools.inc")
(. (. ft_board :get_stage) :add (defq ft_ruler (Ruler ft_board 350 300)))
(. ft_board :take_changes)
(assert-list-eq "a board fitted says how far it all moved" '(-284 -184) (map (const n2i) (. ft_board :fit)))
(assert-list-eq "a ruler on it is moved with what it lay on" '(66 116) (map (const n2i) (get :origin ft_ruler)))
(assert-eq "what keeps a copy in step is told all of it changed" :all (. ft_board :take_changes))
(. ft_board :undo)
(assert-list-eq "undone, the shape is where it was" '(300 200 400 260) (map (const n2i) (cwb-bounds (cwb-items (. ft_board :get_doc)))))
;what is off the paper is taken away, when it is asked for
(defq cr_board (Board (cwb-doc 400 300)) cr_doc (. cr_board :get_doc))
(. cr_board :add (cwb-shape (cwb-d-rect 50 50 100 100) :fill 0xffff0000 :stroke 0))
(. cr_board :add (cwb-shape (cwb-d-rect 380 280 460 340) :fill 0xff00ff00 :stroke 0))
(. cr_board :add (cwb-shape (cwb-d-rect 500 100 560 160) :fill 0xff0000ff :stroke 0))
(. cr_board :add (cwb-shape (cwb-d-line -80 -20 -10 -60) :width 4))
(defq cr_steps (length (get :undo_stack cr_board)))
(assert-eq "two of four things are nowhere on the paper, and go" 2 (. cr_board :crop))
(assert-list-eq "the one on it stays, and the one part on it, all of it" '(1 2) (map (# (elem-get %0 +cwb_id)) (cwb-items cr_doc)))
(assert-eq "it is one step" (inc cr_steps) (length (get :undo_stack cr_board)))
(assert-list-eq "with nothing off the paper nothing goes, and it is no step" (list 0 (inc cr_steps)) (list (. cr_board :crop) (length (get :undo_stack cr_board))))
(. cr_board :undo)
(assert-list-eq "undone, all four are back" '(1 2 3 4) (map (# (elem-get %0 +cwb_id)) (cwb-items cr_doc)))
(. cr_board :redo)
(assert-list-eq "and done again, two" '(1 2) (map (# (elem-get %0 +cwb_id)) (cwb-items cr_doc)))
;taken by the right button, a thing goes to the back of its layer
(defq bk_board (Board (cwb-doc 400 300)) bk_doc (. bk_board :get_doc))
(each (# (. bk_board :add (cwb-shape (cwb-d-rect %0 50 (+ %0 100) 150) :fill 0xffff0000 :stroke 0))) '(50 100 150))
(defun bk-ids () (map (# (elem-get %0 +cwb_id)) (cwb-items bk_doc)))
(defun bk-tap (buttons kind x y) (. bk_board :pointers (list (ptr-event 0 kind buttons x y))) (. bk_board :pointers (list (ptr-event 0 kind 0 x y))))
(defq bk_steps (length (get :undo_stack bk_board)))
(bk-tap +pev_right :mouse 180 100)
(assert-list-eq "the top one of three, taken by the right button and let go, is at the back" '(3 1 2) (bk-ids))
(assert-list-eq "it is selected, and it is a step" (list '(3) (inc bk_steps)) (list (. bk_board :get_selected) (length (get :undo_stack bk_board))))
(bk-tap +pev_right :mouse 180 100)
(assert-list-eq "the same place again takes the one that is now on top there, and that goes to the back" '(2 3 1) (bk-ids))
(. bk_board :undo)
(assert-list-eq "undone, it is back where it was" '(3 1 2) (bk-ids))
(def bk_board :mode :select)
(bk-tap +pev_left :mouse 220 100)
(assert-list-eq "the left button, set to select, takes a thing and leaves it where it is among the rest" '(3 1 2) (bk-ids))
(bk-tap +pev_left :touch 220 100)
(assert-list-eq "and so does a finger" '(3 1 2) (bk-ids))
