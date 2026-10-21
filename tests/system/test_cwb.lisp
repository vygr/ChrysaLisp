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
(assert-list-eq "and it ends on the point it was to" '(500.0 0.0) (map! (const identity) (list wb_arc) -3 -1))
(assert-list-eq "radii too small to reach are made as big as is needed, the end is still the end" '(70.0 80.0)
	(map! (const identity) (list (path-gen-earc 0.0 0.0 5.0 6.0 0.5 1.0 0.0 70.0 80.0 (path 0.0 0.0))) -3 -1))
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
	(wb-near? (Mat3x2-point (Mat3x2-mul (Mat3x2-scale 2) (Mat3x2-translate 10 5)) 1 1) '(22.0 12.0)))
(assert-true "a quarter turn about a point" (wb-near? (Mat3x2-point (Mat3x2-rot +fp_hpi 10 10) 20 10) '(10.0 20.0) 0.001))
(defq wb_m (Mat3x2-mul (Mat3x2-rot 0.7 3 4) (Mat3x2-scale 2.5 0.5 1 1)))
(assert-true "a matrix and the one that undoes it"
	(wb-near? (apply (const Mat3x2-point) (cat (list (Mat3x2-invert wb_m)) (Mat3x2-point wb_m 12 -7))) '(12.0 -7.0) 0.001))
(assert-eq "no matrix and a matrix is the matrix" wb_m (Mat3x2-mul :nil wb_m))

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
	wb_b (cwb-add wb_doc (cwb-shape (cwb-d-line 0 0 100 0) :m (Mat3x2-translate 50 80)))
	wb_g (cwb-add wb_doc (cwb-group (list (cwb-shape (cwb-d-rect 0 0 10 10) :fill 0xff00ff00 :stroke 0)
		(cwb-group (list (cwb-shape (cwb-d-rect 20 0 30 10) :fill 0xff0000ff :stroke 0)) :name "inner"))
		:m (Mat3x2-translate 150 50) :name "outer")))
(assert-list-eq "each item of a layer has an id" '(1 2 3) (wb-ids wb_doc))
(assert-list-eq "those in a group too, in the order they are in" '(4 5 6)
	(list (elem-get (first (cwb-get wb_g :items)) +cwb_id) (elem-get (second (cwb-get wb_g :items)) +cwb_id)
		(elem-get (first (cwb-get (second (cwb-get wb_g :items)) :items)) +cwb_id)))
(assert-list-eq "a shape is flattened to one polygon to fill and two for its edge" '(1 2)
	(map! (const length) (list (cwb-flat wb_a)) 0 -2))
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
	"((:Emap 1) :version 1 :width 64 :height 48"
	" :items ((:list 2) ((:list 3) :shape :d \q[M 0 0 L 10 10]\q) ((:list 5) :shape :d \q[M 5 5 L 9 9]\q :width 1.0)))"))))
(assert-true "a file written by hand loads" wb_hand)
(when wb_hand
	(assert-list-eq "its items are given ids" '(1 2) (wb-ids wb_hand))
	(assert-list-eq "what it did not say is what a shape has anyway" '(0 0xff000000 2.0)
		(list (cwb-get (first (cwb-items wb_hand)) :fill) (cwb-get (first (cwb-items wb_hand)) :stroke)
			(cwb-get (first (cwb-items wb_hand)) :width))))

;a file of a newer Whiteboard: what is not known here, of the document,
;of a thing and of a thing in a group, is kept and saved back as it was
(defq wb_new (cwb-load (string-stream (cat
	"((:Emap 1) :version 1 :width 64 :height 48 :future 42"
	" :items ((:list 2) ((:list 5) :shape :d \q[M 0 0 L 10 10]\q :glow 3)"
	" ((:list 3) :group :items ((:list 1) ((:list 5) :shape :d \q[M 5 5 L 9 9]\q :sparkle \q[x]\q)))))"))))
(assert-true "a file with keys not known here loads" wb_new)
(when wb_new
	(defq wb_new_s (string-stream (cat "")))
	(cwb-save wb_new wb_new_s)
	(defq wb_new2 (cwb-load (string-stream (str wb_new_s))) wb_new_items (cwb-items wb_new2))
	(assert-eq "the document's key not known here is saved back" 42 (. (. wb_new2 :find :others) :find :future))
	(assert-list-eq "a thing's" '(:glow 3) (elem-get (first wb_new_items) +cwb_others))
	(assert-list-eq "and one in a group's" '(:sparkle "x")
		(elem-get (first (elem-get (second wb_new_items) +cwb_items)) +cwb_others))
	(assert-eq "what is known is as it was" "M 0 0 L 10 10" (cwb-get (first wb_new_items) :d)))

;:script is a key of a thing and of the document, reserved for the Lisp
;each is to run, nothing runs it yet: kept as any key is, not as one
;not known
(defq wb_scr (cwb-load (string-stream (cat
	"((:Emap 1) :version 1 :width 64 :height 48 :script \q[(doc)]\q"
	" :items ((:list 1) ((:list 5) :shape :d \q[M 0 0 L 10 10]\q :script \q[(item)]\q)))"))))
(when wb_scr
	(defq wb_scr_s (string-stream (cat "")))
	(cwb-save wb_scr wb_scr_s)
	(defq wb_scr2 (cwb-load (string-stream (str wb_scr_s))))
	(assert-list-eq "a script of a thing and of the document are kept, as theirs"
		'("(item)" "(doc)" :nil :nil)
		(list (cwb-get (first (cwb-items wb_scr2)) :script) (. wb_scr2 :find :script)
			(elem-get (first (cwb-items wb_scr2)) +cwb_others) (. wb_scr2 :find :others))))

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
;a release build is given only what is right, text that is not a tree is
;an error that only a checked build says
(if *test_checked*
	(assert-eq "text that is not a tree is not a document" :nil (cwb-load (string-stream "7 touch 1 300 330")))
	(test-skip "text that is not a tree is not a document" "needs an error checked build"))
(assert-eq "nor a tree of something else" :nil (cwb-load (string-stream "((:list 2) 1 2)")))
(assert-eq "nor nothing" :nil (cwb-load (string-stream "")))

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
(assert-eq "and so does the button on its side" :eraser (. wb_bind :value (ptr-event 1 :pen +pev_right 0 0) :tool))
(assert-eq "a finger moves things" :hand (. wb_bind :value (ptr-event 0x1000003 :touch 1 0 0) :tool))
(assert-eq "the mouse draws with its left button" :pen (. wb_bind :value (ptr-event 0 :mouse +pev_left 0 0) :tool))
(assert-eq "and moves things with its right" :hand (. wb_bind :value (ptr-event 0 :mouse +pev_right 0 0) :tool))
(. wb_bind :bind :pen 9 :nil '(:tool :pen :color 0xffff0000 :width 8.0))
(assert-eq "one pen of them all can be bound to a colour" 0xffff0000 (. wb_bind :value (ptr-event 9 :pen 1 0 0) :color))
(assert-eq "and the rest are not" :nil (. wb_bind :value (ptr-event 1 :pen 1 0 0) :color))

;a stage of two actors, a box at the front and all of it behind
(defclass Wb-actor (x y x1 y1) (Actor)
	(def this :box (list x y x1 y1) :got (list) :left (list))
	(defmethod :hit (x y &optional event)
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
(. wb_stage :pointers (list (ptr-event 1 :pen 1 150 150) (ptr-event 0x1000002 :touch 1 50 50)))
(assert-list-eq "two pointers down, each actor is given its own" '(((1 1)) ((16777218 1)))
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
(. wb_stage :pointers (list (ptr-event 0x1000005 :touch 1 110 110) (ptr-event 0x1000006 :touch 1 190 190)))
(assert-list-eq "two fingers on one actor are given to it together" '((16777221 1) (16777222 1)) (last (get :got wb_front)))
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

;a finger tap picks the box up, and a drag moves it
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 150 100)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 150 100)))
(assert-list-eq "a finger tap on the box selects it" '(2) (. wb_board :get_selected))
;on its line, away from its handles
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 100 120)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 120 140)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 120 140)))
(assert-true "and moves it" (wb-near? (wb-box) '(118.5 118.5 221.5 201.5)))
(. wb_board :undo)
(assert-true "undone, it is back" (wb-near? (cwb-bounds (list (first (cwb-find (. wb_board :get_doc) 2)))) '(98.5 98.5 201.5 181.5)))
(. wb_board :redo)
(. wb_board :select (list 2))
(assert-true "done again, it is moved" (wb-near? (wb-box) '(118.5 118.5 221.5 201.5)))

;two fingers on its lines, moved to twice as far apart about the same middle
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 120 140)))
(. wb_board :pointers (list (ptr-event 0x1000008 :touch 1 220 180)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 70 120) (ptr-event 0x1000008 :touch 1 270 200)))
(assert-true "two fingers that move apart to twice as far make it twice the size, about their middle"
	(wb-near? (wb-box) '(67 77 273 243) 0.05))
(. wb_board :pointers (list (ptr-event 0x1000008 :touch 0 270 200)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 80 120)))
(assert-true "one lets go, the other carries on from where it is and moves it" (wb-near? (wb-box) '(77 77 283 243) 0.05))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 80 120)))
;two fingers on its top line that turn about their middle turn it
(defq wb_box0 (wb-box))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 130 80)))
(. wb_board :pointers (list (ptr-event 0x1000008 :touch 1 230 80)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 180 30) (ptr-event 0x1000008 :touch 1 180 130)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 180 30) (ptr-event 0x1000008 :touch 0 180 130)))
(assert-true "two fingers that turn a quarter turn about their middle turn it a quarter turn"
	(progn (bind '(x y x1 y1) wb_box0) (bind '(nx ny nx1 ny1) (wb-box))
		(and (wb-near? (- nx1 nx) (- y1 y) 0.1) (wb-near? (- ny1 ny) (- x1 x) 0.1))))
(. wb_board :undo)

;in a box that is not filled is on nothing, for a mouse and a finger: only
;what is drawn is to be had, never the box round it
(. wb_board :select (list))
(. wb_board :pointers (list (ptr-event 0 :mouse +pev_right 150 150)))
(. wb_board :pointers (list (ptr-event 0 :mouse 0 150 150)))
(assert-list-eq "the mouse, in a box that is not filled, is on nothing" '() (. wb_board :get_selected))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 150 150)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 150 150)))
(assert-list-eq "and so is a finger" '() (. wb_board :get_selected))
(. wb_board :select (list))

;a pen draws while a finger holds something else, two owners at once
(def wb_board :mode :pen)
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 300 230)))
(. wb_board :pointers (list (ptr-event 1 :pen 1 20 250) (ptr-event 0x1000007 :touch 1 300 235)))
(. wb_board :pointers (list (ptr-event 1 :pen 1 60 270) (ptr-event 0x1000007 :touch 1 300 240)))
(. wb_board :pointers (list (ptr-event 1 :pen 0 60 270) (ptr-event 0x1000007 :touch 0 300 240)))
(assert-list-eq "a pen draws while a finger moves the ellipse, both at once" '(1 2 3 5) (wb-bids))
(assert-list-eq "and what the finger moved is not selected: a drag moves a thing and selects nothing" '() (. wb_board :get_selected))

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
;turned, the handle it is turned by is out from what was its top, which now faces the side
(defq wb_tx (second (last (. wb_handles :spots))) wb_ty (third (last (. wb_handles :spots))))
(assert-true "the handle it is turned by has gone round with it, to its side"
	(progn (bind '(x y x1 y1) (wb-box))
		(and (> wb_tx x1) (wb-near? wb_ty (* 0.5 (+ y y1)) 0.2))))
(wb-drag 0 :mouse 1 (list (list wb_tx wb_ty) (list wb_tx (+ wb_ty 2.0))))
(assert-true "with angles that snap, a small turn puts it on the step it is nearest, a quarter turn, its angle and not how far it was turned"
	(progn (defq wb_a (third (cwb-place (first (. wb_board :selected_items)))))
		(and (wb-near? (sin wb_a) 1.0 0.0001) (wb-near? (cos wb_a) 0.0 0.0001))))
(def wb_board :snap_angle 0.0)

;one thing has its handles in its own frame. A box turned an eighth of a turn
(defq wb_doc2 (. wb_board :get_doc) wb_turned (. wb_board :add (cwb-shape (cwb-d-rect 0 0 100 40) :fill 0xff00ff00 :stroke 0
	:m (Mat3x2-mul (Mat3x2-translate 400 400) (Mat3x2-rot (/ +fp_pi 4.0))))))
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

;a thing's place is where it is, how far it is turned and sized: its matrix
;is worked out from that and kept, never saved, and nothing slants
(defun wb-square? (m) (wb-near? (+ (* (elem-get m 0) (elem-get m 1)) (* (elem-get m 3) (elem-get m 4))) 0.0 0.001))
(defq wb_sh (cwb-shape (cwb-d-rect 0 0 100 40) :m (Mat3x2 1.0 0.6 10.0 0.0 1.0 20.0)))
(assert-true "a matrix that slants, given as one, is a place, and its slant is left out"
	(and (wb-near? (cwb-place wb_sh) '(10.0 20.0 0.0 1.0 1.0) 0.001) (wb-square? (elem-get wb_sh +cwb_m))))
;turned there and back a thousand times, about a point, it is where it was
(defq wb_p (list 200.0 100.0 0.25 1.5 0.5))
(times 1000 (setq wb_p (place-turn (place-turn wb_p 0.37 260.0 130.0) -0.37 260.0 130.0)))
(assert-true "turned there and back a thousand times its angle and size are what they were, and it has not moved"
	(and (= (third wb_p) 0.25) (= (elem-get wb_p 3) 1.5) (= (elem-get wb_p 4) 0.5)
		(wb-near? (slice wb_p 0 2) '(200.0 100.0) 0.05)))
;with angles that snap, it is its angle that snaps, so it can go back to square
(defq wb_tilt (. wb_board :add (cwb-shape (cwb-d-rect 0 0 100 40) :fill 0xff00ff00 :stroke 0
	:place (list 500.0 500.0 0.1 1.0 1.0))))
(. wb_board :select (list wb_tilt))
(def wb_board :snap_angle (/ +fp_pi 12.0))
(bind '(wb_tx wb_ty) (slice (last (. wb_handles :spots)) 1 3))
(wb-drag 0 :mouse 1 (list (list wb_tx wb_ty) (list (+ wb_tx 1.0) wb_ty)))
(assert-eq "a thing at a tenth of a radian, turned a little with angles that snap, is square" 0.0
	(third (cwb-place (first (cwb-find (. wb_board :get_doc) wb_tilt)))))
(def wb_board :snap_angle 0.0)
(. wb_board :select (list))
(. wb_board :undo) (. wb_board :undo)
;a file has the place, not the matrix
(defq wb_f (cwb-doc 200 200))
(cwb-add wb_f (cwb-shape (cwb-d-rect 0 0 10 10) :place (list 30.0 40.0 0.5 2.0 1.0)))
(defq wb_fs (string-stream (cat "")))
(cwb-save wb_f wb_fs)
(assert-list-eq "saved, it says where it is, its angle and its size, and no matrix" '(:t :t :t :nil)
	(map (# (nempty? (substr (str wb_fs) %0))) '(":pos" ":angle" ":scale" ":m ")))
(assert-true "and read back it is where it was"
	(wb-near? (cwb-place (first (cwb-items (cwb-load (string-stream (str wb_fs)))))) '(30.0 40.0 0.5 2.0 1.0) 0.001))
;a group sized more one way than the other, with a thing turned in it:
;ungrouped, the thing would slant. It is made what it draws, slanted, and
;its place does not slant
(defq wb_u (Board (cwb-doc 400 300)))
(defq wb_ug (. wb_u :add (cwb-group (list (cwb-shape (cwb-d-rect -30 -10 30 10) :fill 0xff0000ff :stroke 0xffff0000 :width 4
	:place (list 0.0 0.0 0.5 1.0 1.0))) :place (list 200.0 150.0 0.0 2.0 1.0))))
(defun wb-ink-box (items)
	;the box round what is drawn, its polygons where they are, not round
	;their boxes
	(defq polys (list))
	(cwb-walk items :nil (lambda (shape m)
		(bind '(fills strokes &ignore) (cwb-flat shape))
		(each (# (push polys %0)) (cwb-mat-paths m (cat fills strokes)))))
	(bind '((x y) (x1 y1)) (vector-bounds-2d polys))
	(list x y x1 y1))
(defq wb_ub (wb-ink-box (cwb-items (. wb_u :get_doc))))
(. wb_u :select (list wb_ug))
(. wb_u :ungroup)
(defq wb_ui (cwb-items (. wb_u :get_doc)))
(assert-true "ungrouped, what would slant is its fill and its line, each its own, none slanted, and where it was"
	(and (= (length wb_ui) 2) (every (# (wb-square? (elem-get %0 +cwb_m))) wb_ui)
		(wb-near? (wb-ink-box wb_ui) wb_ub 0.05)))
(assert-eq "and both are selected" 2 (length (. wb_u :get_selected)))

;several things selected, a loose group, are not a thing: a box round
;them square to the board, and no handles to size or turn them. They are
;moved together, by a press on one of them, never by one in their box
(defq wb_sa (. wb_board :add (cwb-shape (cwb-d-rect 300 300 400 340) :fill 0xff00ff00 :stroke 0))
	wb_sb (. wb_board :add (cwb-shape (cwb-d-rect 300 360 400 400) :fill 0xff00ff00 :stroke 0))
	wb_sd (. wb_board :add (cwb-shape (cwb-d-line 0 0 100 0) :width 4
		:m (Mat3x2-mul (Mat3x2-translate 600 600) (Mat3x2-rot (/ +fp_pi 4.0))))))
(defun wb-of (id) (cwb-bounds (list (first (cwb-find (. wb_board :get_doc) id)))))
(. wb_board :select (list wb_sa wb_sb))
(assert-true "two things selected have a box square to the board, and no handles"
	(and (wb-near? (slice (. wb_handles :corners) 0 4) '(300 300 400 300) 0.01) (empty? (ifn (. wb_handles :spots) (list)))))
(. wb_board :select (list wb_sa wb_sd))
(assert-true "the box round several is round the line drawn round each, a turned one's corners too"
	(progn (bind '(fm (x y x1 y1)) (. wb_handles :frame))
		(every (lambda ((cx cy)) (and (<= (- x 0.01) cx (+ x1 0.01)) (<= (- y 0.01) cy (+ y1 0.01))))
			(partition (board-item-corners (first (cwb-find (. wb_board :get_doc) wb_sd))) 2))))
(. wb_board :select (list wb_sa wb_sb))
(defq wb_a0 (wb-of wb_sa) wb_b0 (wb-of wb_sb))
(wb-drag 0 :mouse 1 '((350 350) (352 352)))
(assert-true "a press in their box between them is on nothing: a box dragged there touches nothing, and all is put down"
	(and (empty? (. wb_board :get_selected)) (wb-near? (wb-of wb_sa) wb_a0 0.01)))
(. wb_board :select (list wb_sa wb_sb))
(wb-drag 0 :mouse 1 '((320 320) (330 325)))
(assert-true "a press on one of them moves them all"
	(and (wb-near? (wb-of wb_sa) (map (# (+ %0 %1)) wb_a0 '(10.0 5.0 10.0 5.0)) 0.01) (wb-near? (wb-of wb_sb) (map (# (+ %0 %1)) wb_b0 '(10.0 5.0 10.0 5.0)) 0.01)))
(defq wb_a0 (wb-of wb_sa) wb_b0 (wb-of wb_sb))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 320 320)))
(. wb_board :pointers (list (ptr-event 0x1000008 :touch 1 380 330)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 1 300 310) (ptr-event 0x1000008 :touch 1 420 360)))
(. wb_board :pointers (list (ptr-event 0x1000007 :touch 0 300 310) (ptr-event 0x1000008 :touch 0 420 360)))
(assert-true "two fingers on them that move apart and turn only move them, by their middle"
	(and (wb-near? (wb-of wb_sa) (map (# (+ %0 %1)) wb_a0 '(10.0 10.0 10.0 10.0)) 0.01) (wb-near? (wb-of wb_sb) (map (# (+ %0 %1)) wb_b0 '(10.0 10.0 10.0 10.0)) 0.01)))
(. wb_board :undo) (. wb_board :undo) (. wb_board :undo)
(assert-eq "an undo puts down what was selected" 0 (length (. wb_board :get_selected)))
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
(. wb_board :transform (Mat3x2-translate 0 100))
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
	(map! (const n2i) (list (wb-box)) 0 2))
(wb-drag 0 :mouse 1 '((430 220) (436 225)))
(assert-list-eq "moved a little more it stays there" '(416 208) (map! (const n2i) (list (wb-box)) 0 2))
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

;a line as it is drawn snaps its angle too, and with the grid the grid
;comes first, Chris: "snap to grid has to to priority"
(def wb_board :snap_angle (/ +fp_pi 12.0))
(wb-drag 0 :mouse 1 '((100 100) (150 103) (200 104)))
(assert-true "drawn nearly level with angles that snap it is level, as long as it was pulled"
	(progn (defq wb_parts (map (const str-to-num) (filter (# (not (find %0 '("M" "L")))) (split (cwb-get (last (cwb-items (. wb_board :get_doc))) :d) " "))))
		(and (wb-near? (elem-get wb_parts 3) 100.0 0.05) (wb-near? (elem-get wb_parts 2) (+ 100.0 (sqrt (+ (* 100.0 100.0) 16.0))) 0.05))))
(. wb_board :undo)
(wb-drag 0 :mouse 1 '((100 100) (150 130) (186 150)))
(assert-true "drawn near a twelfth of a turn from level it is at that step"
	(progn (defq wb_parts (map (const str-to-num) (filter (# (not (find %0 '("M" "L")))) (split (cwb-get (last (cwb-items (. wb_board :get_doc))) :d) " ")))
			wb_len (sqrt (+ (* 86.0 86.0) (* 50.0 50.0))))
		(and (wb-near? (elem-get wb_parts 2) (+ 100.0 (* wb_len (cos (/ +fp_pi 6.0)))) 0.05)
			(wb-near? (elem-get wb_parts 3) (+ 100.0 (* wb_len (sin (/ +fp_pi 6.0)))) 0.05))))
(. wb_board :undo)
(def wb_board :snap 16.0)
(wb-drag 0 :mouse 1 '((21 39) (60 70) (99 125)))
(assert-eq "with the grid too, the step is every 45 and the end the point of the grid along it nearest" "M 16 32 L 112 128"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(. wb_board :undo)
(wb-drag 0 :mouse 1 '((21 39) (60 45) (99 50)))
(assert-eq "and nearly level, level on the grid" "M 16 32 L 96 32"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(def wb_board :snap 0.0 :snap_angle 0.0)
(. wb_board :undo)

;snap is each device's, Chris: "per device, same as ink and width". The
;board's is off, the mouse's left button is bound to snap and a pen not,
;the grid the paper has is 16
(def wb_board :snap_gap 16.0)
(. (. wb_board :get_bindings) :set :mouse 0 +pev_left :snap :t)
(wb-drag 0 :mouse 1 '((21 39) (99 71)))
(assert-eq "the mouse bound to snap snaps to the grid" "M 16 32 L 96 64"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(wb-drag 0x2000000 :pen 1 '((21 39) (99 71)))
(assert-eq "a pen bound to none has the board's, and does not" "M 21 39 L 99 71"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(def wb_board :snap 16.0)
(. (. wb_board :get_bindings) :set :pen (ptr-device 0x2000000) +pev_left :snap :nil)
(wb-drag 0x2000000 :pen 1 '((21 39) (99 71)))
(assert-eq "one bound to no snap does not, whatever the board's" "M 21 39 L 99 71"
	(cwb-get (last (cwb-items (. wb_board :get_doc))) :d))
(assert-eq "what each pointer had is let go of as it comes up" 0 (. (get :settings wb_board) :size))
(def wb_board :snap 0.0 :snap_gap 0.0)
(. (. wb_board :get_bindings) :unbind :mouse 0 +pev_left)
(. (. wb_board :get_bindings) :unbind :pen (ptr-device 0x2000000) +pev_left)
(. wb_board :undo) (. wb_board :undo) (. wb_board :undo)

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

;rubbing out part of a line, a line cut and lines still, :rub_mode :part
(defq rb_board (Board (cwb-doc 400 300)) rb_doc (. rb_board :get_doc))
(assert-eq "a board takes a bite out of all it rubs, lines too, unless it is told" :bite (get :rub_mode rb_board))
(def rb_board :rub_mode :part)
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
	'((:butt :round) (:round :arrow)) (map! (# (list (cwb-get %0 :cap1) (cwb-get %0 :cap2))) (list (cwb-items rb_doc)) 0 2))
(assert-list-eq "what keeps a copy in step is told of the new ones" '(3 4) (. rb_board :take_changes))
(assert-eq "rubbed where there is nothing, nothing is" :nil (. rb_board :rub 100 200 10))
(. rb_board :rub 0 100 10)
(assert-eq "rubbed at an end, the end is shorter" "M 12 100 L 88 100" (first (rb-ds)))
(. rb_board :rub_along 12 100 88 100 10)
(assert-list-eq "rubbed all along, it is gone" (list "M 112 100 L 200 100" (cwb-d-rect 250 50 350 150)) (rb-ds))
;:lines, the eraser that only cuts lines: what is not a line goes whole
(def rb_board :rub_mode :lines)
(assert-list-eq "on the box, with no line there, the box goes whole" '(:t 1) (list (. rb_board :rub 300 100 10) (length (cwb-items rb_doc))))
;a line that has been moved and made twice the size
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-line 0 0 100 0) :width 2
	:m (Mat3x2-mul (Mat3x2-translate 50 200) (Mat3x2-scale 2.0))))
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
;:part: a line is cut and is lines still, anything else has a bite out
(def rb_board :rub_mode :part)
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xff00ff00))
(. rb_board :add (cwb-shape "M 50 250 L 250 250" :width 4))
(assert-eq "a filled box rubbed at its side" :t (. rb_board :rub 100 150 10))
(assert-eq "has a bite out, and is flat: its fill and its line, a shape each, no group of them" 3 (length (cwb-items rb_doc)))
(assert-true "it is where it was" (wb-near? (cwb-bounds (slice (cwb-items rb_doc) 0 2)) '(100 100 200 200) 3.0))
(assert-true "and has no line of its own now, each is a fill" (some! (# (= (elem-get %0 +cwb_stroke) 0)) (list (cwb-items rb_doc)) :t 0 2))
(. rb_board :rub_along 150 240 150 260 6)
(assert-list-eq "the line, rubbed across, is two lines, with a line's width still" '(4 4)
	(map (# (n2i (elem-get %0 +cwb_width))) (filter (# (and (cwb-shape? %0) (/= (elem-get %0 +cwb_stroke) 0))) (cwb-items rb_doc))))
(. rb_board :rub_along 90 120 210 120 6)
(assert-true "rubbed right across the box, it is in two, above and below"
	(>= (length (filter (# (= (elem-get %0 +cwb_stroke) 0)) (cwb-items rb_doc))) 2))
;the eraser itself, a pointer
(. rb_board :clear)
(. rb_board :add (cwb-shape (cwb-d-line 100 20 100 280) :width 4))
(. rb_board :add (cwb-shape (cwb-d-line 200 20 200 280) :width 4))
(defq rb_steps (length (get :undo_stack rb_board)))
(. rb_board :pointers (list (ptr-event 2 :eraser 1 40 150)))
(. rb_board :pointers (list (ptr-event 2 :eraser 1 260 150)))
(assert-true "while it rubs, the eraser is to be drawn where it is, as big as it is: three times the width the board draws with" (wb-near? (get :rubber rb_board) '(260 150 9) 0.01))
(defq rb_canvas (Canvas 400 300 1))
(.-> rb_canvas (:set_canvas_flags +canvas_flag_antialias) (:fill 0))
(. rb_board :draw_overlay rb_canvas)
(assert-list-eq "and is drawn: pale inside, a dash of grey at its edge where the dashes begin, nothing beyond"
	(list :t :t 0) (list (/= (wb-pixel rb_canvas 260 150) 0) (/= (wb-pixel rb_canvas 269 150) (wb-pixel rb_canvas 260 150)) (wb-pixel rb_canvas 280 150)))
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
(cwb-add ft_doc (cwb-group (list (cwb-shape (cwb-d-rect 0 0 50 40) :fill 0xff00ff00 :stroke 0)) :m (Mat3x2-translate 500 300)))
(assert-list-eq "fitted, all of it is moved so the box round it is 16 in from the top left" '(-284 -184) (map (const n2i) (cwb-fit ft_doc)))
(assert-list-eq "the box round it is then 16 in" '(16 16 266 156) (map (const n2i) (cwb-bounds (cwb-items ft_doc))))
(assert-list-eq "and the paper is that box and 16 all round" '(282 172) (list (. ft_doc :find :width) (. ft_doc :find :height)))
(assert-eq "a shape is moved by its matrix, it is the shape it was" (cwb-d-rect 300 200 400 260) (cwb-get (first (cwb-items ft_doc)) :d))
(assert-list-eq "fitted again it is not moved" '(0 0) (map (const n2i) (cwb-fit ft_doc)))
(cwb-add ft_doc (cwb-shape (cwb-d-line -40 -30 20 20) :width 4))
(cwb-fit ft_doc 0)
(assert-list-eq "what is off the top left is brought on, with no room round it if none is asked for" '(0 0 308 188)
	(cat (map! (const n2i) (list (cwb-bounds (cwb-items ft_doc))) 0 2) (list (. ft_doc :find :width) (. ft_doc :find :height))))
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
(. bk_board :select (list))
(def bk_board :mode :select)
(defq bk_steps (length (get :undo_stack bk_board)))
(bk-tap +pev_left :mouse 220 100)
(assert-list-eq "the left button, set to select, takes a thing and brings it to the front" '(1 2 3) (bk-ids))
(assert-eq "which is a step" (inc bk_steps) (length (get :undo_stack bk_board)))
(bk-tap +pev_left :mouse 220 100)
(assert-list-eq "one that is at the front already is left there, and that is no step" (list '(1 2 3) (inc bk_steps))
	(list (bk-ids) (length (get :undo_stack bk_board))))
(bk-tap +pev_left :touch 80 100)
(assert-list-eq "a finger brings what it takes to the front too" '(2 3 1) (bk-ids))
;moved by the right button it is drawn in its place, at the back, not over the rest
(. bk_board :select (list))
(. bk_board :dirty? (+ +board_dirty_doc +board_dirty_overlay))
(. bk_board :pointers (list (ptr-event 0 :mouse +pev_right 80 100)))
(. bk_board :pointers (list (ptr-event 0 :mouse +pev_right 120 130)))
(assert-list-eq "a thing dragged by the right button is at the back as it goes, in flight, and its flight is behind the document"
	(list '(1 2 3) '(1) :t) (list (bk-ids) (get :floating bk_board) (get :float_back bk_board)))
(. bk_board :pointers (list (ptr-event 0 :mouse 0 120 130)))
(assert-list-eq "and is at the back when it is let go, where it was dragged to" '((1 2 3) (90 80 190 180))
	(list (bk-ids) (map (const n2i) (cwb-bounds (slice (cwb-items bk_doc) 0 1)))))
(. bk_board :select (list))
(. bk_board :pointers (list (ptr-event 0 :mouse +pev_left 240 100)))
(. bk_board :pointers (list (ptr-event 0 :mouse +pev_left 250 110)))
(assert-list-eq "one dragged by the left button is at the front, in flight in front of the document" '((1 2 3) (3) :nil :t)
	(list (bk-ids) (get :floating bk_board) (get :float_back bk_board) (. bk_board :in_flight?)))
(. bk_board :pointers (list (ptr-event 0 :mouse 0 250 110)))
;a selection only sitting there with something drawn over it after, here or by a board shared with, lands in its place, under that
(. bk_board :select (list 3))
(assert-list-eq "selected and on top, it is in flight" '(3) (get :floating bk_board))
(. bk_board :add (cwb-shape (cwb-d-rect 180 60 260 140) :fill 0xff00ff00 :stroke 0))
(. bk_board :select (list 3))
(assert-list-eq "drawn over, it is in the document under what is over it, still selected" '(() (3)) (list (get :floating bk_board) (. bk_board :get_selected)))
(. bk_board :add (cwb-shape (cwb-d-rect 360 200 380 220) :fill 0xff00ff00 :stroke 0))
(. bk_board :select (list 4))
(assert-list-eq "what is drawn after it but not over it leaves it in flight" '(4) (get :floating bk_board))
(. bk_board :select (list))
;a short click selects in any mode, rubbing out too: a tap of the eraser on
;a thing picks it up, and again puts it down, and rubs nothing out
(def bk_board :mode :eraser)
(defun bk-look () (str (map (# (list (elem-get %0 +cwb_id) (cwb-bounds (list %0)))) (cwb-items bk_doc))))
(defq bk_look (bk-look))
(bk-tap +pev_left :mouse 370 210)
(assert-list-eq "a tap of the eraser on a thing selects it" '(5) (. bk_board :get_selected))
(bk-tap +pev_left :mouse 370 210)
(assert-list-eq "and again puts it down" '() (. bk_board :get_selected))
(assert-eq "and rubs nothing out" bk_look (bk-look))
;a tap is short as well as near, for a pen and an eraser alike, (board-tap?):
;held down longer, still, it is no tap, it rubs out, or draws
(defun bk-hold (buttons x y x1 y1)
	(. bk_board :tick 1000000) (. bk_board :pointers (list (ptr-event 0 :mouse buttons x y)))
	(. bk_board :tick (+ 1000000 +board_tap_time 1)) (. bk_board :pointers (list (ptr-event 0 :mouse buttons x1 y1)))
	(. bk_board :pointers (list (ptr-event 0 :mouse 0 x1 y1))))
(bk-hold +pev_left 370 210 371 211)
(assert-list-eq "the eraser held down on a thing longer than a tap selects nothing, and rubs it out" '(() :t)
	(list (. bk_board :get_selected) (nql bk_look (bk-look))))
(. bk_board :undo)
(def bk_board :mode :frect)
(defq bk_n (length (cwb-items bk_doc)))
(bk-hold +pev_left 370 210 371 211)
(assert-list-eq "and so a pen: it draws, and selects nothing" (list (list) (inc bk_n))
	(list (. bk_board :get_selected) (length (cwb-items bk_doc))))
(. bk_board :undo)
(. bk_board :tick 0)
(def bk_board :mode :select)

;what is written: a line by hand begun soon after the last was ended, by the same pen, is put in a group with it
(defq wr_board (Board (cwb-doc 600 400)) wr_doc (. wr_board :get_doc) wr_now 1000000)
(defun wr-stroke (id kind x y gap)
	;a short line by hand from a point, begun that long after the last thing, and it takes a tenth of a second
	(. wr_board :tick (setq wr_now (+ wr_now gap)))
	(. wr_board :pointers (list (ptr-event id kind 1 x y)))
	(. wr_board :pointers (list (ptr-event id kind 1 (+ x 10) (+ y 20))))
	(. wr_board :tick (setq wr_now (+ wr_now 100000)))
	(. wr_board :pointers (list (ptr-event id kind 1 (+ x 20) y)))
	(. wr_board :pointers (list (ptr-event id kind 0 (+ x 20) y))))
(defun wr-shape () (map (# (if (cwb-group? %0) (list (elem-get %0 +cwb_name) (length (elem-get %0 +cwb_items))) (elem-get %0 +cwb_kind))) (cwb-items wr_doc)))
(wr-stroke 1 :pen 50 50 0)
(assert-list-eq "one line by hand is a line" '(:pen) (wr-shape))
(wr-stroke 1 :pen 80 50 300000)
(assert-list-eq "another begun three tenths of a second after is a group with it" '(("written" 2)) (wr-shape))
(wr-stroke 1 :pen 110 50 1300000)
(wr-stroke 1 :pen 140 50 200000)
(assert-list-eq "and the next two join the group" '(("written" 4)) (wr-shape))
(assert-list-eq "they are where they were drawn" '(48 48 161 64) (map (const n2i) (cwb-bounds (cwb-items wr_doc))))
(wr-stroke 1 :pen 300 50 2500000)
(assert-list-eq "one begun two and a half seconds after is a line by itself, the next word" '(("written" 4) :pen) (wr-shape))
(wr-stroke 1 :pen 330 50 400000)
(assert-list-eq "and what follows that soon is a group with that" '(("written" 4) ("written" 2)) (wr-shape))
(defq wr_steps (length (get :undo_stack wr_board)))
(. wr_board :undo)
(assert-list-eq "to undo takes out the last line, and what was a group of two is the one line" '(("written" 4) :pen) (wr-shape))
(. wr_board :redo)
(assert-list-eq "and to do it again makes them a group" '(("written" 4) ("written" 2)) (wr-shape))
(wr-stroke 2 :pen 360 50 200000)
(assert-list-eq "another pen, as soon after, writes its own" '(("written" 4) ("written" 2) :pen) (wr-shape))
(def wr_board :mode :rect)
(wr-stroke 0 :mouse 400 300 100000)
(def wr_board :mode :pen)
(wr-stroke 0 :mouse 450 300 100000)
(assert-list-eq "a box is not written, nor is a line by hand joined to a box" '(("written" 4) ("written" 2) :pen :rect :pen) (wr-shape))
;a word that has been moved is done with
(. wr_board :select (list (elem-get (first (cwb-items wr_doc)) +cwb_id)))
(. wr_board :transform (Mat3x2-translate 0 200))
(. wr_board :select (list))
(def wr_board :mode :select)
(. wr_board :ungroup)
(def wr_board :mode :pen :auto_group 0)
(wr-stroke 1 :pen 50 350 100000) (wr-stroke 1 :pen 80 350 100000)
(assert-list-eq "with no time allowed nothing is put in a group" '(:pen :pen) (slice (wr-shape) -3 -1))
;the eraser on a word rubs the line it is on, not the word
(def wr_board :rub_mode :part)
(defq wr_word (first (cwb-items wr_doc)) wr_id (elem-get wr_word +cwb_id) wr_parts (length (elem-get wr_word +cwb_items)))
(defq wr_box (cwb-bounds (slice (elem-get wr_word +cwb_items) 0 1) (elem-get wr_word +cwb_m)))
(. wr_board :rub (* 0.5 (+ (first wr_box) (third wr_box))) (- (elem-get wr_box 3) 1.5) 6)
(assert-list-eq "the eraser on one line of a word that was written rubs that line, the word is still a group, of one more line"
	(list wr_id (inc wr_parts)) (list (elem-get (first (cwb-items wr_doc)) +cwb_id) (length (elem-get (first (cwb-items wr_doc)) +cwb_items))))
(defq wr_all (cwb-bounds (list (first (cwb-items wr_doc)))))
(. wr_board :rub_along (- (first wr_all) 5.0) (- (second wr_all) 5.0) (+ (third wr_all) 5.0) (- (second wr_all) 5.0) 60)
(assert-true "rubbed all over, the word is gone" (not (some (# (= (elem-get %0 +cwb_id) wr_id)) (cwb-items wr_doc))))
;several things selected each have their own box, a group that was broken up is seen to be its parts
(defq ug_board (Board (cwb-doc 400 300)))
(. ug_board :add (cwb-group (list (cwb-shape (cwb-d-rect 40 40 100 80) :fill 0xffff0000 :stroke 0)
	(cwb-shape (cwb-d-rect 200 100 260 140) :fill 0xff00ff00 :stroke 0 :m (Mat3x2-rot 0.5 230 120)))))
(def ug_board :mode :select)
(. ug_board :select (list 1))
(. ug_board :ungroup)
(assert-list-eq "a group that is ungrouped is its two parts, both selected" '((2 3) (2 3))
	(list (map (# (elem-get %0 +cwb_id)) (cwb-items (. ug_board :get_doc))) (. ug_board :get_selected)))
(assert-list-eq "the box of a thing is its own, where it is: of one that is level, and of one that has been turned, turned"
	'((40 40 100 40 100 80 40 80) :t)
	(list (map (const n2i) (board-item-corners (first (cwb-items (. ug_board :get_doc)))))
		(progn (defq ug_c (board-item-corners (second (cwb-items (. ug_board :get_doc))))) (/= (second ug_c) (elem-get ug_c 3)))))
(defq ug_canvas (Canvas 400 300 1))
(. ug_canvas :set_canvas_flags +canvas_flag_antialias)
(. ug_canvas :fill 0)
(. ug_board :draw_overlay ug_canvas :nil :t)
(defq ug_s (memory-stream)) (pixmap-write (getf ug_canvas +canvas_pixmap 0) ug_s 32) (stream-seek ug_s 0 0)
(defq ug_d (read-blk ug_s 10000000) ug_px (# (get-uint ug_d (+ (- (length ug_d) (* 400 300 4)) (* 4 (+ (* %1 400) %0))))))
(assert-true "with two selected a line is drawn round each, as well as round both" (and (/= 0 (ug_px 70 80)) (/= 0 (ug_px 100 60))))
(. ug_board :select (list 2))
(. ug_canvas :fill 0)
(. ug_board :draw_overlay ug_canvas :nil :t)
(pixmap-write (getf ug_canvas +canvas_pixmap 0) (setq ug_s (memory-stream)) 32) (stream-seek ug_s 0 0) (setq ug_d (read-blk ug_s 10000000))
(assert-true "with one selected there is the one line round it" (/= 0 (ug_px 70 80)))

;many at once. A board of hundreds of things, hundreds of them selected:
;what is asked of each is asked of a set, and many are taken out in a pass
(defq cm_doc (cwb-doc 800 600) cm_board (Board cm_doc))
(each (# (cwb-add cm_doc (cwb-shape (cwb-d-rect (* %0 2) 10 (+ (* %0 2) 1) 20)))) (range 0 300))
(defq cm_all (map (# (elem-get %0 +cwb_id)) (cwb-items cm_doc)) cm_some (slice cm_all 100 250))
(assert-list-eq "a few ids are asked of as the list they are, many as a set, and a set is left a set"
	'(:nil :t :t) (map (# (if (Fset? %0) :t)) (list (cwb-id-set (slice cm_all 0 5)) (cwb-id-set cm_some) (cwb-id-set (cwb-id-set cm_some)))))
(assert-list-eq "an id is one of them or is not, whichever they are asked of"
	'(:t :nil :t :nil) (map (# (if %0 :t)) (list (cwb-id? (cwb-id-set cm_some) (first cm_some)) (cwb-id? (cwb-id-set cm_some) (first cm_all))
		(cwb-id? (slice cm_all 0 5) (first cm_all)) (cwb-id? (slice cm_all 0 5) (last cm_all)))))
(assert-list-eq "the items that are of the ids, and those that are not, in the order they were in"
	(list 150 150 (first cm_some) (last cm_some) (first cm_all))
	(progn (defq cm_in (cwb-pick (cwb-items cm_doc) cm_some) cm_out (cwb-pick (cwb-items cm_doc) cm_some :t))
		(list (length cm_in) (length cm_out) (elem-get (first cm_in) +cwb_id) (elem-get (last cm_in) +cwb_id) (elem-get (first cm_out) +cwb_id))))
(. cm_board :select cm_some)
(assert-eq "150 of 300 selected are the 150" 150 (length (. cm_board :selected_items)))
(defq cm_group (. cm_board :group))
(assert-list-eq "grouped, they are one thing of 150, where the top one of them was, and 151 things are on the board"
	(list 151 150 100) (list (length (cwb-items cm_doc)) (length (elem-get (first (cwb-find cm_doc cm_group)) +cwb_items))
		(some (# (if (= (elem-get %0 +cwb_id) cm_group) (!))) (cwb-items cm_doc))))
;one inside the group and many outside it, taken out together
(cwb-remove-ids cm_doc (cat (slice cm_all 0 50) (list (elem-get cm_some 7))))
(assert-list-eq "many taken out at once go, those of a layer and one that is in a group"
	(list 101 149) (list (length (cwb-items cm_doc)) (length (elem-get (first (cwb-find cm_doc cm_group)) +cwb_items))))
(. cm_board :ungroup)
(assert-list-eq "broken up, its 149 are on the board again and selected" (list 249 149)
	(list (length (cwb-items cm_doc)) (length (. cm_board :selected_items))))
;each of them has a line round it, however many, so it is seen they are apart
(defun cm-row (canvas y x x1)
	;how many pixels of a row of a canvas, from x to x1, have anything on them
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(bind '(w h) (. canvas :pref_size))
	(defq d (read-blk stream 10000000) at (+ (- (length d) (* w h 4)) (* 4 (* y w))))
	(length (filter (# (/= (get-uint d (+ at (* 4 %0))) 0)) (range x x1))))
(defq cm_canvas (Canvas 800 600 1))
(. cm_canvas :fill 0)
(. cm_board :draw_overlay cm_canvas :nil :t)
(defq cm_apart (cm-row cm_canvas 15 230 470))
(. cm_board :group)
(. cm_canvas :fill 0)
(. cm_board :draw_overlay cm_canvas :nil :t)
(defq cm_one (cm-row cm_canvas 15 230 470))
(. cm_board :ungroup)
(assert-true (cat "149 things selected each have a line round them, " (str cm_apart) " pixels of a row through them are on one, and as one group none is, " (str cm_one))
	(and (> cm_apart 100) (= cm_one 0)))
(. cm_board :delete)
(assert-list-eq "deleted, they are gone and nothing is selected, and undo has them back" (list 100 0 249)
	(list (length (cwb-items cm_doc)) (length (. cm_board :get_selected)) (progn (. cm_board :undo) (length (cwb-items (. cm_board :get_doc))))))

;where each finger is on the board is kept while it is down, and shown
(defq ct_board (Board (cwb-doc 400 300)) ct_canvas (Canvas 400 300 1))
(. ct_board :dirty? +board_dirty_overlay)
(. ct_board :pointers (list (ptr-event 0x1010001 :touch 1 100 100) (ptr-event 0x1010002 :touch 1 300 200) (ptr-event 0 :mouse 0 50 50)))
(assert-list-eq "two fingers down are two places kept, a mouse is not one, and what is over the board is to be drawn"
	'(2 :t) (list (. (get :contacts ct_board) :size) (. ct_board :dirty? +board_dirty_overlay)))
(. ct_canvas :fill 0) (. ct_board :draw_actors ct_canvas)
(assert-list-eq "a ring is drawn where each is, and not where neither is" '(:t :t :nil)
	(list (> (cm-row ct_canvas 100 80 120) 10) (> (cm-row ct_canvas 200 280 320) 10) (> (cm-row ct_canvas 100 180 220) 0)))
(. ct_board :pointers (list (ptr-event 0x1010001 :touch 1 120 100) (ptr-event 0x1010002 :touch 0 300 200)))
(assert-list-eq "one moved and one lifted: one place, where it now is" '(1 (120 100))
	(list (. (get :contacts ct_board) :size) (map (const n2i) (. (get :contacts ct_board) :find 0x1010001))))
(def ct_board :show_contacts :nil)
(. ct_canvas :fill 0) (. ct_board :draw_actors ct_canvas)
(assert-eq "with them not to be shown, none is" 0 (cm-row ct_canvas 100 80 160))

;a pointer belongs to what it went down on, till it comes up. Many fingers
;on the board at once, each on its own thing, are none of them in the way
;of the rest
(defq mh_doc (cwb-doc 800 600) mh_board (Board mh_doc)
	mh_a (elem-get (cwb-add mh_doc (cwb-shape (cwb-d-rect 100 100 160 160) :fill 0xff0000ff :stroke 0)) +cwb_id)
	mh_b (elem-get (cwb-add mh_doc (cwb-shape (cwb-d-rect 400 100 460 160) :fill 0xffff0000 :stroke 0)) +cwb_id)
	mh_c (elem-get (cwb-add mh_doc (cwb-shape (cwb-d-rect 100 400 160 460) :fill 0xff00ff00 :stroke 0)) +cwb_id))
(defun mh-go (&rest events) (. mh_board :pointers (map (# (apply (const ptr-event) %0)) events)))
(defun mh-at (id) (map! (const n2i) (list (cwb-bounds (list (first (cwb-find (. mh_board :get_doc) id))))) 0 2))
(defun mh-sel () (sort (cat (. mh_board :get_selected)) (const -)))
(def mh_board :mode :select)
;one finger on the first, then another on the second, and each is moved
(mh-go '(0x1000001 :touch 1 130 130))
(mh-go '(0x1000002 :touch 1 430 130))
(assert-list-eq "a finger on one thing and another on a second: both are selected, each has hold of its own"
	(list mh_a mh_b) (mh-sel))
(mh-go '(0x1000001 :touch 1 130 230) '(0x1000002 :touch 1 530 130))
(assert-list-eq "each goes with its own finger, one down and one across, and the third stays"
	'((100 200) (500 100) (100 400)) (list (mh-at mh_a) (mh-at mh_b) (mh-at mh_c)))
;a third finger drags a box out on nothing, round the third thing, while they hold theirs
(mh-go '(0x1000003 :touch 1 80 380))
(mh-go '(0x1000003 :touch 1 180 480) '(0x1000001 :touch 1 140 230))
(assert-list-eq "a third finger drags a box out on nothing while they do: what they have is selected still, the box is its own, and the first goes on moving"
	(list (list mh_a mh_b) 1 '(110 200)) (list (mh-sel) (. (get :bands mh_board) :size) (mh-at mh_a)))
;and a fourth another, at the same time
(mh-go '(0x1000004 :touch 1 600 400))
(mh-go '(0x1000004 :touch 1 700 500))
(assert-eq "a fourth drags another box, there are two" 2 (. (get :bands mh_board) :size))
(mh-go '(0x1000004 :touch 0 700 500))
(mh-go '(0x1000003 :touch 0 180 480))
(assert-list-eq "the third lifted: what was in its box is selected with what the two have hold of, and no box is left"
	(list (list mh_a mh_b mh_c) 0) (list (mh-sel) (. (get :bands mh_board) :size)))
;the first finger lifts, the second has hold of its own still
(mh-go '(0x1000001 :touch 0 140 230))
(assert-list-eq "the first lifted: what it had is put down and is not selected, the rest is"
	(list mh_b mh_c) (mh-sel))
(mh-go '(0x1000002 :touch 1 530 180))
(assert-list-eq "and the second goes on with its own, and with nothing else" '((110 200) (500 150) (100 400))
	(list (mh-at mh_a) (mh-at mh_b) (mh-at mh_c)))
;a second finger on the same thing, the two of them size it
(mh-go '(0x1000005 :touch 1 550 200))
(mh-go '(0x1000002 :touch 1 510 160) '(0x1000005 :touch 1 570 220))
(bind '(mh_x mh_y mh_x1 mh_y1) (map (const n2i) (cwb-bounds (list (first (cwb-find mh_doc mh_b))))))
(assert-true "a second finger on the thing the first has hold of: between them they make it bigger"
	(and (> (- mh_x1 mh_x) 100) (> (- mh_y1 mh_y) 100)))
(mh-go '(0x1000005 :touch 0 570 220))
(mh-go '(0x1000002 :touch 1 520 170))
(assert-list-eq "one of the two lifted, the other moves it on from where it is" (list (+ mh_x 10) (+ mh_y 10)) (mh-at mh_b))
(mh-go '(0x1000002 :touch 0 520 170))
(assert-list-eq "the last finger up: what it had is selected still" (list mh_b mh_c) (mh-sel))
;a finger draws nothing and lets go of nothing that a mouse has hold of
(. mh_board :select (list))
(mh-go '(0 :mouse 1 130 430))
(mh-go '(0x1000007 :touch 1 600 300))
(mh-go '(0 :mouse 1 230 430) '(0x1000007 :touch 1 650 350))
(assert-list-eq "a mouse has hold of a thing and a finger goes down on nothing: the mouse has it still, and moves it"
	(list (list mh_c) '(200 400)) (list (mh-sel) (mh-at mh_c)))
(mh-go '(0x1000007 :touch 0 650 350))
(mh-go '(0 :mouse 0 230 430))

;a pointer is a device and a contact of it, and what a pointer is bound to is its device's
(assert-list-eq "the mouse is device 0, a pen named by a small number is that device, and both are contact 0"
	'(0 0 9 0) (list (ptr-device 0) (ptr-contact 0) (ptr-device 9) (ptr-contact 9)))
(assert-list-eq "the third finger of the second device is contact 2 of it, and its device is the id of its contact 0"
	(list 0x2000000 2 0x2000000 0) (list (ptr-device 0x2000002) (ptr-contact 0x2000002) (ptr-device 0x2000000) (ptr-contact 0x2000000)))
(defq dv_bind (.-> (Bindings) (:bind :nil :nil :nil '(:color 1)) (:bind :touch (ptr-device 0x2000003) :nil '(:color 2))
	(:bind :pen (ptr-device 0x3000000) :nil '(:color 3))))
(assert-list-eq "what one panel is bound to is every finger of it, and no finger of another panel, and a pen its own"
	'(2 2 1 3 1) (map (# (. dv_bind :value %0 :color))
		(list (ptr-event 0x2000000 :touch 1 0 0) (ptr-event 0x2000007 :touch 1 0 0) (ptr-event 0x4000000 :touch 1 0 0)
			(ptr-event 0x3000000 :pen 1 0 0) (ptr-event 0x5000000 :pen 1 0 0))))

;out of select mode, one thing that has been turned has its own box round it, turned with it
(defq tb_doc (cwb-doc 400 300) tb_board (Board tb_doc)
	tb_id (elem-get (cwb-add tb_doc (cwb-shape (cwb-d-rect 150 130 250 170) :fill 0xff0000ff :stroke 0)) +cwb_id)
	tb_canvas (Canvas 400 300 1))
(. tb_board :select (list tb_id))
(. tb_board :transform (Mat3x2-rot (/ +fp_pi 4.0) 200.0 150.0))
(. tb_canvas :fill 0) (. tb_board :draw_overlay tb_canvas :nil :t)
;the box square to the board round it would have a line down x 147 or so, from y 97 to y 203. Its own box has none there
(assert-list-eq "the line round a thing turned an eighth of a turn is along its own sides, not a square box round it"
	'(:t :t) (list (= (cm-row tb_canvas 105 140 160) 0) (> (cm-row tb_canvas 150 140 260) 0)))

;two things held, each by its own finger, out of select mode: a line round each, and no box round the two
(defq tw_doc (cwb-doc 400 300) tw_board (Board tw_doc) tw_canvas (Canvas 400 300 1))
(cwb-add tw_doc (cwb-shape (cwb-d-rect 40 40 100 100) :fill 0xff0000ff :stroke 0))
(cwb-add tw_doc (cwb-shape (cwb-d-rect 300 200 360 260) :fill 0xffff0000 :stroke 0))
;with soft edges, as the app's canvases have: a line one pixel wide along whole numbers is not drawn without
(. tw_canvas :set_canvas_flags +canvas_flag_antialias)
(. tw_board :pointers (list (ptr-event 0x1000000 :touch 1 70 70)))
(. tw_board :pointers (list (ptr-event 0x1000001 :touch 1 330 230)))
(. tw_canvas :fill 0) (. tw_board :draw_overlay tw_canvas :nil :t)
;a box round both would have a line along the top, y 40, from x 100 to x 360, and down x 360 from y 40 to y 200
(assert-list-eq "each has its line, and there is none from the one to the other"
	'(:t :t 0 0) (list (> (cm-row tw_canvas 40 40 100) 30) (> (cm-row tw_canvas 200 300 360) 30)
		(cm-row tw_canvas 40 110 290) (cm-row tw_canvas 150 355 365)))

;a box dragged out selects what it touches, what is drawn, not the box round it
(defq tx_doc (cwb-doc 800 600))
(defq tx_ids (map (# (elem-get (cwb-add tx_doc %0) +cwb_id)) (list
	(cwb-shape (cwb-d-line 100 100 300 300) :stroke 0xff000000 :width 4.0)
	(cwb-shape (cwb-d-rect 400 100 600 300) :fill 0xff0000ff :stroke 0)
	(cwb-shape (cwb-d-rect 100 400 300 550) :stroke 0xff000000 :width 2.0)
	(cwb-shape (cwb-d-ellipse 600 450 80 60) :stroke 0xff000000 :width 2.0))))
(defun tx-touch (x y x1 y1) (map (# (elem-get (first %0) +cwb_id)) (cwb-touch-box tx_doc (list x y x1 y1))))
(bind '(tx_line tx_fill tx_box tx_oval) tx_ids)
(assert-list-eq "a box across the middle of a line touches it" (list tx_line) (tx-touch 180 190 220 210))
(assert-list-eq "a box in the corner of the box round the line, where the line is not, touches nothing" (list) (tx-touch 240 110 290 150))
(assert-list-eq "a box over the end of the line touches it" (list tx_line) (tx-touch 90 90 110 110))
(assert-list-eq "a box wholly inside a thing that is filled touches it" (list tx_fill) (tx-touch 480 180 520 220))
(assert-list-eq "a box wholly inside a box that is not filled touches nothing, its line is not there" (list) (tx-touch 150 440 250 510))
(assert-list-eq "a box across its side touches it" (list tx_box) (tx-touch 290 450 320 470))
(assert-list-eq "a box in the corner of the box round an ellipse, outside its line, touches nothing" (list) (tx-touch 522 392 532 400))
(assert-list-eq "a box round a thing touches it, as before, and one across two touches both"
	(list (list tx_oval) (list tx_line tx_fill)) (list (tx-touch 500 380 700 520) (tx-touch 280 150 420 290)))
;the board's own: a box dragged out with the hand
(defq tx_board (Board tx_doc))
(def tx_board :mode :select)
(. tx_board :pointers (list (ptr-event 0 :mouse 1 350 20)))
(. tx_board :pointers (list (ptr-event 0 :mouse 1 450 200)))
(. tx_board :pointers (list (ptr-event 0 :mouse 0 450 200)))
(assert-list-eq "a box dragged out over a corner of a thing selects it" (list tx_fill) (. tx_board :get_selected))
