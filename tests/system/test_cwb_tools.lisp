(report-header "Whiteboard tools: a ruler, a protractor and a set square that pens draw along and hands move")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/tools.inc")

(defun wt-near? (a b &optional tol)
	(setd tol 0.05)
	(if (list?? a)
		(and (list?? b) (= (length a) (length b)) (every (# (<= (abs (- (n2f %0) (n2f %1))) tol)) a b))
		(<= (abs (- (n2f a) (n2f b))) tol)))

(defq wt_board (Board (cwb-doc 800 600)) wt_ruler (Ruler wt_board 400 300) wt_stage (. wt_board :get_stage))
(. wt_stage :add wt_ruler)
(defun wt-ids () (map (# (elem-get %0 +cwb_id)) (cwb-items (. wt_board :get_doc))))
(defun wt-last-d () (cwb-get (last (cwb-items (. wt_board :get_doc))) :d))
(defun wt-go (&rest events)
	;a batch of events, each (id kind buttons x y)
	(. wt_board :pointers (map (# (apply (const ptr-event) %0)) events)))
(defun wt-numbers (d)
	;the numbers of a path
	(map (const str-to-num) (filter (# (not (find (first %0) "MLAZQC"))) (split d " "))))

;a ruler is 400 long and 80 high, about (400 300)
(assert-true "a point on the ruler is on it" (. wt_ruler :hit 400 300))
(assert-true "a point just off one of its long sides is near enough to draw along it" (. wt_ruler :hit 400 250))
(assert-eq "a point well off it is not" :nil (. wt_ruler :hit 400 200))
(assert-true "on the stage, a point on it is the ruler's" (eql wt_ruler (. wt_stage :hit 400 300)))
(assert-true "and a point off it is the surface's" (eql (get :surface wt_board) (. wt_stage :hit 100 100)))

;a pen that wobbles along just above its top side draws a straight line along that side
(wt-go '(1 :pen 1 300 255)) (wt-go '(1 :pen 1 350 250)) (wt-go '(1 :pen 1 420 262)) (wt-go '(1 :pen 1 500 252))
(wt-go '(1 :pen 0 500 252))
(assert-list-eq "a pen run along the ruler draws, and it is in the document" '(1) (wt-ids))
(assert-true "what it drew is the ruler's side, from where it went down to where it came up, and no wobble"
	(wt-near? (wt-numbers (wt-last-d)) '(300 260 500 260)))
(assert-eq "it is a line of two points, not a line drawn by hand" 2 (length (substr (wt-last-d) " 260")))

;two pens at once, one along each side
(wt-go '(1 :pen 1 250 262) '(2 :pen 1 250 345))
(assert-eq "two pens down on it, it has two" 2 (. wt_ruler :held))
(wt-go '(1 :pen 1 550 255) '(2 :pen 1 450 338))
(wt-go '(1 :pen 0 550 255) '(2 :pen 0 450 338))
(assert-list-eq "two pens at once draw two lines" '(1 2 3) (wt-ids))
(assert-true "one along each side" (and
	(wt-near? (wt-numbers (cwb-get (second (cwb-items (. wt_board :get_doc))) :d)) '(250 260 550 260))
	(wt-near? (wt-numbers (cwb-get (third (cwb-items (. wt_board :get_doc))) :d)) '(250 340 450 340))))
(assert-eq "and then it has none" 0 (. wt_ruler :held))

;a finger in its middle moves it
(wt-go '(7 :touch 1 500 300)) (wt-go '(7 :touch 1 520 340)) (wt-go '(7 :touch 0 520 340))
(assert-true "a finger on its middle moves it" (wt-near? (get :origin wt_ruler) '(420 340)))
(assert-list-eq "and draws nothing" '(1 2 3) (wt-ids))

;a finger holds it still while a pen draws along it: the pen does not move it
(wt-go '(7 :touch 1 520 340))
(wt-go '(1 :pen 1 300 302))
(wt-go '(1 :pen 1 400 296) '(7 :touch 1 520 340))
(assert-true "held by a finger, a pen drawing along it does not move it" (wt-near? (get :origin wt_ruler) '(420 340)))
;and the finger, no longer alone on it, does not move it either
(wt-go '(7 :touch 1 540 360))
(assert-true "nor does the finger, while it is not alone on it" (wt-near? (get :origin wt_ruler) '(420 340)))
(wt-go '(1 :pen 0 400 296) '(7 :touch 0 540 360))
(assert-true "the pen drew its line along the side where the ruler is" (wt-near? (wt-numbers (wt-last-d)) '(300 300 400 300)))

;two fingers on it move and turn it
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(7 :touch 1 340 300)) (wt-go '(8 :touch 1 460 300))
(wt-go '(7 :touch 1 400 240) '(8 :touch 1 400 360))
(wt-go '(7 :touch 0 400 240) '(8 :touch 0 400 360))
(assert-true "two fingers that turn a quarter turn about their middle turn it a quarter turn, where it is"
	(and (wt-near? (get :angle wt_ruler) +fp_hpi 0.001) (wt-near? (get :origin wt_ruler) '(400 300) 0.1)))

;the strip along a side turns it about the far corner of that side
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(defq wt_corner (. wt_ruler :to_board -184 -40))
(wt-go '(0 :mouse 1 500 280)) (wt-go '(0 :mouse 1 500 380)) (wt-go '(0 :mouse 0 500 380))
(assert-true "dragged by the strip along its top side, it turns" (> (get :angle wt_ruler) 0.2))
(assert-true "and the far corner of that side stays where it was" (wt-near? (. wt_ruler :to_board -184 -40) wt_corner 0.05))
(def wt_board :snap_angle (/ +fp_pi 12.0))
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(0 :mouse 1 500 280)) (wt-go '(0 :mouse 1 500 380)) (wt-go '(0 :mouse 0 500 380))
(assert-true "with angles that snap it is turned to a twelfth of a half turn" (wt-near? (get :angle wt_ruler) (* 1.0 (/ +fp_pi 12.0)) 0.001))
(def wt_board :snap_angle 0.0)

;a line along a ruler that is turned is turned
(def wt_ruler :origin (list 400.0 300.0) :angle (/ +fp_pi 4.0))
(defq wt_from (. wt_ruler :to_board -100 -46) wt_to (. wt_ruler :to_board 100 -45))
(wt-go (cat '(1 :pen 1) wt_from)) (wt-go (cat '(1 :pen 1) wt_to)) (wt-go (cat '(1 :pen 0) wt_to))
(bind '(x0 y0 x1 y1) (wt-numbers (wt-last-d)))
(assert-true "a line along a ruler turned an eighth of a turn goes down as far as it goes across"
	(wt-near? (- x1 x0) (- y1 y0) 0.05))

;an end makes it longer
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(0 :mouse 1 595 300)) (wt-go '(0 :mouse 1 695 300)) (wt-go '(0 :mouse 0 695 300))
(assert-true "dragged by an end it is longer" (wt-near? (get :length wt_ruler) 605.1 0.5))
(assert-true "and its sides are as long as it is" (wt-near? (elem-get (first (get :edges wt_ruler)) 3) (- (* 0.5 (get :length wt_ruler)) +ruler_cap) 0.01))
(wt-go '(0 :mouse 1 695 300)) (wt-go '(0 :mouse 1 5000 300)) (wt-go '(0 :mouse 0 5000 300))
(assert-eq "but no longer than it may be" 2000.0 (get :length wt_ruler))

;the ring in its middle puts it away, if the pointer comes up where it went down
(wt-go '(0 :mouse 1 400 300)) (wt-go '(0 :mouse 1 300 100)) (wt-go '(0 :mouse 0 300 100))
(assert-true "pressed on its ring and let go somewhere else, it is still there" (find wt_ruler (. wt_stage :get_actors)))
(wt-go '(0 :mouse 1 400 300)) (wt-go '(0 :mouse 0 400 300))
(assert-eq "pressed and let go on its ring, it is put away" :nil (find wt_ruler (. wt_stage :get_actors)))
(assert-true "and a point where it was is the surface's" (eql (get :surface wt_board) (. wt_stage :hit 400 300)))

;;;;;;;;;;;;;;;;
; the protractor
;;;;;;;;;;;;;;;;

(defq wt_pro (Protractor wt_board 400 300))
(. wt_stage :add wt_pro)
(defun wt-arc (mode degs)
	;a pen run round just outside the round side, through those degrees up from the right
	(def wt_pro :mode mode)
	(defq at :nil)
	(each (lambda (deg) (defq a (neg (* (n2f deg) (/ +fp_pi 180.0))))
		(setq at (list (+ 400.0 (* 186.0 (cos a))) (+ 300.0 (* 186.0 (sin a)))))
		(wt-go (cat '(1 :pen 1) at))) degs)
	;it comes up where it is
	(wt-go (cat '(1 :pen 0) at))
	(wt-last-d))
(defq wt_d (wt-arc :line '(10 20 45 70 90)))
(assert-true "a pen run round the protractor draws an arc, an A, of its radius" (found? wt_d "A 180 180"))
(bind '(x y x1 y1) (cwb-bounds (list (last (cwb-items (. wt_board :get_doc))))))
(assert-true "from 10 degrees round to straight up" (wt-near? (list x y x1 y1) '(398.5 118.5 578.8 270.3) 0.6))
(assert-true "a pie is that arc and the two lines to the middle"
	(progn (defq wt_d (wt-arc :pie '(10 20 45 70 90))) (and (starts-with "M 400 300 L" wt_d) (found? wt_d "A 180 180") (ends-with "Z" wt_d))))
(assert-eq "a circle is the whole circle" (cwb-d-ellipse 400 300 180) (wt-arc :circle '(10 20 45 70 90)))
(defq wt_before (length (wt-ids)))
(wt-arc :line '(30 30))
(assert-eq "a pen that goes down and comes up without going round draws nothing" wt_before (length (wt-ids)))
;round more than half way, the way it went
(defq wt_d (wt-arc :line '(170 120 90 45 10)))
(assert-true "an arc is drawn the way the pen went, clockwise too" (found? wt_d "A 180 180"))
(bind '(x y x1 y1) (cwb-bounds (list (last (cwb-items (. wt_board :get_doc))))))
(assert-true "from 170 degrees round over the top to 10" (wt-near? (list x y x1 y1) '(221.2 118.5 578.8 270.3) 0.6))
;its straight side is an edge too
(wt-go '(1 :pen 1 300 306)) (wt-go '(1 :pen 1 500 309)) (wt-go '(1 :pen 0 500 309))
(assert-true "a pen run along its straight side draws a line along it" (wt-near? (wt-numbers (wt-last-d)) '(300 300 500 300)))

;;;;;;;;;;;;;;;;
; the set square
;;;;;;;;;;;;;;;;

(. wt_stage :sub wt_pro)
(defq wt_set (Setsquare wt_board 400 300))
(. wt_stage :add wt_set)
(. wt_set :built)
(assert-eq "a set square has three edges" 3 (length (get :edges wt_set)))
(bind '(kind ax ay bx by) (elem-get (get :edges wt_set) 1))
(defq wt_from (. wt_set :to_board (+ (* 0.75 ax) (* 0.25 bx)) (- (+ (* 0.75 ay) (* 0.25 by)) 3.0))
	wt_to (. wt_set :to_board (+ (* 0.25 ax) (* 0.75 bx)) (- (+ (* 0.25 ay) (* 0.75 by)) 3.0)))
(wt-go (cat '(1 :pen 1) wt_from)) (wt-go (cat '(1 :pen 1) wt_to)) (wt-go (cat '(1 :pen 0) wt_to))
(bind '(x0 y0 x1 y1) (wt-numbers (wt-last-d)))
(assert-true "a line along its long side is at 30 degrees"
	(wt-near? (abs (/ (- y1 y0) (- x1 x0))) 0.57735 0.002))

;they draw themselves
(defq wt_canvas (Canvas 800 600 1))
(. wt_canvas :set_canvas_flags +canvas_flag_antialias)
(. wt_canvas :fill 0)
(. (Ruler wt_board 400 300) :draw wt_canvas)
(. (Protractor wt_board 400 300) :draw wt_canvas (cwb-mat-scale 0.5))
(. wt_set :draw wt_canvas)
(assert-true "each draws on a canvas" :t)
