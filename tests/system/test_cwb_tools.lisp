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

;turned, it is pulled to the angles that matter when it is near one
(defun wt-deg (d) (* (n2f d) (/ +fp_pi 180.0)))
(assert-list-eq "an angle near a 45 is the 45 from 4 degrees off, near a 10 the 10 from little more than 1, and else is as it is"
	'(45 45 90 0 40 30 30 33 45 12)
	(map (# (n2i (+ 0.5 (abs (/ (* (tool-magnet (wt-deg %0)) 180.0) +fp_pi))))) '(42 48.5 87 3.9 41 29 31.2 33 -47 12)))
(assert-true "a quarter turn that is pulled to is a quarter turn, as near as a number can say"
	(wt-near? (tool-magnet (wt-deg 88)) +fp_hpi 0.00005))
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
;about the 0 of its marks, 226 260, from the hole by its right end, to 43 degrees and to 20
(defun wt-turn-to (deg) (defq a (wt-deg deg))
	(wt-go '(0 :mouse 1 550 300))
	(wt-go (list 0 :mouse 1 (+ 226.0 (* 326.0 (cos (+ a 0.1229)))) (+ 260.0 (* 326.0 (sin (+ a 0.1229))))))
	(wt-go (list 0 :mouse 0 (+ 226.0 (* 326.0 (cos (+ a 0.1229)))) (+ 260.0 (* 326.0 (sin (+ a 0.1229))))))
	(defq got (get :angle wt_ruler))
	(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
	got)
(assert-true "a ruler turned to 43 degrees is at 45" (wt-near? (wt-turn-to 43) (wt-deg 45) 0.001))
(assert-true "turned to 20.8 it is at 20" (wt-near? (wt-turn-to 20.8) (wt-deg 20) 0.001))
(assert-true "and to 24 it is at 24, or as near as the hand was" (wt-near? (wt-turn-to 24) (wt-deg 24) 0.01))

;two fingers that go apart make it bigger, about their middle
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0 :size 1.0)
(wt-go '(7 :touch 1 350 300)) (wt-go '(8 :touch 1 450 300))
(wt-go '(7 :touch 1 325 300) '(8 :touch 1 475 300))
(wt-go '(7 :touch 0 325 300) '(8 :touch 0 475 300))
(assert-true "two fingers that go half as far apart again make it half as big again, where it is"
	(and (wt-near? (get :size wt_ruler) 1.5 0.002) (wt-near? (get :origin wt_ruler) '(400 300) 0.1) (wt-near? (get :angle wt_ruler) 0.0 0.001)))
(assert-true "all of it: a point of it is half as far again from its middle on the board"
	(wt-near? (. wt_ruler :to_board 100 -40) '(550 240) 0.01))
(def wt_ruler :size 1.0)

;the hole by an end turns it about the 0 of the marks at its other end, the
;top side's at the left, the bottom side's at the right. Put on a point,
;lines can be drawn from that point every way
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(defq wt_corner (. wt_ruler :to_board -174 -40))
(assert-list-eq "a ruler has a part to put it away, two to make it longer, two to turn it and one to move it"
	'(:close :size :size :turn :turn :move) (map (const first) (get :parts wt_ruler)))
(assert-eq "and a sign is drawn for each but the one that moves it" 5 (length (get :glyphs wt_ruler)))
(wt-go '(0 :mouse 1 550 300)) (wt-go '(0 :mouse 1 550 400)) (wt-go '(0 :mouse 0 550 400))
(assert-true "dragged by the hole by its right end, it turns" (> (get :angle wt_ruler) 0.2))
(assert-true "and the 0 of its marks stays where it was" (wt-near? (. wt_ruler :to_board -174 -40) wt_corner 0.05))
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(defq wt_corner (. wt_ruler :to_board 174 40))
(wt-go '(0 :mouse 1 250 300)) (wt-go '(0 :mouse 1 250 380)) (wt-go '(0 :mouse 0 250 380))
(assert-true "by the hole by its left end it turns the other way, about the 0 of the bottom side's marks, at its right end"
	(and (< (get :angle wt_ruler) -0.2) (wt-near? (. wt_ruler :to_board 174 40) wt_corner 0.05)))
(def wt_board :snap_angle (/ +fp_pi 12.0))
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(0 :mouse 1 550 300)) (wt-go '(0 :mouse 1 550 400)) (wt-go '(0 :mouse 0 550 400))
(assert-true "with angles that snap it is turned to a twelfth of a half turn" (wt-near? (get :angle wt_ruler) (* 1.0 (/ +fp_pi 12.0)) 0.001))
(def wt_board :snap_angle 0.0)

;a line along a ruler that is turned is turned
(def wt_ruler :origin (list 400.0 300.0) :angle (/ +fp_pi 4.0))
(defq wt_from (. wt_ruler :to_board -100 -46) wt_to (. wt_ruler :to_board 100 -45))
(wt-go (cat '(1 :pen 1) wt_from)) (wt-go (cat '(1 :pen 1) wt_to)) (wt-go (cat '(1 :pen 0) wt_to))
(bind '(x0 y0 x1 y1) (wt-numbers (wt-last-d)))
(assert-true "a line along a ruler turned an eighth of a turn goes down as far as it goes across"
	(wt-near? (- x1 x0) (- y1 y0) 0.05))

;an end makes it bigger, all of it, its marks and its numbers with it
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(0 :mouse 1 595 300)) (wt-go '(0 :mouse 1 695 300)) (wt-go '(0 :mouse 0 695 300))
(assert-true "dragged by an end, half as far again from its middle, it is half as big again" (wt-near? (get :size wt_ruler) 1.5128 0.003))
(assert-true "and is made as it was, it is seen bigger" (wt-near? (elem-get (first (get :edges wt_ruler)) 3) (- 200.0 +ruler_cap) 0.01))
(wt-go '(1 :pen 1 300 232)) (wt-go '(1 :pen 1 500 235)) (wt-go '(1 :pen 0 500 235))
(assert-true "a pen is held to its side where that now is" (wt-near? (wt-numbers (wt-last-d)) '(300 239.49 500 239.49) 0.3))
;bigger, it still turns about the 0 of its marks, where that now is
(defq wt_corner (. wt_ruler :to_board -174 -40) wt_hole (. wt_ruler :to_board 150 0))
(wt-go (cat '(0 :mouse 1) wt_hole)) (wt-go (list 0 :mouse 1 (first wt_hole) (+ (second wt_hole) 120.0)))
(wt-go (list 0 :mouse 0 (first wt_hole) (+ (second wt_hole) 120.0)))
(assert-true "a ruler that has been made bigger turns about the 0 of its marks, where that is on the board"
	(and (> (get :angle wt_ruler) 0.2) (wt-near? (. wt_ruler :to_board -174 -40) wt_corner 0.05)))
(def wt_ruler :origin (list 400.0 300.0) :angle 0.0)
(wt-go '(0 :mouse 1 695 300)) (wt-go '(0 :mouse 1 9000 300)) (wt-go '(0 :mouse 0 9000 300))
(assert-eq "but no bigger than it may be" 5.0 (get :size wt_ruler))

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
;short arcs of a big circle, as a pen draws on its way round: every point of each is on the circle
(defun wt-arc-off (r)
	;eighty short arcs, of a few thousandths of a turn, round a circle about 400 300: the furthest any point is off it
	(defq worst 0.0)
	(each (lambda (i)
		(defq a0 (* (n2f i) 0.0731) a1 (+ a0 (* (n2f (inc (% i 7))) 0.004))
			x0 (+ 400.0 (* r (cos a0))) y0 (+ 300.0 (* r (sin a0))))
		(each (lambda ((x y)) (setq worst (max worst (abs (- (sqrt (+ (* (- x 400.0) (- x 400.0)) (* (- y 300.0) (- y 300.0)))) r)))))
			(partition (path-gen-earc x0 y0 r r 0.0 0.0 1.0 (+ 400.0 (* r (cos a1))) (+ 300.0 (* r (sin a1))) (path x0 y0)) 2)))
		(range 0 80))
	worst)
(assert-true "a short arc of a circle of 180 is on the circle, to a twentieth of a pixel" (< (wt-arc-off 180.0) 0.05))
(assert-true "and of one of 600" (< (wt-arc-off 600.0) 0.1))
(assert-true "and of one of 30" (< (wt-arc-off 30.0) 0.05))

;a pen that has only just set off: an arc of next to no length on a big circle
(assert-list-eq "an arc of a hundredth of a pixel on a circle of 180 ends where it ends, and does not throw" '(179 0)
	(map (const n2i) (slice (path-gen-earc 180.0 0.0 180.0 180.0 0.0 0.0 1.0 179.99999 0.01 (path 180.0 0.0)) -3 -1)))
(assert-true "and so does a shape of one draw" (list? (cwb-flat (cwb-shape "M 580 300 A 180 180 0 0 0 579.99999 299.99"))))
(def wt_pro :mode :line)
(wt-go '(1 :pen 1 586 300)) (wt-go '(1 :pen 1 586 299.99))
(assert-true "a pen held to the round side that has moved a hair has its arc drawn over the board"
	(Board? (. wt_board :draw_overlay (Canvas 64 64 1))))
(wt-go '(1 :pen 0 586 299.99))
;one pointer on it: its middle moves it, the band half way in turns it, the
;outer scale sizes it, and a pen close on the round side draws, as above
(def wt_pro :origin (list 400.0 300.0) :angle 0.0 :size 1.0)
(defun wt-part (x y) (first (. wt_pro :part_at x y)))
(assert-list-eq "the parts of a protractor from its middle out, straight up: move, turn, size" '(:move :turn :size)
	(list (wt-part 0 -75) (wt-part 0 -97) (wt-part 0 -140)))
(wt-go '(0 :mouse 4 400 160)) (wt-go '(0 :mouse 4 400 90)) (wt-go '(0 :mouse 0 400 90))
(assert-true "dragged out by its outer scale, half as far again from its middle, it is half as big again"
	(and (wt-near? (get :size wt_pro) 1.5 0.002) (wt-near? (get :origin wt_pro) '(400 300) 0.01)))
(def wt_pro :size 1.5)
(assert-true "a pen run round it then draws an arc of the radius it is seen at"
	(progn (wt-go '(1 :pen 1 675 300)) (wt-go '(1 :pen 1 400 25)) (wt-go '(1 :pen 0 400 25)) (found? (wt-last-d) "A 270 270")))
(def wt_pro :size 1.0)
(wt-go '(0 :mouse 4 400 225)) (wt-go '(0 :mouse 4 420 205)) (wt-go '(0 :mouse 0 420 205))
(assert-true "dragged by its middle it is moved, no bigger"
	(and (wt-near? (get :origin wt_pro) '(420 280) 0.01) (wt-near? (get :size wt_pro) 1.0 0.001)))
(def wt_pro :origin (list 400.0 300.0))
;its straight side is an edge too
(wt-go '(1 :pen 1 300 306)) (wt-go '(1 :pen 1 500 309)) (wt-go '(1 :pen 0 500 309))
(assert-true "a pen run along its straight side, the bottom of the strip below its middle, draws a line along it"
	(wt-near? (wt-numbers (wt-last-d)) '(300 318 500 318)))
(assert-eq "the strip is its too, a point in it is on it" :t (. wt_pro :hit 300 310))
;the marks of its straight side are out from its middle
(defq wt_found (list))
(each (lambda ((x y x1 y1)) (if (find (n2i x) '(0 50 -50)) (push wt_found (list (n2i x) (n2i (abs (- y y1)))))))
	(partition (map (# (n2f (str-to-num %0))) (filter (# (not (find %0 '("M" "L"))))
		(split (cwb-get (elem-get (get :marks wt_pro) -3) :d) " "))) 4))
(assert-list-eq "the marks of its straight side have a long one at its middle and at each centimetre from it"
	'((-50 8) (0 8) (0 8) (50 8)) (sort wt_found (# (- (first %0) (first %1)))))
;the angle it is at is written on it
(def wt_pro :angle 0.0)
(assert-eq "level, a protractor says 0" "0" (. wt_pro :degrees))
(def wt_pro :angle (/ +fp_pi -6.0))
(assert-eq "turned up a twelfth of a turn, the way its numbers go, 30" "30" (. wt_pro :degrees))
(def wt_pro :angle 0.3)
(assert-eq "turned the other way, to a tenth of a degree, 342.8" "342.8" (. wt_pro :degrees))
(assert-eq "it is written once on a protractor" 1 (length (. wt_pro :readout)))
(def wt_pro :angle 0.0)
(def wt_ruler :origin (list 400.0 300.0) :angle (/ +fp_pi -6.0) :size 1.0)
(assert-list-eq "and twice on a ruler, for who reads its top side and for who reads its bottom, half a turn on" '("30" "210")
	(map (# (. wt_ruler :degrees (if (> (length %0) 2) (third %0)))) (get :readouts wt_ruler)))
(assert-eq "as two things to draw" 2 (length (. wt_ruler :readout)))
(. wt_stage :sub wt_ruler)


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
