(report-header "Math: the Space demo's field of rocks, how they meet, the way through them, and the buoys")

(import "apps/demos/space/field.inc")

;a rock (at drift bulk kind spin patch) of numbers given, kind 0, its
;reach 0.9 of its size, as field.inc has it before its mesh comes
(defun sf-rock (x y z vx vy vz bulk)
	(list (doubles (n2d x) (n2d y) (n2d z)) (doubles (n2d vx) (n2d vy) (n2d vz)) (n2d bulk) 0
		(doubles +double_0 +double_0 +double_0 +double_0) (doubles +double_0 +double_0 +double_0)))

;the field moved on so many frames of a 60th, met a part a frame
(defun sf-run (frames)
	(defq part 0 grid :nil dt (const (n2d 0.0166)))
	(times frames
		(rocks-move dt)
		(if (= part 0) (setq grid (rocks-grid)))
		(rocks-meet (* dt (n2d +meet_parts)) grid part)
		(setq part (% (inc part) +meet_parts))))

(defun sf-x (rock) (elem-get (first rock) 0))
(defun sf-vx (rock) (elem-get (second rock) 0))

;the box goes on every way, the shortest way across it
(assert-true "a way across the box, the shortest" (= (near-way (n2d 900)) (n2d -100)))
(assert-true "and the other way" (= (near-way (n2d -700)) (n2d 300)))
(assert-true "a short way as it is" (= (near-way (n2d 120)) (n2d 120)))

;made: as many as it says, in clusters, each its big rock first
(setq rocks (make-rocks))
(assert-eq "as many rocks as it says" +rock_count (length rocks))
(assert-eq "as many clusters" +clusters (length clusters))
(assert-true "a cluster's big rock first, the rest smaller"
	(every (lambda (members)
			(defq big (third (elem-get rocks (first members))))
			(some! (# (< (third (elem-get rocks %0)) big)) (list members) :t 1 -1))
		clusters))

;a head on meeting: they crunch, little rebound, the small one thrown back
;the more, and what moves of them is kept
(setq rocks (list (sf-rock 400 500 500 5 0 0 10) (sf-rock 460 500 500 -5 0 0 20)))
(defq sf_before (+ (* (n2d 1000) (sf-vx (first rocks))) (* (n2d 8000) (sf-vx (second rocks)))))
(sf-run 400)
(defq sf_after (+ (* (n2d 1000) (sf-vx (first rocks))) (* (n2d 8000) (sf-vx (second rocks)))))
(assert-true "they meet and part, not through each other" (< (sf-x (first rocks)) (sf-x (second rocks))))
(assert-true "the small one thrown back" (< (sf-vx (first rocks)) +double_0))
(assert-true "little rebound, a crunch: they part slower than they met"
	(< (- (sf-vx (second rocks)) (sf-vx (first rocks))) (n2d 3)))
(assert-true "what moves of them kept, to a part in a hundred"
	(< (abs (- sf_after sf_before)) (* (abs sf_before) (n2d 0.01))))
(assert-true "a hit heard" (> (length hits) 0))

;the way through: of headings round the way wanted, a safe one
(defun sf-way (thrill)
	(defq t (thread-start (doubles +double_0 +double_0 +double_0) (const (n2d 24)) (doubles +double_0 +double_0 +double_-1)
			(doubles +double_1 +double_0 +double_0) (doubles +double_0 +double_1 +double_0) thrill))
	(while (not (nums? t)) (setq t (thread-step t 7)))
	t)
(defun sf-straight? (way) (> (third (map (const neg) way)) (const (n2d 0.999))))
(setq rocks (list))
(assert-true "nothing in the way, straight on" (sf-straight? (sf-way :t)))
(setq rocks (list (sf-rock 0 0 -120 0 0 0 15)))
(assert-true "a rock dead ahead, turned from" (not (sf-straight? (sf-way :nil))))
(setq rocks (list (sf-rock -24 0 -120 0 0 0 8) (sf-rock 24 0 -120 0 0 0 8)))
(assert-true "a gap ahead, through it" (sf-straight? (sf-way :t)))
(setq rocks (list (sf-rock 35 0 -150 0 0 0 8) (sf-rock 65 0 -150 0 0 0 8)))
(assert-true "open ahead and a gap to the right, the tour guide goes for the gap" (> (first (sf-way :t)) (n2d 0.2)))
(assert-true "and the mouse's way, the safest near the pointer, stays straight" (sf-straight? (sf-way :nil)))

;a buoy: red with a rock close by, green with none
(setq rocks (list (sf-rock 100 100 100 0 0 0 10)))
(setq buoys (list (list (doubles (n2d 100) (n2d 100) (n2d 108)) (doubles +double_0 +double_0 +double_0)
		+double_0 (cat +buoy_green) :nil)
	(list (doubles (n2d 600) (n2d 600) (n2d 600)) (doubles +double_0 +double_0 +double_0)
		+double_0 (cat +buoy_green) :nil)))
(buoy-look 0)
(buoy-look 1)
(assert-true "a buoy by a rock reads danger" (> (third (first buoys)) (n2d 0.8)))
(assert-true "and is moved out of it" (> (third (first (first buoys))) (n2d 112)))
(assert-true "one in open space reads none" (= (third (second buoys)) +double_0))
