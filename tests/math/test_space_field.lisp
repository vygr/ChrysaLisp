(report-header "Math: the Space demo's field of rocks, how they meet, the way through them, and the buoys")

(import "apps/demos/space/field.inc")

;a rock (at drift bulk kind spin patch) of numbers given, kind 0, its
;reach 0.9 of its size, as field.inc has it before its mesh comes
(defun sf-rock (x y z vx vy vz bulk)
	(list (doubles (n2d x) (n2d y) (n2d z)) (doubles (n2d vx) (n2d vy) (n2d vz)) (n2d bulk) 0
		(doubles +double_0 +double_0 +double_0 +double_1 +double_0 +double_0 +double_0)
		(doubles +double_0 +double_0 +double_0)))

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
		+double_0 (cat +buoy_green) :nil 0 0 0)
	(list (doubles (n2d 600) (n2d 600) (n2d 600)) (doubles +double_0 +double_0 +double_0)
		+double_0 (cat +buoy_green) :nil 0 0 0)))
(buoy-look 0)
(buoy-look 1)
(assert-true "a buoy by a rock reads danger" (> (third (first buoys)) (n2d 0.8)))
(assert-true "and is moved out of it" (> (third (first (first buoys))) (n2d 112)))
(assert-true "one in open space reads none" (= (third (second buoys)) +double_0))

;a blast: its front throws the rocks it goes over out from it, a small
;one faster than a big one, and leaves one out of its reach as it was
(setq rocks (list (sf-rock 520 500 500 0 0 0 5) (sf-rock 480 500 500 0 0 0 40) (sf-rock 800 500 500 0 0 0 5)))
(assert-eq "its front not yet at them, none moved" 0 (rocks-blast (n2d 500) (n2d 500) (n2d 500) +double_0 (n2d 10)))
(assert-eq "its front gone over them, those in its reach moved" 2
	(rocks-blast (n2d 500) (n2d 500) (n2d 500) (n2d 10) (n2d 400)))
(assert-true "out from it" (and (> (sf-vx (first rocks)) +double_0) (< (sf-vx (second rocks)) +double_0)))
(assert-true "a small one faster than a big one" (> (sf-vx (first rocks)) (* (n2d 4) (abs (sf-vx (second rocks))))))
(assert-true "one out of its reach as it was" (= (sf-vx (third rocks)) +double_0))
(assert-true "a place in a rock is in it" (eql (rock-at (n2d 522) (n2d 500) (n2d 500)) (first rocks)))
(assert-true "and one between is not" (not (rock-at (n2d 650) (n2d 500) (n2d 500))))

;tumbling: a rock struck off its middle spins about the way across where
;it was struck and the way the strike went, one struck through its middle
;not at all; and it turns, its quaternion staying of length 1
(defun sf-spin (rock) (slice (elem-get rock 4) 0 3))
(setq rocks (list (sf-rock 500 500 500 0 0 0 10) (sf-rock 700 500 500 0 0 0 10)))
(rock-strike (first rocks) (n2d 500) (n2d 504) (n2d 491) (doubles +double_0 +double_0 +double_1))
(bind '(wx wy wz) (sf-spin (first rocks)))
(assert-true "struck above its middle going along z, it spins about x" (and (> wx (n2d 0.1)) (= wy +double_0) (= wz +double_0)))
(rock-strike (second rocks) (n2d 700) (n2d 500) (n2d 491) (doubles +double_0 +double_0 +double_1))
(assert-true "struck through its middle, not at all" (every (# (= %0 +double_0)) (sf-spin (second rocks))))
(times 120 (rocks-move (n2d 0.0166)))
(bind '(& & & qw qx qy qz) (elem-get (first rocks) 4))
(assert-true "it has turned, about x" (and (< qw (n2d 0.99)) (> (abs qx) (n2d 0.1)) (< (abs qy) (n2d 0.001))))
(assert-true "its quaternion of length 1" (< (abs (- (+ (* qw qw) (* qx qx) (* qy qy) (* qz qz)) +double_1)) (const (/ +double_1 (n2d 1000000)))))
(assert-true "the matrix of it turns up about x" (< (abs (elem-get (rock-turned (elem-get (first rocks) 4)) 4)) (n2d 0.001)))

;a buoy at a swarm calm long enough goes to a cluster with none, and one
;in open space stays, however calm
(setq rocks (make-rocks) buoys (make-buoys))
(defq sf_taken (map (# (elem-get %0 4)) buoys) sf_open (last buoys) sf_open_at (cat (first sf_open)))
(elem-set (first buoys) 5 +calm_looks)
(elem-set sf_open 5 (* 2 +calm_looks))
(buoys-restation (doubles +double_0 +double_0 +double_0) +double_0)
(assert-true "the calm one at a cluster with none" (not (find (elem-get (first buoys) 4) sf_taken)))
(assert-eq "its count of calm begun again" 0 (elem-get (first buoys) 5))
(assert-true "the one in open space where it was" (eql (first sf_open) sf_open_at))

;a cluster put in orbit again moves only its rocks out of sight: one by the
;eye stays where it is, the others go, so none is seen to go or come
(setq rocks (make-rocks) buoys (list))
(defq sf_members (first clusters) sf_near (elem-get rocks (second sf_members))
	sf_near_at (cat (first sf_near)) sf_eye (nums-add (first sf_near) (doubles (n2d 3) +double_0 +double_0))
	sf_others (map! (# (cat (first (elem-get rocks %0)))) (list sf_members) 2 -1))
(cluster-again sf_members sf_eye (n2d 60))
(assert-true "the one by the eye where it was" (eql (first sf_near) sf_near_at))
(assert-true "some of the others put in orbit again"
	(some (lambda (was) (not (eql (first (elem-get rocks (elem-get sf_members (+ (!) 2)))) was))) sf_others))

;a buoy's colour eased to its hazard's: calm at a swarm green, calm in open
;space ice blue
(setq buoys (list (list (doubles (n2d 100) (n2d 100) (n2d 100)) (doubles +double_0 +double_0 +double_0)
		+double_0 (cat +buoy_red) 0 0 0 0)
	(list (doubles (n2d 600) (n2d 600) (n2d 600)) (doubles +double_0 +double_0 +double_0)
		+double_0 (cat +buoy_red) :nil 0 0 0)))
(times 300 (buoys-move (n2d 0.0166)))
(defun sf-near (a b) (every (# (< (abs (- %0 %1)) (const (n2d 0.01)))) a b))
(assert-true "calm at a swarm, green" (sf-near (elem-get (first buoys) 3) +buoy_green))
(assert-true "calm in open space, ice blue" (sf-near (elem-get (second buoys) 3) +buoy_blue))

;a buoy gone nuclear: the small rocks in its core shattered and sent out of
;sight, no more of their cluster; a big one left; one outside as it was
(setq rocks (list (sf-rock 500 500 500 0 0 0 40) (sf-rock 540 500 500 0 0 0 6) (sf-rock 500 560 500 0 0 0 8)
		(sf-rock 800 500 500 0 0 0 6))
	clusters (list (list 0 1 2)))
(defq sf_eye (doubles (n2d 500) (n2d 500) (n2d 400)) sf_shattered (rocks-nuke (n2d 500) (n2d 500) (n2d 500) sf_eye (n2d 300)))
(assert-eq "the two small ones in its core shattered" 2 (length sf_shattered))
(assert-true "and gone out of sight" (every (# (out-of-sight? (first (elem-get rocks %0)) sf_eye (const (n2d 300)))) '(1 2)))
(assert-true "no more of their cluster" (eql (first clusters) '(0)))
(assert-true "the big one where it was" (= (sf-x (first rocks)) (n2d 500)))
(assert-true "the one outside its core where it was" (= (sf-x (elem-get rocks 3)) (n2d 800)))
