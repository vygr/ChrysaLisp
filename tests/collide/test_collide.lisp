(report-header "Collide")

(import "lib/collide/collide.inc")

;places, near, and bounds
(defq +c_eps (/ +double_1 (n2d 1000000))
	c_at (lambda (x y z) (doubles (n2d x) (n2d y) (n2d z)))
	c_d (lambda (a b) (/ (n2d a) (n2d b)))
	c_near (lambda (a b) (< (abs (- a b)) +c_eps))
	c_ball (lambda (r) (list :sphere (n2d r)))
	c_box (lambda (x y z) (list :box (n2d x) (n2d y) (n2d z)))
	c_ids (lambda (found) (sort (map (const first) found) (const -)))
	c_meet (lambda (a b d) (collide-meet a :nil b :nil d))
	c_ray (lambda (b c from way far r) (collide-ray b :nil c from way far r))
	c_eighth (quat-axis-angle (doubles +double_0 +double_1 +double_0) (/ +double_pi +double_4)))

;;;;;;;;;;;;;;;;;;;;;;;
; bounds, one with one
;;;;;;;;;;;;;;;;;;;;;;;

(assert-true "balls apart" (not (c_meet (c_ball 1) (c_ball 1) (c_at 3 0 0))))
(assert-true "balls touching, how deep and which way"
	(progn (bind '(depth way) (c_meet (c_ball 2) (c_ball 1) (c_at 2 0 0)))
		(and (c_near depth +double_1) (c_near (first way) +double_1))))
(assert-true "a ball by a box, out through its side"
	(progn (bind '(depth way) (c_meet (c_ball 1) (c_box 2 2 2) (doubles +double_0 +double_0 (neg (c_d 5 2)))))
		(and (c_near depth (c_d 1 2)) (c_near (third way) +double_-1))))
(assert-true "a box by a ball, the way from the box"
	(progn (bind '(depth way) (c_meet (c_box 2 2 2) (c_ball 1) (doubles +double_0 (neg (c_d 5 2)) +double_0)))
		(and (c_near depth (c_d 1 2)) (c_near (second way) +double_-1))))
(assert-true "a ball by a box's corner, round it" (not (c_meet (c_ball 1) (c_box 1 1 1) (c_at 2 2 0))))
(assert-true "boxes, by their least overlap"
	(progn (bind '(depth way) (c_meet (c_box 1 1 1) (c_box 1 1 1) (doubles (c_d 3 2) (c_d 1 2) +double_0)))
		(and (c_near depth (c_d 1 2)) (c_near (first way) +double_1))))
(assert-true "boxes apart" (not (c_meet (c_box 1 1 1) (c_box 1 1 1) (c_at 0 3 0))))
(assert-true "a ray to a ball" (c_near (c_ray (c_ball 1) (c_at 10 0 0) (c_at 0 0 0) (c_at 1 0 0) (n2d 100) +double_0) (n2d 9)))
(assert-true "a ball swept to a ball" (c_near (c_ray (c_ball 1) (c_at 10 0 0) (c_at 0 0 0) (c_at 1 0 0) (n2d 100) +double_1) (n2d 8)))
(assert-true "a ray past a ball" (not (c_ray (c_ball 1) (c_at 10 3 0) (c_at 0 0 0) (c_at 1 0 0) (n2d 100) +double_0)))
(assert-true "a ray short of a ball" (not (c_ray (c_ball 1) (c_at 10 0 0) (c_at 0 0 0) (c_at 1 0 0) (n2d 5) +double_0)))
(assert-true "a ray to a box" (c_near (c_ray (c_box 1 1 1) (c_at 0 0 -10) (c_at 0 0 0) (c_at 0 0 -1) (n2d 100) +double_0) (n2d 9)))
(assert-true "a ray past a box" (not (c_ray (c_box 1 1 1) (c_at 3 0 -10) (c_at 0 0 0) (c_at 0 0 -1) (n2d 100) +double_0)))
(assert-true "from in a box" (c_near (c_ray (c_box 1 1 1) (c_at 0 0 0) (c_at 0 0 0) (c_at 0 0 -1) (n2d 100) +double_0) +double_0))

;;;;;;;;;
; a world
;;;;;;;;;

(defq c_w (Collide (n2d 4)) c_a (. c_w :add (c_at 0 0 0) (c_ball 1)) c_b (. c_w :add (c_at 3 0 0) (c_ball 1))
	c_c (. c_w :add (c_at 30 0 0) (c_box 1 1 1) 2))
(assert-list-eq "near a place" '(0 1) (c_ids (. c_w :near (doubles (c_d 3 2) +double_0 +double_0) (n2d 1))))
(assert-list-eq "near, of a mask" '(2) (c_ids (. c_w :near (c_at 30 0 0) (n2d 1) 2)))
(assert-list-eq "near, none of that mask" '() (c_ids (. c_w :near (c_at 30 0 0) (n2d 1) 1)))
(assert-eq "none touch yet" 0 (length (. c_w :pairs)))
(. c_w :move c_b (doubles (c_d 3 2) +double_0 +double_0))
(assert-eq "moved, they touch" "((0 1))" (str (map (# (slice %0 0 2)) (. c_w :pairs))))
(assert-list-eq "a ray meets the first" (list c_a) (slice (. c_w :cast (c_at -10 0 0) (c_at 1 0 0) (n2d 100)) 0 1))
(assert-true "how far along" (c_near (second (. c_w :cast (c_at -10 0 0) (c_at 1 0 0) (n2d 100))) (n2d 9)))
(assert-list-eq "a ray of a mask, the box" (list c_c) (slice (. c_w :cast (c_at -10 0 0) (c_at 1 0 0) (n2d 100) +double_0 2) 0 1))
(assert-eq "a ray that meets nothing" :nil (. c_w :cast (c_at -10 5 0) (c_at 1 0 0) (n2d 100)))

;a big one, met from cells away
(defq c_big (. c_w :add (c_at 10 0 0) (c_ball 9) 3))
(assert-eq "a big one meets those far in it" (str (list (list c_b c_big)))
	(str (map (# (slice %0 0 2)) (filter (# (= (second %0) c_big)) (. c_w :pairs)))))
(assert-list-eq "and near finds it" (list c_big) (c_ids (. c_w :near (c_at 18 0 0) +double_1 1)))
(. c_w :remove c_big)
(assert-list-eq "gone" '() (c_ids (. c_w :near (c_at 18 0 0) +double_1 1)))
(assert-eq "its id had again" c_big (. c_w :add (c_at 50 50 50) (c_ball 1)))

;touches begun, held and ended
(defq c_w (Collide (n2d 4)) c_a (. c_w :add (c_at 0 0 0) (c_ball 1)) c_b (. c_w :add (c_at 5 0 0) (c_ball 1)))
(assert-list-eq "nothing begun" '(0 0 0) (map (const length) (. c_w :touches)))
(. c_w :move c_b (c_at 1 0 0))
(assert-list-eq "begun" '(1 0 0) (map (const length) (. c_w :touches)))
(assert-list-eq "held" '(0 1 0) (map (const length) (. c_w :touches)))
(. c_w :move c_b (c_at 5 0 0))
(assert-eq "ended" (str (list (list c_a c_b))) (str (third (. c_w :touches))))
(assert-list-eq "and no more" '(0 0 0) (map (const length) (. c_w :touches)))

;made exact by the app: these two only meet when much nearer
(. c_w :move c_b (doubles (c_d 3 2) +double_0 +double_0))
(assert-eq "an exact shape says not yet" 0
	(length (. c_w :pairs (lambda (w a b depth way) (if (> depth +double_1) (list depth way))))))
(. c_w :move c_b (doubles (c_d 1 2) +double_0 +double_0))
(assert-eq "and now" 1 (length (. c_w :pairs (lambda (w a b depth way) (if (> depth +double_1) (list depth way))))))

;a world that wraps: across its edge is near
(defq c_w (Collide (n2d 4) (doubles (n2d 100) +double_0 (n2d 100)))
	c_a (. c_w :add (c_at 1 0 50) (c_ball 2)) c_b (. c_w :add (c_at 99 0 50) (c_ball 2)))
(assert-eq "they touch across the edge" 1 (length (. c_w :pairs)))
(assert-true "the way across it" (c_near (first (. c_w :apart (c_at 1 0 0) (c_at 99 0 0))) +double_-2))
(assert-list-eq "near across the edge" '(0 1) (c_ids (. c_w :near (c_at 0 0 50) +double_1)))
(assert-list-eq "a ray across the edge, the one before it" (list c_b) (slice (. c_w :cast (c_at 90 0 50) (c_at 1 0 0) (n2d 20)) 0 1))
(assert-eq "not round the way it does not wrap" 0
	(length (. (progn (defq c_v (Collide (n2d 4) (doubles (n2d 100) +double_0 (n2d 100))))
		(. c_v :add (c_at 0 1 0) (c_ball 1)) (. c_v :add (c_at 0 99 0) (c_ball 1)) c_v) :pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; owners, sensors, and what is in a region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a buoy: a tight body to hit, and a wide sensor for what is near it
(defq c_w (Collide (n2d 4))
	c_hull (. c_w :attach :buoy (c_at 0 0 0) (c_at 0 0 0) (c_ball 1))
	c_zone (. c_w :attach :buoy (c_at 0 0 0) (c_at 0 0 0) (c_ball 10) 1 :t)
	c_rock (. c_w :add (c_at 20 0 0) (c_ball 1)))
(assert-list-eq "an owner's bodies" (list c_hull c_zone) (. c_w :bodies :buoy))
(assert-eq "never meeting each other" 0 (length (. c_w :pairs)))
(assert-eq "its sensor" :t (. c_w :sensor? c_zone))
(. c_w :touches)
(. c_w :move c_rock (c_at 8 0 0))
(assert-eq "a rock come into the zone" (str (list (list c_zone c_rock)))
	(str (map (# (slice %0 0 2)) (first (. c_w :touches)))))
(. c_w :move c_rock (c_at (/ 3 2) 0 0))
(assert-eq "into the hull too, the zone still held" "(1 1 0)" (str (map (const length) (. c_w :touches))))
(. c_w :move_owner :buoy (c_at 0 0 50))
(assert-true "the owner moved, its bodies with it"
	(and (c_near (third (. c_w :centre c_hull)) (n2d 50)) (c_near (third (. c_w :centre c_zone)) (n2d 50))))
(assert-eq "and the rock left behind, both ended" 2 (length (third (. c_w :touches))))
(defq c_other (. c_w :add (c_at 0 15 50) (c_ball 10) 1 :t))
(assert-eq "two sensors never meet" 0 (length (. c_w :pairs)))
(assert-eq "a ray goes through a sensor" :nil (. c_w :cast (c_at 0 5 45) (c_at 0 0 1) (n2d 20)))
(assert-list-eq "a ray meets the hull" (list c_hull) (slice (. c_w :cast (c_at 0 0 30) (c_at 0 0 1) (n2d 30)) 0 1))

;what is in a region, all of it or some
(defq c_w (Collide (n2d 4)) c_in (. c_w :add (c_at 2 0 0) (c_ball 1)) c_edge (. c_w :add (c_at 5 0 0) (c_ball 1))
	c_out (. c_w :add (c_at 9 0 0) (c_ball 1)) c_s (. c_w :add (c_at 1 0 0) (c_ball 1) 1 :t))
(defq c_found (. c_w :within (c_ball 5) (c_at 0 0 0)))
(assert-list-eq "a ball's, sensors not" (list c_in c_edge) (c_ids c_found))
(assert-list-eq "which are all in it" (list c_in) (c_ids (filter (# (elem-get %0 3)) c_found)))
(assert-list-eq "with sensors" (list c_in c_edge c_s) (c_ids (. c_w :within (c_ball 5) (c_at 0 0 0) :nil :t)))
(assert-list-eq "a box's" (list c_edge c_out) (c_ids (. c_w :within (c_box 3 1 1) (c_at 7 0 0))))
(assert-list-eq "all in a box" (list c_out) (c_ids (filter (# (elem-get %0 3)) (. c_w :within (c_box 3 2 2) (c_at 8 0 0)))))
(. c_w :remove c_s)
(assert-list-eq "a sensor gone" (list c_in c_edge) (c_ids (. c_w :within (c_ball 5) (c_at 0 0 0) :nil :t)))
(. c_w :detach :none)
(assert-eq "detaching nothing does nothing" 3 (length (. c_w :within (c_ball 20) (c_at 0 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; turned boxes, and boxes square to the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a box an eighth of a turn about up reaches further across, by its corner
(assert-true "its box square to the world"
	(progn (bind '(ex ey ez) (collide-extent (c_box 1 1 1) c_eighth))
		(and (c_near ex (sqrt +double_2)) (c_near ey +double_1) (c_near ez (sqrt +double_2)))))
(assert-true "a ball by its corner, turned" (collide-meet (c_ball 1) :nil (c_box 1 1 1) c_eighth (doubles (c_d -23 10) +double_0 +double_0)))
(assert-true "not, square" (not (collide-meet (c_ball 1) :nil (c_box 1 1 1) :nil (doubles (c_d -23 10) +double_0 +double_0))))
(assert-true "boxes by a corner, turned"
	(progn (bind '(depth way) (collide-meet (c_box 1 1 1) :nil (c_box 1 1 1) c_eighth (doubles (c_d 22 10) +double_0 +double_0)))
		(and (c_near depth (- (+ +double_1 (sqrt +double_2)) (c_d 22 10))) (c_near (first way) +double_1))))
(assert-true "boxes apart, square" (not (collide-meet (c_box 1 1 1) :nil (c_box 1 1 1) :nil (doubles (c_d 22 10) +double_0 +double_0))))
(assert-true "turned both, still apart" (not (collide-meet (c_box 1 1 1) c_eighth (c_box 1 1 1) c_eighth (doubles (c_d 30 10) +double_0 +double_0))))
(assert-true "a ray to a turned box, at its edge"
	(c_near (collide-ray (c_box 1 1 1) c_eighth (c_at 0 0 0) (doubles (c_d 12 10) +double_0 +double_10) (c_at 0 0 -1) (n2d 100) +double_0)
		(- +double_10 (- (sqrt +double_2) (c_d 12 10)))))

;a world of turned boxes, asked their boxes square to it
(defq c_w (Collide (n2d 4)) c_t (. c_w :add (c_at 0 0 0) (c_box 1 1 1) 1 :nil c_eighth))
(assert-true "a body's box square to the world"
	(progn (bind '(lo hi) (. c_w :aabb c_t)) (and (c_near (first lo) (neg (sqrt +double_2))) (c_near (second hi) +double_1))))
(defq c_u (. c_w :add (doubles (c_d 22 10) +double_0 +double_0) (c_box 1 1 1)))
(assert-eq "turned, they touch" 1 (length (. c_w :pairs)))
(. c_w :turn c_t :nil)
(assert-eq "square, they do not" 0 (length (. c_w :pairs)))

;a ship of two bodies, turned together
(defq c_w (Collide (n2d 4)) c_hull (. c_w :attach :ship (c_at 0 0 0) (c_at 0 0 0) (c_box 1 1 2))
	c_gun (. c_w :attach :ship (c_at 0 0 0) (c_at 0 0 3) (c_ball 1)))
(. c_w :move_owner :ship (c_at 10 0 0) (quat-axis-angle (doubles +double_0 +double_1 +double_0) (/ +double_pi +double_2)))
(assert-true "its gun turned round with it" (c_near (first (. c_w :centre c_gun)) (n2d 13)))
(assert-true "its hull turned" (progn (bind '(lo hi) (. c_w :aabb c_hull)) (c_near (first hi) (n2d 12))))
(assert-true "the box of all of it"
	(progn (bind '(lo hi) (. c_w :aabb_owner :ship)) (and (c_near (first lo) (n2d 8)) (c_near (first hi) (n2d 14)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; narrow tests, the app's: yes or no
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;two balls whose bounds meet; one's true shape a cube in it, half size by
;its shape, which the app's test reads: as near the cube as a ball reaches
(defq c_w (Collide (n2d 4)) c_p (. c_w :add (c_at 0 0 0) (c_ball 2))
	c_q (. c_w :add (doubles (c_d 25 10) (c_d 25 10) +double_0) (c_ball 2))
	c_cubed (lambda (w a b)
		(defq cube (if (. w :shape a) a b) ball (if (= cube a) b a))
		(collide-meet (list :sphere (second (. w :bound ball))) :nil
			(cat (list :box) (. w :shape cube)) :nil
			(. w :apart (. w :centre ball) (. w :centre cube)))))
(assert-eq "the bounds meet" 1 (length (. c_w :pairs)))
(. c_w :narrow c_p c_cubed (list +double_1 +double_1 +double_1))
(assert-eq "its cube does not" 0 (length (. c_w :pairs)))
(assert-eq "its shape kept" 3 (length (. c_w :shape c_p)))
(. c_w :move c_q (doubles (c_d 25 10) +double_0 +double_0))
(assert-eq "nearer, it does" 1 (length (. c_w :pairs)))
(assert-true "the depth the bounds'" (c_near (elem-get (first (. c_w :pairs)) 2) (c_d 15 10)))
(. c_w :narrow c_q (lambda (w a b) :nil))
(assert-eq "the first's test says" 1 (length (. c_w :pairs)))
(. c_w :narrow c_p :nil)
(assert-eq "else the second's" 0 (length (. c_w :pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; spread over calls: each body every so many, the player every one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;two rocks touching, every 4 calls; the player touching a third, every call
(defq c_w (Collide (n2d 4)))
(. c_w :every_new 4)
(defq c_r1 (. c_w :add (c_at 0 0 0) (c_ball 1)) c_r2 (. c_w :add (c_at 1 0 0) (c_ball 1))
	c_r3 (. c_w :add (c_at 20 0 0) (c_ball 1)) c_me (. c_w :add (c_at 21 0 0) (c_ball 1)))
(. c_w :every c_me 1)
(defq c_calls (map (lambda (_) (. c_w :pairs)) (list 0 1 2 3 4 5 6 7)))
(assert-eq "the rocks, once each 4 calls" 2
	(reduce (lambda (n ps) (+ n (length (filter (# (= (first %0) c_r1)) ps)))) c_calls 0))
(assert-eq "the player, every call" 8
	(reduce (lambda (n ps) (+ n (length (filter (# (= (second %0) c_me)) ps)))) c_calls 0))
(assert-eq "the rocks' test stands for 4 calls" 4
	(elem-get (some (# (some (lambda (p) (if (= (first p) c_r1) p)) %0)) c_calls) 4))
(assert-eq "the player's for 1" 1
	(elem-get (first (filter (# (= (second %0) c_me)) (first c_calls))) 4))

;touches: a rock's touch held over the calls it is not tested, ended only
;when it is tested and does not
(defq c_w (Collide (n2d 4)))
(. c_w :every_new 4)
(defq c_r1 (. c_w :add (c_at 0 0 0) (c_ball 1)) c_r2 (. c_w :add (c_at 1 0 0) (c_ball 1)))
(defq c_seen (map (lambda (_) (map (const length) (. c_w :touches))) (list 0 1 2 3)))
(assert-eq "begun once in a round" 1 (reduce (lambda (n s) (+ n (first s))) c_seen 0))
(assert-eq "never ended while touching" 0 (reduce (lambda (n s) (+ n (third s))) c_seen 0))
(. c_w :move c_r2 (c_at 9 0 0))
(defq c_seen (map (lambda (_) (map (const length) (. c_w :touches))) (list 0 1 2 3)))
(assert-eq "ended once, when tested" 1 (reduce (lambda (n s) (+ n (third s))) c_seen 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; centres held, moved in place, and a sensor's rate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq c_w (Collide (n2d 4)) c_p1 (c_at 0 0 0) c_p2 (c_at 30 0 0)
	c_a (. c_w :add c_p1 (c_ball 1)) c_b (. c_w :add c_p2 (c_ball 1)))
(elem-set c_p2 0 (c_d 3 2))
(assert-eq "moved in place, its pairs at once" 1 (length (. c_w :pairs)))
(. c_w :refile)
(assert-list-eq "refiled, a region finds it" (list c_b) (c_ids (. c_w :near (c_at 2 0 0) (c_d 1 10))))
(assert-true "the centre held, not a copy" (eql (. c_w :centre c_b) c_p2))

;a rock and its pull, a wide sensor of the same owner on its centre, every
;6 calls, the rocks every 2: the sensor's pairs at its own rate
(defq c_w (Collide (n2d 4)) c_at1 (c_at 0 0 0))
(. c_w :every_new 2)
(defq c_rock (. c_w :attach 0 c_at1 :nil (c_ball 1)) c_zone (. c_w :attach 0 c_at1 :nil (c_ball 20) 1 :t)
	c_other (. c_w :add (c_at 10 0 0) (c_ball 1)))
(. c_w :every c_zone 6)
(defq c_calls (map (lambda (_) (. c_w :pairs)) (list 0 1 2 3 4 5)))
(assert-eq "the zone met once in its 6" 1 (reduce (lambda (n ps) (+ n (length ps))) c_calls 0))
(assert-eq "standing for 6" 6 (elem-get (some (# (if (nempty? %0) (first %0))) c_calls) 4))
(assert-true "its centre the rock's" (eql (. c_w :centre c_zone) (. c_w :centre c_rock)))
