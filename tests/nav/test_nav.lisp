(report-header "Nav")

(import "lib/nav/nav.inc")

;places, near, a straight way and a frame step
(defq +n_eps (/ +double_1 (n2d 1000000)) +n_dt (/ +double_1 (n2d 30))
	n_near (lambda (a b e) (< (reduce max (map (lambda (x y) (abs (- x y))) a b) +double_0) e))
	n_at (lambda (x y z) (doubles (n2d x) (n2d y) (n2d z)))
	n_d (lambda (a b) (/ (n2d a) (n2d b))))

;;;;;;;
; graph
;;;;;;;

;a grid of 3 by 3 a unit apart, each linked to those across and along
(defq n_g (graph))
(each! (lambda () (graph-node n_g (n_at (% (!) 3) 0 (/ (!) 3)))) '() 0 9)
(each! (lambda ()
		(defq i (!))
		(if (< (% i 3) 2) (graph-link n_g i (inc i)))
		(if (< i 6) (graph-link n_g i (+ i 3))))
	'() 0 9)

(assert-eq "a node each place" 9 (graph-size n_g))
(assert-eq "linked both ways" 2 (length (graph-links n_g 0)))
(assert-eq "the middle has four" 4 (length (graph-links n_g 4)))
(assert-eq "nearest" 5 (graph-nearest n_g (doubles (n2d 2) +double_0 (n_d 13 10))))
(assert-eq "nearest of those that will do" 2
	(graph-nearest n_g (doubles (n2d 2) +double_0 (n_d 13 10)) (lambda (g n) (< n 3))))
(assert-eq "corner to corner, five nodes" 5 (length (graph-route n_g 0 8)))
(assert-list-eq "it starts and ends where asked" '(0 8)
	(progn (defq n_r (graph-route n_g 0 8)) (list (first n_r) (last n_r))))
(assert-list-eq "to itself, itself" '(4) (graph-route n_g 4 4))
(assert-true "each step a link"
	(every (lambda (a b) (find b (graph-links n_g a)))
		(slice (defq n_r (graph-route n_g 0 8)) 0 -2) (slice n_r 1 -1)))

;a wall: the links of the middle row shut, by the cost now, or gone
(defq n_shut (lambda (g a b) (if (and (/= (/ a 3) (/ b 3)) (or (= (/ a 3) 1) (= (/ b 3) 1)) (/= (% a 3) 2)) :nil
		(graph-distance (graph-place g a) (graph-place g b)))))
(assert-true "round the wall, by the gap" (find 5 (graph-route n_g 0 6 n_shut)))
(assert-eq "round the wall, the least" 7 (length (graph-route n_g 0 6 n_shut)))
(assert-eq "no way, :nil" :nil (graph-route n_g 0 6 (lambda (g a b) (if (/= (/ a 3) (/ b 3)) :nil +double_1))))
(graph-unlink n_g 0 1)
(assert-list-eq "unlinked both ways" '(:nil :nil) (list (find 1 (graph-links n_g 0)) (find 0 (graph-links n_g 1))))
(assert-list-eq "round the unlinked" '(0 3 4 1) (graph-route n_g 0 1))
(graph-link n_g 0 1)

;;;;;;
; path
;;;;;;

(defq n_line (nav-path-resample (list (n_at 0 0 0) (n_at 10 0 0)) +double_1))
(assert-eq "resampled a unit apart" 11 (length n_line))
(assert-true "its ends kept" (and (n_near (first n_line) (n_at 0 0 0) +n_eps) (n_near (last n_line) (n_at 10 0 0) +n_eps)))
(assert-true "its length" (< (abs (- (nav-path-length n_line) (n2d 10))) +n_eps))
(assert-true "a straight line smoothed is as it was"
	(every (lambda (a b) (n_near a b +n_eps)) n_line (nav-path-smooth n_line 3)))
(assert-true "a corner smoothed is rounded, its ends kept"
	(progn (bind '(a b c) (nav-path-smooth (list (n_at 0 0 0) (n_at 4 0 0) (n_at 4 0 4))))
	(and (n_near a (n_at 0 0 0) +n_eps) (n_near c (n_at 4 0 4) +n_eps) (n_near b (n_at 3 0 1) +n_eps))))
(assert-true "the way along it" (n_near (nav-path-way-at n_line 4) (n_at 1 0 0) +n_eps))
(assert-eq "on it, the place passed" 3 (nav-path-on n_line (doubles (n_d 34 10) +double_1 +double_0) 0 8))
(assert-eq "nearer the next, still the place passed" 3 (nav-path-on n_line (doubles (n_d 36 10) +double_0 +double_0) 0 8))
(assert-eq "never back" 5 (nav-path-on n_line (n_at 1 0 0) 5 8))
(assert-eq "no further on than it looks" 2 (nav-path-on n_line (n_at 9 0 0) 0 2))
(assert-true "how far past" (< (abs (- (nav-path-past n_line 3 (doubles (n_d 34 10) +double_1 +double_0)) (n_d 4 10))) +n_eps))
(assert-true "so far ahead" (n_near (nav-path-ahead n_line 3 (n_d 24 10)) (doubles (n_d 54 10) +double_0 +double_0) +n_eps))
(assert-true "ahead past the end, the end" (n_near (nav-path-ahead n_line 3 (n2d 50)) (n_at 10 0 0) +n_eps))

;;;;;;;;
; spring
;;;;;;;;

(defq n_spring (lambda (zeta)
	(defq x +double_0 v +double_0 top +double_0)
	(times 300
		(defq r (nav-spring x v +double_1 +n_dt +double_2 zeta))
		(setq x (first r) v (second r) top (max top x)))
	(list x top)))
(assert-true "damped just enough, there and never past"
	(progn (bind '(x top) (n_spring +double_1)) (and (< (abs (- x +double_1)) (n_d 1 100)) (<= top +double_1))))
(assert-true "damped less, it swings past" (> (second (n_spring (n_d 3 10))) (n_d 11 10)))

;;;;;
; Nav
;;;;;

;along a path from off it: on it and level, its path on along -z
(defq n_nav (Nav (n_at 3 0 0) (n_at 0 0 -1)) n_least +double_0)
(def n_nav :cruise (n2d 4))
(. n_nav :routed (nav-path-resample (list (n_at 0 0 0) (n_at 0 0 -200)) +double_1))
(times 450 (. n_nav :step +n_dt) (setq n_least (min n_least (first (get :at n_nav)))))
(assert-true "follows onto its path" (< (abs (first (get :at n_nav))) (n_d 5 100)))
(assert-true "and never far past it" (> n_least (neg (n_d 15 100))))
(assert-true "up at its speed" (< (abs (- (get :speed n_nav) (n2d 4))) (n_d 1 100)))
(assert-true "facing along it" (n_near (get :way n_nav) (n_at 0 0 -1) (n_d 1 100)))
(assert-true "upright" (> (second (get :up n_nav)) (n_d 999 1000)))
(assert-true "its matrix" (= (length (. n_nav :matrix)) 16))

;a turn eased, :lerp, the same
(defq n_nav (Nav (n_at 3 0 0) (n_at 1 0 0)))
(def n_nav :steer :lerp :rate (n2d 3) :cruise (n2d 4))
(. n_nav :routed (nav-path-resample (list (n_at 0 0 0) (n_at 0 0 -200)) +double_1))
(times 450 (. n_nav :step +n_dt))
(assert-true "eased onto its path from across it" (< (abs (first (get :at n_nav))) (n_d 1 10)))

;to a goal and stopping there
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :stop :t :cruise (n2d 3))
(. n_nav :go (n_at 0 0 -20))
(assert-eq "its route straight" 21 (length (get :path n_nav)))
(times 600 (. n_nav :step +n_dt))
(assert-eq "got there" :t (get :arrived n_nav))
(assert-true "and stopped" (< (abs (get :speed n_nav)) (n_d 1 10)))
(assert-true "there" (< (nav-far (get :at n_nav) (n_at 0 0 -20)) (n_d 15 10)))

;a tour, round and round, each stop told
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :cruise (n2d 4) :near (n2d 2) :next_fn nav-next-tour :count 0
	:tour (list (n_at 0 0 -12) (n_at 12 0 -12) (n_at 12 0 0) (n_at 0 0 0))
	:arrive_fn (lambda (nav) (def nav :count (inc (get :count nav)))))
(. n_nav :go (nav-next-tour n_nav))
(times 900 (. n_nav :step +n_dt))
(assert-true "round its tour" (>= (get :count n_nav) 4))

;by a graph, round the wall
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 1)))
(def n_nav :route_fn nav-route-graph :graph n_g :cost_fn n_shut :smooth 0)
(. n_nav :go (n_at 0 0 2))
(assert-true "a route by the gap" (some (lambda (p) (n_near p (n_at 2 0 1) +n_eps)) (get :path n_nav)))

;a route that comes later: it heads for its goal meanwhile
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :route_fn (lambda (nav from to via) :nil))
(. n_nav :go (n_at 10 0 0))
(assert-eq "waiting on its route" :t (get :pending n_nav))
(times 30 (. n_nav :step +n_dt))
(assert-true "turned for its goal meanwhile" (> (first (get :way n_nav)) +double_0))
(. n_nav :routed (list (get :at n_nav) (n_at 10 0 0)))
(assert-eq "its route come" :nil (get :pending n_nav))
(. n_nav :routed (list (n_at 10 0 -10)) :t)
(assert-eq "more of it on the end" 3 (length (get :path n_nav)))

;a rock on its way, a ball: round it, never into it
(defq n_rock (n_at 0 0 -15) n_ball (lambda (p) (- (nav-far p n_rock) +double_3)) n_close (n2d 100))
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :cruise (n2d 4) :field_fn n_ball :clear +double_2 :body +double_1
	:avoid_fn nav-avoid-field :move_fn nav-move-field)
(. n_nav :go (n_at 0 0 -30))
(times 450 (. n_nav :step +n_dt) (setq n_close (min n_close (n_ball (get :at n_nav)))))
(assert-true "never into the rock" (>= n_close (n_d 99 100)))
(assert-true "and past it" (< (third (get :at n_nav)) (n2d -20)))

;the same by a cone of ways, each asked how near the rock it goes, a
;nimbler turn, as a thing near what is in its way has
(defq n_close (n2d 100) n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :cruise (n2d 4) :k +double_3 :clear +double_2 :avoid_fn nav-avoid-cone
	:clear_fn (lambda (nav way)
		(defq at (get :at nav) to (nums-sub n_rock at) t (max +double_0 (min +double_10 (nums-dot to way))))
		(n_ball (nums-add at (nums-scale way t)))))
(. n_nav :go (n_at 0 0 -30))
(times 450 (. n_nav :step +n_dt) (setq n_close (min n_close (n_ball (get :at n_nav)))))
(assert-true "round the rock by the cone" (> n_close +double_1))
(assert-true "and past it by the cone" (< (third (get :at n_nav)) (n2d -20)))

;stuck: against a wall it can not go through, told so
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :stucks 0 :stuck_fn (lambda (nav) (def nav :stucks (inc (get :stucks nav))))
	:move_fn (lambda (nav to dt) (doubles (first to) (second to) (max +double_-5 (third to)))))
(. n_nav :go (n_at 0 0 -30))
(times 300 (. n_nav :step +n_dt))
(assert-true "stuck, and told" (> (get :stucks n_nav) 0))

;a player's: the stick right, it turns right, leaning into it
(defq n_nav (Nav (n_at 0 0 0) (n_at 0 0 -1)))
(def n_nav :aim_fn nav-aim-pointer :up_fn nav-up-lean :stick (list +double_1 +double_0) :lean (n2d 2))
(times 15 (. n_nav :step +n_dt))
(assert-true "turned right" (> (first (get :way n_nav)) +double_0))
(assert-true "leaning into it" (> (first (get :up n_nav)) +double_0))

;its state, kept and set again
(defq n_state (. n_nav :state) n_other (Nav (n_at 5 5 5) (n_at 1 0 0)))
(. n_other :state n_state)
(assert-true "its state set again" (and (n_near (get :at n_other) (get :at n_nav) +n_eps)
	(n_near (get :way n_other) (get :way n_nav) +n_eps)))
