(report-header "Mover")

(import "lib/physics/move.inc")

;places, near, and a frame step
(defq +m_dt (/ +double_1 (n2d 60))
	m_at (lambda (x y z) (doubles (n2d x) (n2d y) (n2d z)))
	m_near (lambda (a b e) (< (reduce max (map (lambda (x y) (abs (- x y))) a b) +double_0) e))
	m_len (lambda (v) (sqrt (nums-dot v v))))

;a body goes on by its velocity, the app's own doubles moved
(defq m (Mover (m_at 100 100 100)) m_place (m_at 10 10 10)
	m_a (. m :add m_place (m_at 6 0 0)))
(times 60 (. m :step +m_dt))
(assert-true "on by its velocity" (m_near m_place (m_at 16 10 10) (n2d 0.001)))
(assert-true "the app's own place" (eql m_place (. m :place m_a)))

;out of one side of the box and in at the other
(. m :set m_a :mass +double_1)
(elem-set (. m :velocity m_a) 0 (n2d -1200))
(. m :step +m_dt)
(assert-true "in at the other side" (> (first m_place) (n2d 90)))

;a kick by its mass, at once, or taken up at its rate
(defq m (Mover) m_a (. m :add (m_at 0 0 0)) m_b (. m :add (m_at 0 0 0)))
(. m :set m_a :mass +double_2)
(. m :impulse m_a (m_at 4 0 0))
(assert-true "a kick over its mass" (m_near (. m :velocity m_a) (m_at 2 0 0) (n2d 0.001)))
(. m :set m_b :take +double_4)
(. m :push m_b (m_at 4 0 0))
(assert-true "taken up, not yet" (m_near (. m :velocity m_b) (m_at 0 0 0) (n2d 0.001)))
(. m :step +m_dt)
(assert-true "taken up a share a step" (< +double_0 (first (. m :velocity m_b)) +double_1))
(times 120 (. m :step +m_dt))
(assert-true "and all of it in time" (m_near (. m :velocity m_b) (m_at 4 0 0) (n2d 0.01)))

;damped by its drag, and no faster than its top
(defq m (Mover) m_a (. m :add (m_at 0 0 0) (m_at 10 0 0)) m_b (. m :add (m_at 0 0 0) (m_at 10 0 0)))
(. m :set m_a :drag +double_1)
(. m :set m_b :top +double_4)
(times 60 (. m :step +m_dt))
(assert-true "damped" (< (m_len (. m :velocity m_a)) (n2d 4)))
(assert-true "no faster than its top" (m_near (. m :velocity m_b) (m_at 4 0 0) (n2d 0.001)))

;a shove eased in, never a jump
(defq m (Mover) m_a (. m :add (m_at 0 0 0)))
(. m :shove m_a (m_at 3 0 0))
(. m :step +m_dt)
(assert-true "a shove, a part a step" (< +double_0 (first (. m :place m_a)) +double_1))
(times 120 (. m :step +m_dt))
(assert-true "and all of it in time" (m_near (. m :place m_a) (m_at 3 0 0) (n2d 0.01)))
(. m :shove m_a (m_at 2 0 0) :t)
(. m :shove m_a (m_at 2 0 0) :t)
(times 120 (. m :step +m_dt))
(assert-true "a shove as now, not added up" (m_near (. m :place m_a) (m_at 5 0 0) (n2d 0.01)))

;two meeting head on: bounced apart, the lighter the more, and shoved
;apart; going apart, no bounce
(defq m (Mover) m_a (. m :add (m_at 0 0 0) (m_at 2 0 0)) m_b (. m :add (m_at 3 0 0) (m_at -2 0 0)))
(. m :set m_b :mass (n2d 8))
(defq m_closing (. m :hit m_a m_b (m_at 1 0 0) +double_1 (n2d 1.5) (n2d 1.5)))
(assert-true "closing" (< m_closing +double_0))
(assert-true "both turned back" (and (< (first (. m :velocity m_a)) +double_0) (< (first (. m :velocity m_b)) +double_0)))
(assert-true "the lighter turned the more"
	(> (- +double_2 (first (. m :velocity m_a))) (* (n2d 4) (- (first (. m :velocity m_b)) +double_-2))))
(times 60 (. m :step +m_dt))
(assert-true "and shoved apart" (> (- (first (. m :place m_b)) (first (. m :place m_a))) (n2d 3)))
(assert-eq "going apart, no bounce" +double_0 (. m :hit m_a m_b (m_at 1 0 0) +double_0 +double_1 +double_1))

;a kinematic body is not moved by a kick nor a hit, but moves the other
(defq m (Mover) m_a (. m :add (m_at 0 0 0) (m_at 2 0 0)) m_b (. m :add (m_at 3 0 0)))
(. m :set m_a :kinematic :t)
(. m :impulse m_a (m_at -9 0 0))
(assert-true "a kick does not move it" (m_near (. m :velocity m_a) (m_at 2 0 0) (n2d 0.001)))
(. m :hit m_a m_b (m_at 1 0 0) +double_1 +double_1 +double_1)
(assert-true "nor a hit" (m_near (. m :velocity m_a) (m_at 2 0 0) (n2d 0.001)))
(assert-true "the other moved off it" (> (first (. m :velocity m_b)) (n2d 2)))

;one following another has its velocity, at once or eased to it
(defq m (Mover) m_a (. m :add (m_at 0 0 0) (m_at 5 0 0)) m_b (. m :add (m_at 0 0 0))
	m_c (. m :add (m_at 0 0 0)))
(. m :set m_b :follows m_a)
(. m :set m_c :follows m_a :follow_rate +double_2)
(. m :step +m_dt)
(assert-true "following, its leader's velocity" (m_near (. m :velocity m_b) (m_at 5 0 0) (n2d 0.001)))
(assert-true "eased to it" (< +double_0 (first (. m :velocity m_c)) +double_1))
(. m :remove m_a)
(assert-eq "its leader gone, it follows none" -1 (elem-get (get :follows m) m_b))
(assert-eq "a number used again" m_a (. m :add (m_at 0 0 0)))

;its velocity turned to the way it faces, its speed kept
(defq m (Mover) m_a (. m :add (m_at 0 0 0) (m_at 4 0 0)))
(. m :set m_a :align (n2d 30))
(. m :face m_a (m_at 0 0 1))
(times 30 (. m :step +m_dt))
(assert-true "turned to the way it faces" (> (third (. m :velocity m_a)) (n2d 3.9)))

;a spin, no faster than its top, its turn kept of length 1
(defq m (Mover) m_spin (doubles +double_0 +double_0 +double_0 +double_1 +double_0 +double_0 +double_0)
	m_a (. m :add (m_at 0 0 0) :nil m_spin))
(. m :set m_a :spin_top +double_1)
(. m :spin m_a (m_at 0 3 0))
(assert-true "no faster than its top" (m_near (slice m_spin 0 3) (m_at 0 1 0) (n2d 0.001)))
(times 60 (. m :step +m_dt))
(assert-true "turned, of length 1" (m_near (list (m_len (slice m_spin 3 7))) (list +double_1) (n2d 0.0001)))
(assert-true "turned about its way" (> (abs (elem-get m_spin 5)) (n2d 0.4)))

;damped toward what it is in: a river's flow carries it along, its own or
;the mover's of where it is; a steady acceleration, gravity
(defq m (Mover) m_a (. m :add (m_at 0 0 0)) m_b (. m :add (m_at 0 0 0)) m_c (. m :add (m_at 0 0 0)))
(. m :set m_a :drag +double_2 :flow (m_at 3 0 0))
(. m :set m_b :drag +double_2)
(def m :flow_fn (lambda (p) (m_at 0 0 -2)))
(. m :set m_c :accel (m_at 0 -10 0))
(times 120 (. m :step +m_dt))
(assert-true "carried along by its flow" (m_near (. m :velocity m_a) (m_at 3 0 0) (n2d 0.1)))
(assert-true "or the mover's of where it is" (m_near (. m :velocity m_b) (m_at 0 0 -2) (n2d 0.1)))
(assert-true "falling" (m_near (. m :velocity m_c) (m_at 0 -20 0) (n2d 0.01)))

;the step in VP gives what the step in Lisp does, to the bit: two movers made
;alike, every number a body has in use among them, a flow function, a body
;kinematic and one removed, stepped by each and every double the same. The
;middle of the box and how far round it, how fast, the least rate and its
;spread, a take and a mass, a kick, a follow rate, a way faced and how fast,
;gravity, a drag, a flow and a flow function's, a top
(defq +m_mid (n2d 50) +m_spread (n2d 49) +m_fast (n2d 30) +m_rate_least +double_2 +m_rate_spread +double_8
	+m_take (n2d 5) +m_mass +double_3 +m_kick_x +double_4 +m_kick_z (n2d -2) +m_follow +double_4
	+m_face (doubles (n2d 0.6) (n2d 0.8) +double_0) +m_align +double_3
	+m_gravity (doubles +double_0 (n2d -9.8) +double_1) +m_drag (n2d 1.5)
	+m_flow (doubles +double_3 +double_0 +double_-1) +m_flow_x (n2d 0.01) +m_flow_z +double_1/2 +m_top (n2d 5))

(defun m-world ()
	; (m-world) -> mover
	(defq m (Mover (m_at 100 100 100)) r (lambda (k) (sin (n2d (* k 7919)))))
	(each! (lambda ()
			(defq i (!) id (. m :add (doubles (+ +m_mid (* +m_spread (r i))) (+ +m_mid (* +m_spread (r (+ i 1000))))
						(+ +m_mid (* +m_spread (r (+ i 2000)))))
					(doubles (* +m_fast (r (+ i 3000))) (* +m_fast (r (+ i 4000))) (* +m_fast (r (+ i 5000))))
					(if (= (% i 3) 1) (doubles (r (+ i 6000)) (r (+ i 7000)) (r (+ i 8000)) +double_1 +double_0 +double_0 +double_0))
					(doubles (r (+ i 9000)) (r (+ i 9100)) (r (+ i 9200)))))
			(. m :set id :rate (+ +m_rate_least (* +m_rate_spread (abs (r (+ i 9300))))))
			(when (= (% i 2) 0)
				(. m :set id :take +m_take :mass +m_mass)
				(. m :impulse id (doubles +m_kick_x (r i) +m_kick_z)))
			(and (= (% i 3) 0) (> i 0) (. m :set id :follows (dec i) :follow_rate (if (= (% i 9) 0) +double_0 +m_follow)))
			(when (= (% i 4) 0)
				(. m :face id +m_face)
				(. m :set id :align +m_align))
			(if (= (% i 5) 0) (. m :set id :accel +m_gravity))
			(if (= (% i 6) 0) (. m :set id :drag +m_drag :flow +m_flow))
			(if (= (% i 6) 1) (. m :set id :drag +double_2))
			(if (= (% i 7) 0) (. m :set id :top +m_top))
			(if (= i 11) (. m :set id :kinematic :t))
			(. m :spin id (doubles (r (+ i 9400)) +double_0 +double_0)))
		'() 0 60)
	(. m :remove 13)
	(def m :flow_fn (lambda (p) (doubles (* (first p) +m_flow_x) +double_0 +m_flow_z)))
	m)

(defq m_v (m-world) m_l (m-world))
(times 240 (. m_v :step +m_dt) (. m_l :step_lisp +m_dt))
(assert-true "the VP step gives the Lisp's, every double"
	(every (lambda (key)
			(every (lambda (a b) (if a (eql a b) (not b))) (get key m_v) (get key m_l)))
		'(:places :vels :shoves :kicks :spins)))
(assert-true "and they went somewhere" (not (eql (. m_v :place 0) (. (m-world) :place 0))))
(assert-error "a part of a body not a doubles" (progn (. m_v :set 2 :accel (list 1 2 3)) (. m_v :step +m_dt))
	{(move-step dt wrap})
