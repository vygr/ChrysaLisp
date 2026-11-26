;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water's fish: their meshes, a job a child of the farm does, and their
; schools, which the app moves each frame.
;
; A fish is a body lofted along x, its nose at +1/2 and its tail at -1/2,
; round rings of it as deep and as wide as the kind is, a forked tail and a
; fin on its back. The vertex shader beats its tail, a wave down its body
; the more the nearer the tail, so x is kept as it is in the mesh.
;
; A school is fish of one kind, each with where it is and how fast it goes
; which way. Each fish keeps apart from those too near it, goes the way
; those near it go, keeps to the middle of them, and makes for the place
; the school is wandering to, a new one now and then in the open water; it
; turns from the rock, along the slope of the field, keeps under the
; surface, and darts away from the diver. A school comes into being ahead
; of the diver, out where the water hides it, and is let go of when it is
; further off than that, so there are always about as many round, new ones
; met as the diver swims.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")
(import "lib/gpu/vp.inc")

;the kinds of fish: silver schooling fish, striped reef fish, and now and
;then a shark, and each kind's schools at most, how many fish in each at
;least and more, how long a fish is, how fast it swims at least and most,
;and how deep and wide it is of its length
(defq +fish_kinds 3)

;module
(env-push)

(defq +kinds ''(
		;schools fish more long slow fast deep wide
		(3 15 10 0.5 1.6 3.4 0.24 0.12)
		(2 4 4 0.9 1.2 2.6 0.34 0.13)
		(1 1 0 3.0 0.8 1.5 0.19 0.17))
	;the shark, how seldom one comes, one frame in so many, and how near it
	;the others flee
	+shark 2 +shark_seldom 2400 +shark_near (n2d 9)
	;the mesh: rings along the body, sides round each, how long the body is
	;to the root of the tail, where it is deepest, how much it narrows to
	;the tail, how much its normals lean to the nose
	+rings 10 +sides 8 +body_long (n2d 0.86) +deepest (n2d 2.89) +taper (n2d 0.55) +nose_lean (n2d 0.4)
	;the fins, triangles of corners x, y of how deep, z of how wide: the
	;tail, forked, and a shark's; the fin on the back, and a shark's; a
	;shark's fins out each side
	+tail ''((-0.36 0 0) (-0.5 0.75 0) (-0.44 0 0) (-0.36 0 0) (-0.44 0 0) (-0.5 -0.75 0))
	+shark_tail ''((-0.36 0 0) (-0.5 1.3 0) (-0.44 0 0) (-0.36 0 0) (-0.44 0 0) (-0.5 -0.6 0))
	+fin ''((0.12 0.45 0) (-0.1 0.75 0) (-0.2 0.4 0))
	+shark_fin ''((0.12 0.45 0) (-0.1 1.25 0) (-0.2 0.4 0))
	+pectorals ''((0.2 -0.25 0) (-0.02 -0.4 2.4) (0.06 -0.25 0) (0.2 -0.25 0) (-0.02 -0.4 -2.4) (0.06 -0.25 0))
	;the range of a random number, how far up or down a school comes, how
	;clear the water must be there, and where it wanders to, how much it
	;keeps level
	;how fast a tail beats, of how fast it swims a length, and how far it
	;swings, of its length, a shark's
	+beat (n2d 4.5) +swing (n2d 0.1) +shark_swing (n2d 0.07)
	+rnd_range 1000000 +spot_spread (n2d 12) +spot_clear (n2d 6) +goal_clear (n2d 5) +level_by (n2d 0.7)
	;how far a fish looks for others, of its length, how near is too near,
	;and how much each pull counts: apart, the same way, to the middle, to
	;where the school goes, from the rock, from the diver
	+sees (n2d 6) +too_near (n2d 1.6) +apart +double_2 +same (n2d 0.6) +middle (n2d 0.4) +goal (n2d 0.5)
	+from_rock (n2d 4) +from_diver (n2d 6)
	;how near the rock it turns, how near the surface and the diver, how
	;fast it turns, a second, how far a school wanders to next and how long
	;it takes, and where it comes and goes, of how far the frame sees
	+rock_near (n2d 3.5) +surface_near (n2d 4) +diver_near (n2d 5) +turn_rate (n2d 2.5)
	+wander_most (n2d 25) +wander_long 6000000 +comes (n2d 0.85) +goes (n2d 0.95)
	;how far ahead of the diver schools wander to, and where the first come,
	;of how far the frame sees, at least and more
	+ahead_goal (n2d 30) +first_least (n2d 0.2) +first_more (n2d 0.4)
	;how steep a fish swims at most, of its way
	+steep_most (n2d 0.45))

(defun fish-rnd ()
	; (fish-rnd) -> double
	(/ (n2d (random +rnd_range)) (const (n2d +rnd_range))))

(defun fish-unit (v)
	; (fish-unit v) -> doubles
	(defq vv (nums-dot v v))
	(if (> vv +double_0) (nums-scale v (recip (sqrt vv))) (doubles +double_1 +double_0 +double_0)))

(defun fish-corner (p n)
	; (fish-corner p n) -> doubles
	(cat p (const (doubles +double_1)) n))

(defun fish-fins (out tris deep wide n)
	; (fish-fins out tris deep wide n) -> out
	;the triangles of fins, each corner x, y of how deep and z of how wide,
	;their normal n
	(each (lambda ((x y z))
			(push out (fish-corner (doubles (n2d x) (* (n2d y) deep) (* (n2d z) wide)) n)))
		tris)
	out)

(defun fish-mesh (kind)
	; (fish-mesh kind) -> str
	;the mesh of a kind of fish as the shaders want it, a vertex where it is
	;and its normal: the body, rings round x, deepest a third back from the
	;nose; the tail, forked, a shark's upper blade the longer; the fin on its
	;back, a shark's tall; and a shark's fins out each side
	(bind '(& & & & & & deep wide) (elem-get +kinds kind))
	(defq deep (n2d deep) wide (n2d wide) out (list) shark (= kind +shark)
		rings (map! (lambda ()
				(defq t (/ (n2d (!)) (n2d +rings)) x (- +double_1/2 (* t +body_long))
					;how thick here, nose to the narrow before the tail
					k (* (sin (* +deepest (sqrt t))) (- +double_1 (* +taper t t))))
				(map! (lambda ()
						(defq a (* (n2d (!)) (const (/ +double_2pi (n2d +sides)))) cy (cos a) sz (sin a))
						(list (doubles x (* cy deep k +double_1/2) (* sz wide k +double_1/2))
							(fish-unit (doubles (* (- +double_1/2 t) +nose_lean) (/ cy deep) (/ sz wide)))))
					'() 0 +sides))
			'() 0 (inc +rings)))
	(each (lambda (r0 r1)
			(each! (lambda ()
					(defq i (!) j (% (inc i) +sides))
					(bind '(a na) (elem-get r0 i))
					(bind '(b nb) (elem-get r0 j))
					(bind '(c nc) (elem-get r1 j))
					(bind '(d nd) (elem-get r1 i))
					(push out (fish-corner a na) (fish-corner b nb) (fish-corner c nc)
						(fish-corner a na) (fish-corner c nc) (fish-corner d nd)))
				'() 0 +sides))
		(slice rings 0 -2) (slice rings 1 -1))
	(defq side (const (doubles +double_0 +double_0 +double_1)))
	(fish-fins out (if shark +shark_tail +tail) deep wide side)
	(fish-fins out (if shark +shark_fin +fin) deep wide side)
	(if shark (fish-fins out +pectorals deep wide (const (doubles +double_0 +double_1 +double_0))))
	(shader-verts-str (apply (const cat) out)))

(defun fish-spot (water eye forward far)
	; (fish-spot water eye forward far) -> :nil | doubles
	;a place in the open water far off ahead of the eye, of a few tries
	(defq at :nil)
	(times 8
		(unless at
			(defq a (* (- (fish-rnd) +double_1/2) +double_pi) c (cos a) s (sin a)
				fx (first forward) fz (third forward) fl (sqrt (+ (* fx fx) (* fz fz))))
			(if (> fl +double_0) (setq fx (/ fx fl) fz (/ fz fl)) (setq fx +double_1 fz +double_0))
			(defq p (doubles (+ (first eye) (* far (- (* fx c) (* fz s))))
					(+ (second eye) (* (- (fish-rnd) +double_1/2) +spot_spread))
					(+ (third eye) (* far (+ (* fx s) (* fz c))))))
			(if (> (water p) +spot_clear) (setq at p))))
	at)

(defun school-new (water eye forward far kind)
	; (school-new water eye forward far kind) -> :nil | school
	;a school of a kind far off ahead of the eye: (kind goal goal_at fish),
	;each fish (at way speed phase)
	(when (defq at (fish-spot water eye forward far))
		(bind '(& n more & slow fast &ignore) (elem-get +kinds kind))
		(defq heading (fish-unit (doubles (- (fish-rnd) +double_1/2) +double_0 (- (fish-rnd) +double_1/2))))
		(list kind at (pii-time)
			(map! (lambda ()
					(list (nums-add at (doubles (* (- (fish-rnd) +double_1/2) +double_4) (* (- (fish-rnd) +double_1/2) +double_2)
							(* (- (fish-rnd) +double_1/2) +double_4)))
						heading (+ (n2d slow) (* (fish-rnd) (- (n2d fast) (n2d slow)))) (* (fish-rnd) +double_2pi)))
				'() 0 (+ n (random (inc more)))))))

(defun fish-steer (water eye fish mates long dt sharks)
	; (fish-steer water eye fish mates long dt sharks)
	;one fish turned toward where its school and the water would have it go,
	;and moved on
	(bind '(at way speed &ignore) fish)
	(defq push_by (doubles +double_0 +double_0 +double_0) mean_way (doubles +double_0 +double_0 +double_0)
		mean_at (doubles +double_0 +double_0 +double_0) n 0 sees (* +sees long) near (* +too_near long))
	(each (lambda ((o_at o_way &ignore))
			(defq d (nums-sub at o_at) dd (nums-dot d d))
			(when (and (> dd +double_0) (< dd (* sees sees)))
				(++ n)
				(setq mean_way (nums-add mean_way o_way) mean_at (nums-add mean_at o_at))
				(if (< dd (* near near)) (setq push_by (nums-add push_by (nums-scale d (/ near dd)))))))
		mates)
	(defq want (nums-scale way +double_1))
	(when (> n 0)
		(setq want (nums-add want (nums-add (nums-scale push_by +apart)
			(nums-add (nums-scale (fish-unit mean_way) +same)
				(nums-scale (fish-unit (nums-sub (nums-scale mean_at (recip (n2d n))) at)) +middle))))))
	(setq want (nums-add want (nums-scale (fish-unit (nums-sub (elem-get fish 4) at)) +goal)))
	;from the rock, along the slope of the field, and from the surface
	(when (< (defq h (water at)) +rock_near)
		(defq e +double_1/2 g (doubles (- (water (nums-add at (doubles e +double_0 +double_0))) h)
				(- (water (nums-add at (doubles +double_0 e +double_0))) h)
				(- (water (nums-add at (doubles +double_0 +double_0 e))) h)))
		(setq want (nums-add want (nums-scale (fish-unit g) (* +from_rock (- +double_1 (/ h +rock_near)))))))
	;from the diver, and from a shark
	(when (< (defq dd (nums-dot (defq d (nums-sub at eye)) d)) (* +diver_near +diver_near))
		(setq want (nums-add want (nums-scale (fish-unit d) +from_diver))))
	(each (lambda (shark_at)
			(when (< (nums-dot (defq d (nums-sub at shark_at)) d) (* +shark_near +shark_near))
				(setq want (nums-add want (nums-scale (fish-unit d) +from_diver)))))
		sharks)
	(setq want (fish-unit want))
	;turned toward it no faster than it can, level the more, and on
	(defq k (min +double_1 (* dt +turn_rate)) way (fish-unit (nums-add way (nums-scale (nums-sub want way) k))))
	;never steeper than a fish swims, and down again from the surface
	(elem-set way 1 (max (neg +steep_most) (min +steep_most (* (second way) +level_by))))
	(if (> (second at) (- (elem-get fish 5) +surface_near +double_1)) (elem-set way 1 (min (second way) (neg +double_1/4))))
	(setq way (fish-unit way))
	(elem-set fish 1 way)
	(elem-set fish 0 (nums-add at (nums-scale way (* speed dt)))))

(defun schools-move (schools water eye forward view dt surface)
	; (schools-move schools water eye forward view dt surface) -> schools
	;the schools moved on a step, those gone far off let go of and new ones
	;come in their place ahead, as many of each kind as there should be
	(defq out (list) now (pii-time) sharks (list))
	(each (lambda ((kind & & fish))
			(if (= kind +shark) (push sharks (first (first fish)))))
		schools)
	(each (lambda (school)
			(bind '(kind goal goal_at fish) school)
			(defq long (n2d (elem-get (elem-get +kinds kind) 3)) mid (doubles +double_0 +double_0 +double_0))
			(each (# (setq mid (nums-add mid (first %0)))) fish)
			(setq mid (nums-scale mid (recip (n2d (length fish)))))
			(when (< (sqrt (nums-dot (defq d (nums-sub mid eye)) d)) (* view +goes))
				;a new place to wander to now and then
				(when (or (> (- now goal_at) +wander_long) (< (nums-dot (defq g (nums-sub goal mid)) g) +double_4))
					;somewhere ahead of the diver, so schools cross its way
					(defq to (nums-add (nums-add eye (nums-scale forward +ahead_goal))
						(doubles (* (- (fish-rnd) +double_1/2) +double_2 +wander_most)
							(* (- (fish-rnd) +double_1/2) +double_4) (* (- (fish-rnd) +double_1/2) +double_2 +wander_most))))
					(elem-set to 1 (min (second to) (- surface +surface_near)))
					(if (> (water to) +goal_clear) (setq goal to goal_at now))
					(elem-set school 1 goal)
					(elem-set school 2 goal_at))
				(each (lambda (f)
						(push f goal surface)
						(fish-steer water eye f fish long dt (if (= kind +shark) '() sharks))
						(pop f)
						(pop f)
						(defq at (first f))
						(if (> (second at) (- surface +surface_near)) (elem-set at 1 (- surface +surface_near))))
					fish)
				(push out school)))
		schools)
	;as many schools of each kind as there should be
	(each! (lambda ()
			(defq kind (!) have (reduce (lambda (n s) (if (= (first s) kind) (inc n) n)) out 0))
			(times (- (first (elem-get +kinds kind)) have)
				(and (or (/= kind +shark) (= (random +shark_seldom) 0))
					;at first round the diver, after out where the water hides them
					(defq s (school-new water eye forward
						(* view (if (empty? schools) (+ +first_least (* (fish-rnd) +first_more)) +comes)) kind))
					(push out s))))
		'() 0 +fish_kinds)
	out)

(defun fish-matrix (at way long)
	; (fish-matrix at way long) -> mat4x4
	;a fish where it is, its x the way it swims, its y up, of its length
	(defq side (fish-unit (doubles (neg (third way)) +double_0 (first way)))
		up (fish-unit (doubles (- (* (second side) (third way)) (* (third side) (second way)))
			(- (* (third side) (first way)) (* (first side) (third way)))
			(- (* (first side) (second way)) (* (second side) (first way))))))
	(if (< (second up) +double_0) (setq up (nums-scale up +double_-1) side (nums-scale side +double_-1)))
	(bind '(fx fy fz) (nums-scale way long))
	(bind '(ux uy uz) (nums-scale up long))
	(bind '(sx sy sz) (nums-scale side long))
	(bind '(x y z) at)
	(doubles fx ux sx x fy uy sy y fz uz sz z +double_0 +double_0 +double_0 +double_1))

(defun fish-draws (schools had mesh_base lens eye_val pvals files seconds eye forward view)
	; (fish-draws schools had mesh_base lens eye_val pvals files seconds eye forward view) -> draws
	;the fish near enough and not behind, each where it is, swimming its
	;way, its tail beating as fast as it swims, the pixel shader's inputs the
	;one list for all
	(defq out (list) time_val (list 'time seconds))
	(each (lambda ((kind & & fish))
			(when (elem-get had kind)
				(defq long (n2d (elem-get (elem-get +kinds kind) 3)) swing (if (= kind +shark) +shark_swing +swing))
				(each (lambda ((at way speed phase))
						(defq d (nums-sub at eye) far (sqrt (nums-dot d d)))
						(and (< far view) (> (nums-dot d forward) (neg long))
							(push out (list (+ mesh_base kind)
								(list (list 'model (fish-matrix at way long)) lens eye_val
									(list 'fish (doubles (n2d kind) phase (* +beat (/ speed long)) swing)) time_val)
								pvals '() files))))
					fish)))
		schools)
	out)

;module
(export-symbols '(fish-mesh schools-move fish-draws +fish_kinds))
(env-pop)
