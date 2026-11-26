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
;then a shark or a ray, and each kind's schools at most, how many fish in each at
;least and more, how long a fish is, how fast it swims at least and most,
;and how deep and wide it is of its length
(defq +fish_kinds 4)

;module
(env-push)

(defq +kinds ''(
		;schools fish more long slow fast deep wide
		(3 15 10 0.5 1.6 3.4 0.24 0.12)
		(2 4 4 0.9 1.2 2.6 0.34 0.13)
		(1 1 0 3.0 0.8 1.5 0.19 0.17)
		(1 1 0 2.2 0.6 1.1 0.06 1.0))
	;the shark and the ray, how seldom one comes, one frame in so many, how
	;near the shark the others flee, how far over the floor a ray keeps and
	;how hard it makes for it
	+shark 2 +ray 3 +shark_seldom 1200 +shark_near (n2d 9) +ray_over +double_2 +ray_dive (n2d 0.6)
	;a ray's mesh: steps along and across it, how broad it is at most, how
	;far back its broadest, how high its back, its tail, a thin blade, and
	;how far its wings flap, of its length
	+ray_along 8 +ray_across 8 +ray_span (n2d 0.62) +ray_chord (n2d 0.8) +ray_e (n2d 0.01) +ray_broadest (n2d 0.8) +ray_back (n2d 0.06)
	+ray_tail ''((-0.3 0 0.01) (-0.95 0 0) (-0.3 0 -0.01)) +ray_flap (n2d 0.14)
	;the mesh: rings along the body, sides round each, how long the body is
	;to the root of the tail, where it is deepest, how much it narrows to
	;the tail, how much its normals lean to the nose
	+rings 10 +sides 8 +body_long (n2d 0.86) +deepest (n2d 2.89) +taper (n2d 0.55) +nose_lean (n2d 0.4)
	;the fins, triangles of corners x, y of how deep, z of how wide: the
	;tail, forked, and a shark's; the fin on the back, and a shark's, its
	;foot just inside the back as the body narrows; a shark's fins out each
	;side
	+tail ''((-0.36 0 0) (-0.5 0.75 0) (-0.44 0 0) (-0.36 0 0) (-0.44 0 0) (-0.5 -0.75 0))
	+shark_tail ''((-0.36 0 0) (-0.5 1.3 0) (-0.44 0 0) (-0.36 0 0) (-0.44 0 0) (-0.5 -0.6 0))
	+fin ''((0.15 0.36 0) (-0.1 0.75 0) (-0.2 0.14 0))
	+shark_fin ''((0.15 0.36 0) (-0.1 1.25 0) (-0.2 0.14 0))
	+pectorals ''((0.2 -0.25 0) (-0.06 -0.5 1.1) (0.06 -0.25 0) (0.2 -0.25 0) (-0.06 -0.5 -1.1) (0.06 -0.25 0))
	;the range of a random number, how far up or down a school comes, how
	;clear the water must be there, and where it wanders to, how much it
	;keeps level
	;an eye: rows and round of its ball, how far back on the body, how far
	;up, of the head there, how far in it, of how big, how big of how deep
	;the fish is, a shark's; a ray's, how far back and out on it, how big,
	;and how far its middle is over its back, of how big, and how far over
	;its back the dome's foot is
	+eye_rows 4 +eye_round 6 +eye_x (n2d 0.36) +eye_up (n2d 0.25) +eye_in (n2d 0.33) +eye_size (n2d 0.12)
	+shark_eye (n2d 0.087) +ray_eye_s (n2d 0.28) +ray_eye_t (n2d 0.14) +ray_eye (n2d 0.022) +ray_eye_up (n2d 0.33)
	+ray_eye_clear (n2d 0.003)
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
	;how much further off they flee the diver's scooter at its loudest
	+noise_shy (n2d 1.5)
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

(defun fish-corner (p n &optional paint)
	; (fish-corner p n [paint]) -> doubles
	;a corner, where it is, its normal, and 1 if of an eye
	(cat p (const (doubles +double_1)) n (doubles (ifn paint +double_0))))

(defun fish-eye (out c r &optional least_y)
	; (fish-eye out c r [least_y]) -> out
	;an eye, a ball at c of radius r, its corners marked as of an eye; with
	;a least_y, none of it lower than that, a dome on a ray's thin back
	(defq rows (map! (lambda ()
			(defq th (* +double_pi (/ (n2d (!)) (n2d +eye_rows))) y (cos th) w (sin th))
			(map! (lambda ()
					(defq ph (* +double_2pi (/ (n2d (!)) (n2d +eye_round))) d (doubles (* w (cos ph)) y (* w (sin ph)))
						p (nums-add c (nums-scale d r)))
					(and least_y (< (second p) least_y) (elem-set p 1 least_y))
					(fish-corner p d +double_1))
				'() 0 +eye_round))
		'() 0 (inc +eye_rows)))
	(each (lambda (r0 r1)
			(each! (lambda ()
					(defq i (!) j (% (inc i) +eye_round))
					(push out (elem-get r0 i) (elem-get r0 j) (elem-get r1 j)
						(elem-get r0 i) (elem-get r1 j) (elem-get r1 i)))
				'() 0 +eye_round))
		(slice rows 0 -2) (slice rows 1 -1))
	out)

(defun fish-fins (out tris deep wide n)
	; (fish-fins out tris deep wide n) -> out
	;the triangles of fins, each corner x, y of how deep and z of how wide,
	;their normal n
	(each (lambda ((x y z))
			(push out (fish-corner (doubles (n2d x) (* (n2d y) deep) (* (n2d z) wide)) n)))
		tris)
	out)

(defun ray-at (s t)
	; (ray-at s t) -> doubles
	;a place on a ray, s nose to rear, t one wing tip to the other, a
	;diamond, broadest well back, its back raised along the middle
	(defq along (- +double_1/2 (* s +ray_chord)) broad (* +ray_span (sin (* +double_pi (pow s +ray_broadest)))))
	(doubles along (* +ray_back (- +double_1 (* t t)) (- +double_1 s)) (* t broad)))

(defun ray-corner (s t e)
	; (ray-corner s t e) -> doubles
	;a corner of a ray's sheet, its normal of the slope there
	(defq p (ray-at s t) a (nums-sub (ray-at (+ s e) t) p) b (nums-sub (ray-at s (+ t e)) p))
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(defq n (fish-unit (doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))
	(if (< (second n) +double_0) (setq n (nums-scale n +double_-1)))
	(fish-corner p n))

(defun ray-mesh ()
	; (ray-mesh) -> str
	;a ray as the shaders want it: its body a sheet, its normals of its
	;slope, and its tail
	(defq out (list) e +ray_e)
	(each! (lambda ()
			(defq i (!) s0 (/ (n2d i) (n2d +ray_along)) s1 (/ (n2d (inc i)) (n2d +ray_along)))
			(each! (lambda ()
					(defq j (!) t0 (- (/ (* +double_2 (n2d j)) (n2d +ray_across)) +double_1)
						t1 (- (/ (* +double_2 (n2d (inc j))) (n2d +ray_across)) +double_1))
					(push out (ray-corner s0 t0 e) (ray-corner s0 t1 e) (ray-corner s1 t1 e)
						(ray-corner s0 t0 e) (ray-corner s1 t1 e) (ray-corner s1 t0 e)))
				'() 0 +ray_across))
		'() 0 +ray_along)
	(fish-fins out +ray_tail +double_1 +double_1 (const (doubles +double_0 +double_1 +double_0)))
	;its eyes on top of its head
	(each (lambda (t)
			(defq at (ray-at +ray_eye_s t) back (+ (second at) +ray_eye_clear))
			(elem-set at 1 (+ (second at) (* +ray_eye +ray_eye_up)))
			(fish-eye out at +ray_eye back))
		(list +ray_eye_t (neg +ray_eye_t)))
	(shader-verts-str (apply (const cat) out)))

(defun body-mesh (kind)
	; (body-mesh kind) -> str
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
	;its eyes, each side of its head, set into it; a shark's small
	(defq t (/ (- +double_1/2 +eye_x) +body_long) k (* (sin (* +deepest (sqrt t))) (- +double_1 (* +taper t t)))
		r (* deep (if shark +shark_eye +eye_size)) y (* deep k +eye_up)
		;the side of the head at that height, and the eye a little in it
		z (- (* wide k +double_1/2 (sqrt (- +double_1 (* +double_4 +eye_up +eye_up)))) (* r +eye_in)))
	(fish-eye out (doubles +eye_x y z) r)
	(fish-eye out (doubles +eye_x y (neg z)) r)
	(shader-verts-str (apply (const cat) out)))

(defun fish-mesh (kind)
	; (fish-mesh kind) -> str
	;the mesh of a kind of fish as the shaders want it, a vertex where it is
	;and its normal: the body, rings round x, deepest a third back from the
	;nose; the tail, forked, a shark's upper blade the longer; the fin on its
	;back, a shark's tall; and a shark's fins out each side
	(if (= kind +ray) (ray-mesh) (body-mesh kind)))

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

(defun fish-steer (water eye fish mates long dt sharks low shy)
	; (fish-steer water eye fish mates long dt sharks low shy)
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
	;a ray down to keep near the floor
	(defq h (water at))
	(and low (> h +ray_over) (setq want (nums-add want (doubles +double_0 (neg +ray_dive) +double_0))))
	;from the rock, along the slope of the field, and from the surface
	(when (< h +rock_near)
		(defq e +double_1/2 g (doubles (- (water (nums-add at (doubles e +double_0 +double_0))) h)
				(- (water (nums-add at (doubles +double_0 e +double_0))) h)
				(- (water (nums-add at (doubles +double_0 +double_0 e))) h)))
		(setq want (nums-add want (nums-scale (fish-unit g) (* +from_rock (- +double_1 (/ h +rock_near)))))))
	;from the diver, and from a shark
	(when (< (defq dd (nums-dot (defq d (nums-sub at eye)) d)) (* shy shy))
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

(defun schools-move (schools water eye forward view dt surface noise)
	; (schools-move schools water eye forward view dt surface noise) -> schools
	;the schools moved on a step, those gone far off let go of and new ones
	;come in their place ahead, as many of each kind as there should be. The
	;louder the diver's scooter, noise 0 to 1, the further off they flee it
	(defq out (list) now (pii-time) sharks (list) shy (* +diver_near (+ +double_1 (* noise +noise_shy))))
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
						(fish-steer water eye f fish long dt (if (= kind +shark) '() sharks) (= kind +ray) shy)
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
				(and (or (< kind +shark) (= (random +shark_seldom) 0))
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
				(defq long (n2d (elem-get (elem-get +kinds kind) 3))
					swing (cond ((= kind +shark) +shark_swing) ((= kind +ray) +ray_flap) (+swing)))
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
