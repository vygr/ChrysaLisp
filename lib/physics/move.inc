;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mover, the one way things move: each body a place and a velocity, a
; kick and a shove still to take up and, if it tumbles, a spin and a turn;
; each with its own numbers, as a Nav has its own: how heavy, how damped,
; how fast at most, how fast it takes up a kick and a shove, how it follows
; another, how it bounces, grinds and grips. Not a physics engine: no
; forces summed, no solver. Kicks, shoves and hits change it, and a step
; moves all of it:
;
;	(defq m (Mover (doubles 1000.0 1000.0 1000.0)))
;	(defq rock (. m :add at drift spin))
;	(. m :set rock :mass (* bulk bulk bulk) :spin_top (/ +rim bulk))
;	(. m :impulse rock dv)	a kick, by its mass
;	(. m :push rock dv)	a change of velocity, mass or not
;	(. m :shove rock d)	a change of place, eased in, never a jump
;	(. m :hit a b way depth ra rb)	two meeting, bounced and shoved apart
;	(. m :step dt)
;
; Its places, velocities, shoves and spins are the app's own doubles, given
; to :add, so what reads them reads them moved; a spin is (wx wy wz qw qx
; qy qz), how fast it turns about which way and how it is turned. A body
; may follow another, its velocity that one's, as a buoy keeps by a rock;
; align its velocity to a way it faces, as a ship; or be kinematic, moved
; by the app alone and moving others as if it were too heavy to move.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")

;the step in VP, lib/physics/lisp.vp, the same sums as :step_lisp
(ffi (path-to-absolute "./step") move-step)

;module
(env-push)

(defq +move_none (doubles +double_0 +double_0 +double_0) +move_tiny (/ +double_1 (n2d 1000000))
	;a body's numbers when not said: as heavy as 1, no drag, no top speed,
	;a shove eased in at 10 a second, a little bounce, no grind nor grip,
	;no top spin
	+move_rate (n2d 10) +move_bounce +double_1/10)

(defun move-add-to (v dv k)
	; (move-add-to v dv k)
	;v made more by k of dv, in place
	(elem-set v 0 (+ (elem-get v 0) (* (elem-get dv 0) k)))
	(elem-set v 1 (+ (elem-get v 1) (* (elem-get dv 1) k)))
	(elem-set v 2 (+ (elem-get v 2) (* (elem-get dv 2) k))))

(defclass Mover (&optional wrap) :nil
	; (Mover [wrap]) -> mover
	;bodies in a box that goes on every way, wrap its size each way, a place
	;out of one side in at the other; or in all of space. Its :flow_fn,
	;(flow_fn place) -> :nil | velocity, the velocity of what a body is in
	;there, a river's, a current's, the wind's, that drag takes it to
	(def this :wrap wrap :flow_fn :nil :places (list) :vels (list) :shoves (list) :kicks (list) :spins (list)
		:faces (list) :accels (list) :flows (list)
		:inv_mass (doubles) :drag (doubles) :top (doubles) :rate (doubles) :take (doubles) :bounce (doubles)
		:grind (doubles) :grip (doubles) :spin_top (doubles) :align (doubles) :follows (nums)
		:follow_rate (doubles) :kinematic (nums) :live (nums) :free (list))

	(defmethod :add (place &optional vel spin shove)
		; (. mover :add place [vel spin shove]) -> id
		;a body at a place, going at a velocity, tumbling by a spin, a shove
		;to go, each the app's own doubles, or new; its numbers as not said
		(defq vel (ifn vel (cat +move_none)) shove (ifn shove (cat +move_none))
			id (if (nempty? (get :free this)) (pop (get :free this)) (length (get :places this))))
		(each (lambda (key v)
				(defq vs (get key this))
				(if (< id (length vs)) (elem-set vs id v) (push vs v)))
			'(:places :vels :shoves :kicks :spins :faces :accels :flows :inv_mass :drag :top :rate :take :bounce :grind :grip
				:spin_top :align :follows :follow_rate :kinematic :live)
			(list place vel shove (cat +move_none) spin :nil :nil :nil +double_1 +double_0 +double_0 +move_rate +double_0
				+move_bounce +double_0 +double_0 +double_0 +double_0 -1 +double_0 0 1))
		id)

	(defmethod :remove (id)
		; (. mover :remove id) -> mover
		;a body gone, its number used again
		(elem-set (get :live this) id 0)
		(elem-set (get :follows this) id -1)
		;and none follow it now
		(defq follows (get :follows this))
		(each (lambda (f) (if (= f id) (elem-set follows (!) -1))) follows)
		(push (get :free this) id)
		this)

	(defmethod :set (id &rest pairs)
		; (. mover :set id key val ...) -> mover
		;a body's numbers: :mass; :drag, its damping, a share a second of how
		;much faster it goes than what it is in, still, or its :flow, a
		;velocity of its own, or the mover's :flow_fn of where it is; :accel,
		;a steady acceleration, gravity or a thrust held, :nil none; :top its
		;most speed, 0 none; :take how fast it takes up a kick, a share a
		;second, 0 at once; :rate how fast a shove eases in, a share a second;
		;:bounce; :grind the share of a sideways slide a hit takes; :grip how
		;hard that slide spins it; :spin_top; :align how fast its velocity
		;turns to the way it faces, a share a second; :follows the body whose
		;velocity it has, -1 none, and :follow_rate how fast it comes to it, a
		;share a second, 0 at once; :kinematic :t or :nil
		(each (lambda ((key val))
				(case key
					(:mass (elem-set (get :inv_mass this) id (if (> val +double_0) (recip val) +double_0)))
					(:kinematic (elem-set (get :kinematic this) id (if val 1 0)))
					(:accel (elem-set (get :accels this) id val))
					(:flow (elem-set (get :flows this) id val))
					(:t (elem-set (get key this) id val))))
			(partition pairs 2))
		this)

	(defmethod :place (id)
		; (. mover :place id) -> doubles
		(elem-get (get :places this) id))

	(defmethod :velocity (id)
		; (. mover :velocity id) -> doubles
		(elem-get (get :vels this) id))

	(defmethod :mass (id)
		; (. mover :mass id) -> double
		;0 for a kinematic body, as heavy as can be
		(defq im (elem-get (get :inv_mass this) id))
		(if (or (= im +double_0) (= (elem-get (get :kinematic this) id) 1)) +double_0 (recip im)))

	(defmethod :face (id way)
		; (. mover :face id way) -> mover
		;the way a body faces, its velocity turned to it as fast as :align
		(elem-set (get :faces this) id way)
		this)

	(defmethod :impulse (id dv)
		; (. mover :impulse id dv) -> mover
		;a kick, its velocity to change by dv over its mass, taken up as
		;fast as its :take
		(. this :push id dv (elem-get (get :inv_mass this) id)))

	(defmethod :push (id dv &optional k)
		; (. mover :push id dv [k]) -> mover
		;its velocity to change by k of dv, 1 if not said, whatever its
		;mass: at once, or as a kick taken up as fast as its :take
		(when (= (elem-get (get :kinematic this) id) 0)
			(move-add-to (elem-get (get (if (> (elem-get (get :take this) id) +double_0) :kicks :vels) this) id)
				dv (ifn k +double_1)))
		this)

	(defmethod :shove (id d &optional as_now)
		; (. mover :shove id d [as_now]) -> mover
		;its place to change by d more, eased in over its steps, never a jump;
		;or as_now, d all it is still to change, a correction measured afresh
		;each step, so it does not add up while it is easing in
		(defq s (elem-get (get :shoves this) id))
		(if as_now (nums-scale d +double_1 s) (move-add-to s d +double_1))
		this)

	(defmethod :spin (id dw)
		; (. mover :spin id dw) -> mover
		;its spin made faster about a way, no faster than its :spin_top, if
		;it has one
		(when (defq spin (elem-get (get :spins this) id))
			(defq wx (+ (elem-get spin 0) (elem-get dw 0)) wy (+ (elem-get spin 1) (elem-get dw 1))
				wz (+ (elem-get spin 2) (elem-get dw 2)) w (sqrt (+ (* wx wx) (* wy wy) (* wz wz)))
				most (elem-get (get :spin_top this) id) k (if (and (> most +double_0) (> w most)) (/ most w) +double_1))
			(elem-set spin 0 (* wx k))
			(elem-set spin 1 (* wy k))
			(elem-set spin 2 (* wz k)))
		this)

	(defmethod :hit (a b way depth ra rb &optional ease)
		; (. mover :hit a b way depth ra rb [ease]) -> closing
		;two bodies meeting, way of length 1 from a to b, depth how far one is
		;in the other, ra and rb how far each reaches to where they meet: if
		;coming together, bounced apart along the way, the lighter the more,
		;as little as the less of their :bounce; the slide across taken down
		;by their :grind and spinning each by their :grip, the smaller the
		;faster, as a wheel rolls; and shoved apart, the lighter the more,
		;ease of the shove if not all, eased in by their steps. How fast they
		;were closing, under 0, or 0
		(defq vels (get :vels this) inv (get :inv_mass this) kin (get :kinematic this)
			ia (if (= (elem-get kin a) 1) +double_0 (elem-get inv a))
			ib (if (= (elem-get kin b) 1) +double_0 (elem-get inv b)) both (+ ia ib) closing +double_0)
		(when (> both +double_0)
			(defq va (elem-get vels a) vb (elem-get vels b) wa (/ ia both) wb (/ ib both))
			(bind '(nx ny nz) way)
			(setq closing (+ (* (- (elem-get vb 0) (elem-get va 0)) nx) (* (- (elem-get vb 1) (elem-get va 1)) ny)
				(* (- (elem-get vb 2) (elem-get va 2)) nz)))
			(cond
				((< closing +double_0)
					(defq bounce (min (elem-get (get :bounce this) a) (elem-get (get :bounce this) b))
						grind (max (elem-get (get :grind this) a) (elem-get (get :grind this) b))
						grip (max (elem-get (get :grip this) a) (elem-get (get :grip this) b))
						kick (* (+ +double_1 bounce) closing)
						slide (doubles (- (elem-get vb 0) (elem-get va 0) (* closing nx))
							(- (elem-get vb 1) (elem-get va 1) (* closing ny)) (- (elem-get vb 2) (elem-get va 2) (* closing nz))))
					(move-add-to va way (* kick wa))
					(move-add-to vb way (neg (* kick wb)))
					(move-add-to va slide (* grind wa))
					(move-add-to vb slide (neg (* grind wb)))
					(when (> grip +double_0)
						(bind '(tx ty tz) slide)
						(defq across (doubles (- (* ny tz) (* nz ty)) (- (* nz tx) (* nx tz)) (- (* nx ty) (* ny tx))))
						(if (> ra +double_0) (. this :spin a (nums-scale across (/ (* grip wa) ra))))
						(if (> rb +double_0) (. this :spin b (nums-scale across (/ (* grip wb) rb))))))
				((setq closing +double_0)))
			(defq over (* depth (ifn ease +double_1)))
			(move-add-to (elem-get (get :shoves this) a) way (neg (* over wa)))
			(move-add-to (elem-get (get :shoves this) b) way (* over wb)))
		closing)

	(defmethod :step (dt)
		; (. mover :step dt) -> mover
		;each body moved on dt seconds: as much of its kick taken up as it
		;takes in the time; its velocity come to its leader's if it follows
		;one, turned to the way it faces, damped by its drag, no faster than
		;its top; its place on by it and by as much of its shove as eases in,
		;kept in the box; turned on as fast as it tumbles. A kinematic body
		;moved only by its shove. Done in VP, move-step, as :step_lisp does
		;it, the flows of the mover's :flow_fn found first, for the bodies
		;it moves with drag and no flow of their own
		(defq flows (get :flows this) flow_fn (get :flow_fn this))
		(if flow_fn
			(setq flows (map (lambda (at flow drag alive moved)
					(cond (flow) ((and (= alive 1) (= moved 0) (> drag +double_0)) (flow_fn at))))
				(get :places this) flows (get :drag this) (get :live this) (get :kinematic this))))
		(move-step dt (ifn (get :wrap this) (const (doubles)))
			(get :places this) (get :vels this) (get :shoves this) (get :kicks this) (get :spins this)
			(get :faces this) (get :accels this) flows (get :drag this) (get :top this) (get :rate this)
			(get :take this) (get :align this) (get :follow_rate this) (get :follows this) (get :kinematic this)
			(get :live this))
		this)

	(defmethod :step_lisp (dt)
		; (. mover :step_lisp dt) -> mover
		;the step in Lisp, what move-step does in VP, kept to hold it to
		(defq places (get :places this) vels (get :vels this) shoves (get :shoves this) spins (get :spins this)
			faces (get :faces this) drags (get :drag this) tops (get :top this) rates (get :rate this)
			aligns (get :align this) follows (get :follows this) kin (get :kinematic this) live (get :live this)
			kicks (get :kicks this) takes (get :take this) follow_rates (get :follow_rate this)
			accels (get :accels this) flows (get :flows this) flow_fn (get :flow_fn this)
			wrap (get :wrap this) h (* dt +double_1/2))
		(defq t1 (cat +move_none) t2 (cat +move_none))
		(each (lambda (at v s alive moved)
				(when (= alive 1)
					(defq id (!))
					(when (= moved 0)
						(when (> (defq take (elem-get takes id)) +double_0)
							(defq kick (elem-get kicks id))
							(nums-scale kick (min +double_1 (* dt take)) t1)
							(nums-add v t1 v)
							(nums-sub kick t1 kick))
						(when (>= (defq f (elem-get follows id)) 0)
							(defq fr (elem-get follow_rates id))
							(nums-sub (elem-get vels f) v t1)
							(nums-add v (if (> fr +double_0) (nums-scale t1 (min +double_1 (* dt fr)) t1) t1) v))
						(when (and (defq face (elem-get faces id)) (> (defq al (elem-get aligns id)) +double_0)
								(> (defq speed (sqrt (nums-dot v v))) +move_tiny))
							;turned toward it, its speed kept
							(nums-sub (nums-scale face speed t1) v t1)
							(nums-add v (nums-scale t1 (min +double_1 (* dt al)) t1) v)
							(defq now_speed (sqrt (nums-dot v v)))
							(if (> now_speed +move_tiny) (nums-scale v (/ speed now_speed) v)))
						;its steady acceleration, gravity, a thrust held
						(if (defq accel (elem-get accels id)) (nums-add v (nums-scale accel dt t1) v))
						;damped toward the velocity of what it is in, still or a flow,
						;its own or the mover's of where it is
						(when (> (defq drag (elem-get drags id)) +double_0)
							(defq keep (max +double_0 (- +double_1 (* drag dt))))
							(cond
								((defq flow (ifn (elem-get flows id) (if flow_fn (flow_fn at))))
									(nums-sub v flow t1)
									(nums-add flow (nums-scale t1 keep t1) v))
								((nums-scale v keep v))))
						;no faster than its top
						(and (> (defq top (elem-get tops id)) +double_0) (> (defq speed (sqrt (nums-dot v v))) top)
							(nums-scale v (/ top speed) v))
						(nums-add at (nums-scale v dt t1) at))
					;as much of its shove as eases in
					(nums-scale s (min +double_1 (* dt (elem-get rates id))) t2)
					(nums-add at t2 at)
					(nums-sub s t2 s)
					;kept in the box
					(when wrap
						(bind '(x y z) at)
						(bind '(wx wy wz) wrap)
						(cond ((< x +double_0) (elem-set at 0 (+ x wx))) ((>= x wx) (elem-set at 0 (- x wx))))
						(cond ((< y +double_0) (elem-set at 1 (+ y wy))) ((>= y wy) (elem-set at 1 (- y wy))))
						(cond ((< z +double_0) (elem-set at 2 (+ z wz))) ((>= z wz) (elem-set at 2 (- z wz)))))
					(when (defq spin (elem-get spins id))
						(bind '(wx wy wz qw qx qy qz) spin)
						(defq nw (- qw (* h (+ (* wx qx) (* wy qy) (* wz qz))))
							nx (+ qx (* h (+ (* wx qw) (- (* wy qz) (* wz qy)))))
							ny (+ qy (* h (+ (* wy qw) (- (* wz qx) (* wx qz)))))
							nz (+ qz (* h (+ (* wz qw) (- (* wx qy) (* wy qx)))))
							n (/ +double_1 (sqrt (+ (* nw nw) (* nx nx) (* ny ny) (* nz nz)))))
						(elem-set spin 3 (* nw n))
						(elem-set spin 4 (* nx n))
						(elem-set spin 5 (* ny n))
						(elem-set spin 6 (* nz n)))))
			places vels shoves live kin)
		this)
	)

;module
(export-classes '(Mover))
(export-symbols '(move-add-to))
(env-pop)
