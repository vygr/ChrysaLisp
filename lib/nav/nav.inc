;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Nav, a thing that goes somewhere: told where it is and which way it
; faces, where to go and by where, it finds its way and keeps to it,
; turning as a thing of weight does, a spring damped as much as asked,
; and going as fast as it should. What is its own to each use is a
; function it is given, a property, so the same Nav is a player, a fish, a
; tour guide or a submarine in a tunnel:
;
; :route_fn (nav from to via) -> :nil | path, the places from here to
; there, :nil if the answer comes later, (. nav :routed path)
; :refine_fn (nav path) -> :nil | path, the path made fit, a tunnel's
; middle, or :nil, the answer later as above
; :next_fn (nav) -> :nil | place, where to go on getting there
; :aim_fn (nav dt) -> :nil | place, what to steer at this step
; :avoid_fn (nav aim) -> place, the aim bent round what is in the way
; :speed_fn (nav dt) -> double, how fast it would go
; :up_fn (nav way dt) -> way, which way it would have its up
; :move_fn (nav to dt) -> place, the last say on where it goes
; :arrive_fn (nav) and :stuck_fn (nav), told it
;
; The plugins here, nav-..., are the common ones; each takes what it
; needs of the Nav's properties. A plugin takes the Nav, so it runs on
; any task that imports it: a Nav's state is data, (:state), and goes in a
; message as it is, its functions do not, a child has its own
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/quaternion.inc")
(import "./graph.inc")
(import "./path.inc")

;module
(env-push)

(defq +nav_y (doubles +double_0 +double_1 +double_0) +nav_back (doubles +double_0 +double_0 +double_-1)
	+nav_none (doubles +double_0 +double_0 +double_0) +nav_tiny (/ +double_1 (n2d 1000000))
	+nav_dt_most +double_1/10 +nav_rnd_of (n2d 1000000) +nav_most_of (/ (n2d 99) (n2d 100))
	;where on the way to its aim a field is looked at
	+nav_looks (doubles (n2d 0.35) (n2d 0.7) +double_1))

(defun nav-spring (x v to dt k zeta)
	; (nav-spring x v to dt k zeta) -> (x v)
	;x moved toward to by a spring of stiffness k, damped by zeta: 1 just
	;enough never to go past it, more slower to it, less swinging past; v
	;how fast it moves. A step of no more than a tenth of a second
	(defq dt (min dt +nav_dt_most) v (+ v (* (- (* k k (- to x)) (* +double_2 zeta k v)) dt)))
	(list (+ x (* v dt)) v))

(defun nav-cross (a b)
	; (nav-cross a b) -> doubles
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))))

(defun nav-far (a b)
	; (nav-far a b) -> double
	(sqrt (nums-dot (defq d (nums-sub b a)) d)))

(defun nav-rnd ()
	; (nav-rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) +nav_rnd_of))

;;;;;;;;;;;;;;;;;;;;;;;;
; routes, and on arrival
;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav-route-line (nav from to via)
	; (nav-route-line nav from to via) -> path
	;straight, through the places by the way, its places :step apart
	(nav-path-resample (cat (list from) via (list to)) (get :step nav)))

(defun nav-route-graph (nav from to via)
	; (nav-route-graph nav from to via) -> path
	;by the links of the Nav's :graph, the route of least cost, :cost_fn as
	;graph-route has it, from the node nearest here to the one nearest
	;there through those nearest the places by the way, its places :step
	;apart and smoothed :smooth times; just here, if there is none, so it
	;is there at once and goes on, :next_fn
	(defq g (get :graph nav) cost_fn (get :cost_fn nav)
		stops (map (# (graph-nearest g %0)) (cat (list from) via (list to)))
		nodes (list (first stops)))
	(each (lambda (a b)
			(when nodes
				(if (defq r (graph-route g a b cost_fn)) (setq nodes (cat nodes (rest r))) (setq nodes :nil))))
		(slice stops 0 -2) (slice stops 1 -1))
	(if nodes
		(nav-path-smooth (nav-path-resample (cat (list from) (graph-places g nodes) (list to)) (get :step nav)) (get :smooth nav))
		(list (cat from))))

(defun nav-next-tour (nav)
	; (nav-next-tour nav) -> place
	;the next of the places of its :tour, round and round
	(defq tour (get :tour nav) i (% (inc (get :tour_at nav)) (length tour)))
	(def nav :tour_at i)
	(elem-get tour i))

(defun nav-next-wander (nav)
	; (nav-next-wander nav) -> :nil | place
	;somewhere within :wander of its :home, level with it, one that
	;(:wander_ok nav place) says will do, of a few tried
	(defq home (get :home nav) far (get :wander nav) ok (get :wander_ok nav) out :nil)
	(times 8
		(unless out
			(defq p (nums-add home (doubles (* (- (nav-rnd) +double_1/2) +double_2 far) +double_0
				(* (- (nav-rnd) +double_1/2) +double_2 far))))
			(if (or (not ok) (ok nav p)) (setq out p))))
	out)

;;;;;;;;;;;;;;;;;;;;;;;;;
; what to steer at, and round
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav-aim-follow (nav dt)
	; (nav-aim-follow nav dt) -> :nil | place
	;along its path, from where it is on it, or will be in :predict times
	;the time its turn takes, the nearest of the next :look places and how
	;far past it, a place as far on as it goes in :lead
	;seconds, no nearer than :lead_least, sliding on as it goes, never
	;jumping a place at a time; with no path yet, its goal; with none, on
	;as it is
	(cond
		((defq pts (get :path nav))
			;from where it will be when its turn has caught up, the way from
			;there to the aim, put where it is: so it does not swing across its
			;path, its turn behind its aim
			(defq at (get :at nav) from (nums-add at (nums-scale (get :way nav) (* (get :speed nav) (nav-settle nav))))
				cursor (nav-path-on pts from (get :cursor nav) (get :look nav)))
			(def nav :cursor cursor)
			(defq lead (max (get :lead_least nav) (* (get :speed nav) (get :lead nav)))
				aim (nav-path-ahead pts cursor (+ (nav-path-past pts cursor from) lead)))
			;near the end, the end itself
			(if (< (nav-far from aim) (* lead +nav_most_of)) aim (nums-add at (nums-sub aim from))))
		((get :goal nav))))

(defun nav-settle (nav)
	; (nav-settle nav) -> double
	;the seconds it looks ahead to where it will be, :predict times how long
	;its turn takes to catch up, 1/:k of a spring, 1/:rate of a lerp
	(/ (get :predict nav) (if (eql (get :steer nav) :lerp) (get :rate nav) (get :k nav))))

(defun nav-aim-seek (nav dt)
	; (nav-aim-seek nav dt) -> :nil | place
	;straight at its goal, no path
	(get :goal nav))

(defun nav-stick (v dead bend)
	; (nav-stick v dead bend) -> double
	;how hard a stick, -1 to 1, pushes: nothing in its dead middle, and past
	;it to the power bend, its sign kept, 1 straight, 2 gentle near the
	;middle and full at the edge
	(defq a (max +double_0 (/ (- (abs v) dead) (- +double_1 dead))))
	(if (< v +double_0) (neg (pow a bend)) (pow a bend)))

(defun nav-aim-pointer (nav dt)
	; (nav-aim-pointer nav dt) -> place
	;a player's: ahead, and across and up by its :stick, (x y) -1 to 1, as
	;far as :stick_reach, past its :stick_dead middle and bent by
	;:stick_bend, nav-stick; :nil the stick let go
	(defq way (get :way nav) at (nums-add (get :at nav) way))
	(if (defq stick (get :stick nav))
		(defq dead (get :stick_dead nav) bend (get :stick_bend nav) reach (get :stick_reach nav)
			at (nums-add at (nums-add (nums-scale (nav-cross way (get :up nav)) (* (nav-stick (first stick) dead bend) reach))
				(nums-scale (get :up nav) (* (nav-stick (second stick) dead bend) reach))))))
	at)

(defun nav-field-slope (field p e)
	; (nav-field-slope field p e) -> doubles
	;the way a field grows at a place, of length 1
	(defq h (field p))
	(nav-path-unit (doubles (- (field (nums-add p (doubles e +double_0 +double_0))) h)
		(- (field (nums-add p (doubles +double_0 e +double_0))) h)
		(- (field (nums-add p (doubles +double_0 +double_0 e))) h))))

(defun nav-field-clear (field p clear_by e)
	; (nav-field-clear field p clear e) -> (p moved)
	;a place moved up the slope of a field, (field p) how clear of what is in
	;the way it is, till it is clear, a few steps; and was it moved
	(defq moved :nil)
	(times 6
		(when (< (defq h (field p)) clear_by)
			(setq p (nums-add p (nums-scale (nav-field-slope field p e) (- clear_by h))) moved :t)))
	(list p moved))

(defun nav-avoid-field (nav aim)
	; (nav-avoid-field nav aim) -> place
	;on the way to the aim, the first of a few places along it not :clear of
	;what is in the way, :field_fn of a place how clear it is, moved clear,
	;the aim, so it steers round or over it
	(defq at (get :at nav) to (nums-sub aim at) field (get :field_fn nav) clear_by (get :clear nav) e (get :field_step nav))
	(ifn (some (lambda (along)
				(bind '(p moved) (nav-field-clear field (nums-add at (nums-scale to along)) clear_by e))
				(if moved p))
			+nav_looks)
		aim))

(defun nav-side (want way)
	; (nav-side want way) -> way
	;which side of want a way is, across it, of length 1
	(nav-path-unit (nums-sub way (nums-scale want (nums-dot way want)))))

(defun nav-avoid-cone (nav aim)
	; (nav-avoid-cone nav aim) -> place
	;the way to the aim, or of ways round it, as far as :cone radians, in
	;:cone_rings rings of :cone_count, the one that (:clear_fn nav way) says
	;clears what is in the way by :clear, turning the least from the aim
	;and the most to the side it went round last time, :cone_keep of that,
	;so it keeps to one side, not sliding round nor flipping from side to
	;side; with none that do, the one that clears it the most, less as
	;much again for its side
	(defq at (get :at nav) to (nums-sub aim at) far (sqrt (nums-dot to to)) want (nav-path-unit to)
		clear_fn (get :clear_fn nav) clear_by (get :clear nav))
	(cond
		((>= (clear_fn nav want) clear_by) (def nav :cone_side :nil) aim)
		(:t (defq across (nav-path-unit (nav-cross want (get :up nav))) up (nav-cross across want)
				last_side (get :cone_side nav) keep (get :cone_keep nav)
				best :nil best_score :nil top :nil most_way want rings (get :cone_rings nav) count (get :cone_count nav))
			(each! (lambda ()
					(defq off (* (get :cone nav) (/ (n2d (inc (!))) (n2d rings))))
					(each! (lambda ()
							(defq a (/ (* +double_2pi (n2d (!))) (n2d count))
								way (nav-path-unit (nums-add want (nums-add (nums-scale across (* off (cos a)))
									(nums-scale up (* off (sin a))))))
								c (clear_fn nav way))
							(defq turned (if last_side (* keep (- +double_1 (nums-dot (nav-side want way) last_side))) +double_0))
							(if (or (not top) (> (- c turned) top)) (setq top (- c turned) most_way way))
							(when (>= c clear_by)
								(defq score (+ off turned))
								(if (or (not best_score) (< score best_score)) (setq best way best_score score))))
						'() 0 count))
				'() 0 rings)
			(def nav :cone_side (nav-side want (setq best (ifn best most_way))))
			(nums-add at (nums-scale best far)))))

;;;;;;;;;;;;;;;;;;;;;
; speed, up and move
;;;;;;;;;;;;;;;;;;;;;

(defun nav-speed-cruise (nav dt)
	; (nav-speed-cruise nav dt) -> double
	;its :cruise; with :stop and nothing next, slower as it nears its goal
	;within :slow_from, and stopped there
	(defq cruise (get :cruise nav) goal (get :goal nav))
	(cond
		((not (and (get :stop nav) (not (get :next_fn nav)))) cruise)
		(goal (* cruise (min +double_1 (/ (nav-far (get :at nav) goal) (get :slow_from nav)))))
		((get :arrived nav) +double_0)
		(cruise)))

(defun nav-up-free (nav way dt)
	; (nav-up-free nav way dt) -> way
	;its own up, kept, no roll of its own: in space
	(get :up nav))

(defun nav-up-level (nav way dt)
	; (nav-up-level nav way dt) -> way
	;the world's up, :world_up, its up; but going near straight up or down,
	;where that gives no roll, its own up kept, the more the nearer: from
	;:upright_from across, of 1 level, fully over :upright_over more
	(defq world_up (get :world_up nav) along (nums-dot way world_up)
		across (sqrt (max +double_0 (- +double_1 (* along along))))
		world (max +double_0 (min +double_1 (/ (- across (get :upright_from nav)) (get :upright_over nav)))))
	(nav-path-unit (nums-add (nums-scale (get :up nav) (- +double_1 world)) (nums-scale world_up world))))

(defun nav-up-lean (nav way dt)
	; (nav-up-lean nav way dt) -> way
	;level, nav-up-level, and leant into a turn, as a plane, a bird or a
	;ship does: by :lean times how far across it is turning, no more than
	;:lean_most
	(defq level (nav-up-level nav way dt) across (nav-cross way level)
		lean (max (neg (get :lean_most nav)) (min (get :lean_most nav)
			(* (get :lean nav) (nums-dot way (nav-cross (get :way nav) (get :up nav)))))))
	(nav-path-unit (nums-add (nums-scale level (cos lean)) (nums-scale across (sin lean)))))

(defun nav-move-field (nav to dt)
	; (nav-move-field nav to dt) -> place
	;kept out of what is in the way, :field_fn, by its :body
	(first (nav-field-clear (get :field_fn nav) to (get :body nav) (get :field_step nav))))

;;;;;;;;;
; turning
;;;;;;;;;

(defun nav-turn-to (nav want dt)
	; (nav-turn-to nav want dt)
	;the Nav's turn, its way and up, turned toward the turn want, as its
	;:steer says: :spring, how fast it turns sprung toward the turn still to
	;go, stiffness :k, damping :zeta; :lerp, a share of the way there each
	;step, the more the longer, :rate a second. No faster than :turn_most
	(defq now (get :turn nav) top (get :turn_most nav) dt (min dt +nav_dt_most))
	(defq turn (cond
		((eql (get :steer nav) :lerp)
			(quat-toward now want (min (* top dt)
				(* (- +double_1 (exp (neg (* dt (get :rate nav))))) (quat-angle now want)))))
		(:t ;the turn still to go, as a way about which and how far
			(defq apart (quat-mul want (quat-conj now)) k (get :k nav))
			(if (< (first apart) +double_0) (setq apart (nums-scale apart +double_-1)))
			(bind '(aw ax ay az) apart)
			(defq half (acos (min +double_1 aw)) s (sin half)
				go (if (> s +nav_tiny) (nums-scale (doubles ax ay az) (/ (* +double_2 half) s)) +nav_none)
				spin (get :spin nav)
				spin (nums-add spin (nums-scale (nums-sub (nums-scale go (* k k))
					(nums-scale spin (* +double_2 (get :zeta nav) k))) dt))
				rate (sqrt (nums-dot spin spin)))
			(if (> rate top) (setq spin (nums-scale spin (/ top rate)) rate top))
			(def nav :spin spin)
			(if (> rate +nav_tiny) (quat-mul (quat-axis-angle (nums-scale spin (recip rate)) (* rate dt)) now) now))))
	(setq turn (quat-unit turn))
	(def nav :turn turn :way (quat-vec3-rot turn +nav_back) :up (quat-vec3-rot turn +nav_y)))

(defun nav-turn-pointer (nav dt)
	; (nav-turn-pointer nav dt)
	;a player's turn, toward its pointer's aim, nav-aim-pointer: a share of
	;the way there a step, :point_rate a second, no faster than :point_most,
	;so it stops as the stick is let go and never swings past; its up level
	;and leant into the turn, nav-up-lean. Its spring's spin let go, so the
	;auto pilot's turn starts from rest
	(defq way (nav-path-unit (nums-sub (nav-aim-pointer nav dt) (get :at nav)))
		now (get :turn nav) want (quat-frame way (nav-up-lean nav way dt)) dt (min dt +nav_dt_most)
		turn (quat-unit (quat-toward now want (min (* (get :point_most nav) dt)
			(* (- +double_1 (exp (neg (* dt (get :point_rate nav))))) (quat-angle now want))))))
	(def nav :turn turn :spin +nav_none :way (quat-vec3-rot turn +nav_back) :up (quat-vec3-rot turn +nav_y)))

;;;;;
; Nav
;;;;;

(defclass Nav (at way &optional up) :nil
	; (Nav at way [up]) -> nav
	;at a place, facing a way, its up the world's if not said
	(defq up (nav-path-unit (ifn up +nav_y)) way (nav-path-unit way)
		up (nav-path-unit (nav-cross (nav-cross way up) way)))
	(def this :at (cat at) :way way :up up :turn (quat-frame way up) :spin +nav_none
		:speed +double_0 :speed_v +double_0
		;where it is going, by where, its path and how far along it, waiting
		;on a route, got there
		:goal :nil :via (list) :path :nil :cursor 0 :pending :nil :arrived :nil
		;how fast it goes, the stiffness of that, and slowing to a stop
		:cruise +double_1 :speed_k +double_2 :stop :nil :slow_from +double_4
		;how it turns, its spring, its share a second, as fast as it may, and
		;how many times the time its turn takes it aims from ahead
		:steer :spring :k +double_1 :zeta +double_1 :rate +double_1 :turn_most +double_pi :predict (n2d 1.5)
		;on a path, how many places on it looks for where it is, and how far
		;ahead it aims, seconds and at least; places of a path how far apart,
		;how often smoothed; how near is there
		:look 8 :lead +double_1 :lead_least +double_1 :step +double_1 :smooth 2 :near +double_1
		;up: the world's, from how far across it is kept, over how much more;
		;a lean into a turn, the most
		:world_up +nav_y :upright_from (n2d 0.2) :upright_over (n2d 0.3) :lean +double_1 :lean_most +double_1
		;a field, how clear of it to keep its aim and its body, the step of
		;its slope; a cone of ways, how wide, rings and ways a ring, how much
		;to keep to the side it went round last, and that side
		:field_fn :nil :clear +double_2 :body +double_1 :field_step +double_1/2
		:clear_fn :nil :cone +double_2 :cone_rings 4 :cone_count 8 :cone_keep +double_2 :cone_side :nil
		;a tour, a wander, a pointer's stick, how far it reaches, its dead
		;middle and its bend, and the share a second a player's turn goes, and
		;how fast at most
		:tour (list) :tour_at -1 :home (cat at) :wander (n2d 20) :wander_ok :nil :stick :nil :stick_reach +double_1
		:stick_dead +double_0 :stick_bend +double_1 :point_rate +double_4 :point_most +double_1
		;stuck: gone less in so many seconds than so much of its cruise
		:stuck_secs +double_2 :stuck_least +double_1/4 :stuck_at (cat at) :stuck_for +double_0
		;the plugins
		:route_fn nav-route-line :refine_fn :nil :next_fn :nil :aim_fn nav-aim-follow :avoid_fn :nil
		:speed_fn nav-speed-cruise :up_fn nav-up-level :move_fn :nil :arrive_fn :nil :stuck_fn :nil)

	(defmethod :go (to &optional via)
		; (. nav :go to [via]) -> nav
		;off to a place, by the places via, its route asked for now
		(def this :goal (cat to) :via (ifn via (list)) :arrived :nil :path :nil :cursor 0 :pending :t)
		(if (defq pts ((get :route_fn this) this (get :at this) to (get :via this)))
			(. this :refine pts))
		this)

	(defmethod :refine (pts)
		; (. nav :refine path) -> nav
		;a route made fit, by its :refine_fn, and followed, or waited on
		(if (defq fn (get :refine_fn this)) (setq pts (fn this pts)))
		(if pts (. this :routed pts))
		this)

	(defmethod :routed (pts &optional more)
		; (. nav :routed path [more]) -> nav
		;the path it follows, or with more, added to the end of the one it
		;has, what it has been along trimmed
		(cond
			((and more (defq was (get :path this)))
				(defq cut (max 0 (- (get :cursor this) 2)))
				(def this :path (cat (slice was cut -1) pts) :cursor (- (get :cursor this) cut)))
			((def this :path pts :cursor 0)))
		(def this :pending :nil)
		this)

	(defmethod :step (dt)
		; (. nav :step dt) -> nav
		;a step of dt seconds: steered at its aim, round what is in the way,
		;turned toward that, upright as it would be, its speed sprung to what
		;it would go, and on; there, or stuck, told so
		(defq at (get :at this) aim ((get :aim_fn this) this dt))
		(and aim (defq fn (get :avoid_fn this)) (setq aim (fn this aim)))
		(defq way (if (and aim (> (nav-far at aim) +nav_tiny)) (nav-path-unit (nums-sub aim at)) (get :way this)))
		(nav-turn-to this (quat-frame way ((get :up_fn this) this way dt)) dt)
		(bind '(speed speed_v) (nav-spring (get :speed this) (get :speed_v this) ((get :speed_fn this) this dt)
			dt (get :speed_k this) +double_1))
		(defq to (nums-add at (nums-scale (get :way this) (* speed dt))))
		(if (defq fn (get :move_fn this)) (setq to (fn this to dt)))
		(def this :at to :speed speed :speed_v speed_v)
		(. this :check aim dt))

	(defmethod :check (aim dt)
		; (. nav :check aim dt) -> nav
		;got there: near the end of its path, or its goal with no path, not
		;waiting on a route; then on to the next, :next_fn, or stopped there.
		;Stuck: aiming somewhere, but gone less in :stuck_secs than
		;:stuck_least of how far its :cruise goes
		(defq at (get :at this) goal (get :goal this) pts (get :path this))
		(when (and goal (not (get :pending this))
				(if pts (and (>= (get :cursor this) (- (length pts) 2)) (< (nav-far at (last pts)) (get :near this)))
					(< (nav-far at goal) (get :near this))))
			(def this :arrived :t)
			(if (defq fn (get :arrive_fn this)) (fn this))
			(cond
				((and (defq fn (get :next_fn this)) (defq next_to (fn this))) (. this :go next_to))
				((def this :goal :nil :path :nil))))
		(def this :stuck_for (+ (get :stuck_for this) dt))
		(when (>= (get :stuck_for this) (get :stuck_secs this))
			(and aim (defq fn (get :stuck_fn this))
				(< (nav-far (get :stuck_at this) at) (* (get :stuck_least this) (get :cruise this) (get :stuck_for this)))
				(fn this))
			(def this :stuck_at (cat at) :stuck_for +double_0))
		this)

	(defmethod :right ()
		; (. nav :right) -> way
		(nav-cross (get :way this) (get :up this)))

	(defmethod :matrix ()
		; (. nav :matrix) -> mat4x4
		;its turn as a matrix, of a model's frame
		(quat-mat4x4 (get :turn this)))

	(defmethod :state (&optional state)
		; (. nav :state [state]) -> state | nav
		;what it is and is doing, data to keep or send; or set again from it
		(cond
			(state
				(bind '(at turn spin speed speed_v goal path cursor) state)
				(def this :at at :turn turn :spin spin :speed speed :speed_v speed_v :goal goal :path path :cursor cursor
					:way (quat-vec3-rot turn +nav_back) :up (quat-vec3-rot turn +nav_y))
				this)
			((list (get :at this) (get :turn this) (get :spin this) (get :speed this) (get :speed_v this)
				(get :goal this) (get :path this) (get :cursor this))))))

;module
(export-classes '(Nav))
(export-symbols
	'(nav-spring nav-cross nav-far nav-rnd nav-turn-to
	nav-settle nav-route-line nav-route-graph nav-next-tour nav-next-wander
	nav-aim-follow nav-aim-seek nav-stick nav-aim-pointer nav-turn-pointer
	nav-field-slope nav-field-clear nav-avoid-field nav-side nav-avoid-cone
	nav-speed-cruise nav-up-free nav-up-level nav-up-lean nav-move-field))
(env-pop)
