;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's visitor: now and then one of Flight's craft, its saucer or its
; triangle, comes out of the dark from a side, above or below, curious to
; see what the ship is doing. It keeps station by it a while, its nose on
; the ship, then tilts its underside to the biggest rock in the ship's
; sight and plays its beam over it, prospecting, as it samples over water
; in Flight, takes nothing and, bored, leaves, away from the ship. A
; missile on it throws debris off it, its lights flash red and it bolts;
; it can not be killed.
;
; Its parts are Flight's, made by Space's farm, Flight's craft.inc, its
; vertex shader Flight's and its pixel shader Space's own, the metal of
; a buoy, visitor_lit.shader; its beam Flight's tube, with Space's
; beam_vertex.shader and beam_lit.shader.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/scene.inc")
(import "apps/demos/space/sky.inc")
(import "apps/demos/flight/craft.inc")
(import "apps/demos/space/field.inc")

;module
(env-push)

;when it first comes, and after, how long apart, microseconds, at least
;and more; how far off it comes from, and goes to; where it keeps station
;by the ship, ahead, how far out to the frame's edge, in from it by its
;sizes, and up a little from a side; how long it keeps it, at least and
;more; how fast it comes and how fast it goes, a second, how fast its way
;eases to where it makes for; how far its beam reaches, its least length,
;how thick a ray it is looked along for a rock in the way, and how big one
;is to be in the way; how far to the sides and up and down a rock is to be
;in the frame, at a distance of 1, how much less one at its edge counts
;than one in the middle, how long it plays its beam over it, how wide its
;beam is of its size and how bright; how square its underside is to the
;rock, the cos, for its beam to come on and to stay on; how far out to its
;side it leaves; how long it is startled when hit, how long it flashes red
;after, how fast it bolts, and how near the place it keeps it is when it
;has come, or how long it comes at most, should a rock be there
(defq +visit_first_least 25000000 +visit_first_more 15000000 +visit_least 150000000 +visit_more 150000000
	+visit_from (n2d 520) +visit_gone (n2d 640) +pace_ahead (n2d 60) +pace_out (n2d 0.9) +pace_in (n2d 1.3) +pace_up (n2d 4)
	+pace_least 2500000 +pace_more 2000000 +come_speed (n2d 90) +leave_speed (n2d 240)
	+visit_ease (n2d 1.2) +probe_reach (n2d 600) +beam_least (n2d 20) +beam_ray +double_1 +block_least (n2d 10) +probe_wide (n2d 0.55) +probe_high (n2d 0.42) +probe_middle (n2d 0.7) +probe_long 6000000
	+visit_beam_wide (n2d 0.42) +beam_bright +double_1 +beam_facing (n2d 0.97) +beam_held (n2d 0.8)
	+leave_aside (n2d 0.8) +track_ease (n2d 8) +startle_long 2000000
	+alarm_long 2500000 +bolt_speed (n2d 380) +near_enough (n2d 6) +come_long 8000000
	;the parts it is made of, of Flight's: the saucer, the ring, the lamp
	;ball and the triangle; and the scout's and the triangle's numbers
	visitor_parts (list +craft_saucer +craft_ring +craft_ball +craft_delta)
	+scout_spin (n2d 0.6) +scout_film +double_1/2 +scout_panels +double_4 +scout_ring_film (n2d 0.9)
	+scout_ring_panels +double_6 +scout_ring_spin (n2d 1.5) +scout_ring_scale (n2d 1.25)
	+scout_lamp_scale (n2d 0.09) +scout_lamp_drop (n2d -0.12) +scout_lamp_out (n2d 0.6)
	+tri_film +double_1 +tri_panels +double_7 +tri_ring_film (n2d 0.6) +tri_ring_spin (n2d -2.5)
	+tri_ring_scale (n2d 0.32) +tri_ring_height +double_1/2 +tri_ring_drop (n2d -0.06) +tri_lamp_scale (n2d 0.08)
	+tri_lamp_rise (n2d 0.02) +tri_nose (n2d 0.8) +tri_wing_x (n2d 0.7) +tri_wing_z (n2d -0.4)
	+tube_flare (n2d 1.15) +triangle_size (n2d 11)
	;where it comes from, of how far off, ahead and aside, and how much of
	;its speed it moves it at least to turn it
	+come_from (n2d 0.7) +turn_least +double_1/10
	;how near straight up or down its beam goes before it is turned about
	;another way across
	+beam_steep (n2d 0.9)
	;how much its up is tipped toward the eye
	+tip_to_eye (n2d 0.45)
	;how far out it reaches, of its size, kept out of the rocks
	+visitor_reach (n2d 0.6)
	;how fast its up turns to what it wants, a second, and how far down its
	;beam starts, of its size
	+up_ease +double_2 +belly (n2d 0.15)
	;the visitor now: the craft, :nil away; what it is doing, when it comes
	;next, where it is and its way, how long it does what it does, the rock
	;it looks at, how bright its beam, till when it flashes red, the side it
	;keeps station on, where it made for last, its up, whether it came from
	;a side or above or below, and the size of its rock when it chose it
	visitor :nil visit_phase :away visit_next 0 visit_at :nil visit_way :nil visit_until 0 visit_rock :nil
	visit_beam +double_0 visit_alarm 0 visit_side +double_1 visit_goal :nil visit_up :nil visit_across :t visit_bulk +double_0)

(defun rgb (r g b)
	; (rgb r g b) -> doubles
	(doubles (n2d r) (n2d g) (n2d b)))

(defun part-node (part tint glow film &optional panels lamps)
	; (part-node part tint glow film [panels lamps]) -> scene_node
	;a part of the craft, as Flight's are
	(def (defq node (Scene-node)) :part part :tint tint :glow glow :film film
		:panels (ifn panels +double_0) :lamps (ifn lamps (rgb 0 0 0)))
	node)

(defun make-scout ()
	; (make-scout) -> scene_node
	;Flight's scout: a saucer, a ring that turns round it, and four lamps
	;under it, the whole of it turning slowly
	(def (defq scout (Scene-node "scout")) :size +double_8 :spin +scout_spin)
	(defq dark (rgb 0 0 0))
	(. scout :add_node (part-node +craft_saucer (rgb 0.55 0.58 0.62) dark +scout_film
		+scout_panels (rgb 1.0 0.55 0.2)))
	(def (defq ring (part-node +craft_ring (rgb 0.3 0.32 0.36) (rgb 0.05 0.15 0.2) +scout_ring_film
		+scout_ring_panels (rgb 1.0 0.55 0.2))) :spin +scout_ring_spin)
	(. scout :add_node (. ring :set_scale +scout_ring_scale +scout_ring_scale +scout_ring_scale))
	(each (lambda ((x z))
			(. scout :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.45 1.0 0.8) +double_0)
				(:set_scale +scout_lamp_scale +scout_lamp_scale +scout_lamp_scale)
				(:set_translation x +scout_lamp_drop z))))
		(list (list +scout_lamp_out +double_0) (list (neg +scout_lamp_out) +double_0)
			(list +double_0 +scout_lamp_out) (list +double_0 (neg +scout_lamp_out))))
	scout)

(defun make-triangle ()
	; (make-triangle) -> scene_node
	;Flight's triangle: gunmetal, its nose ahead, a ring of light turning
	;under its middle and a ball of light at each corner
	(def (defq tri (Scene-node "triangle")) :size +triangle_size)
	(. tri :add_node (part-node +craft_delta (rgb 0.34 0.36 0.4) (rgb 0 0 0) +tri_film
		+tri_panels (rgb 0.3 0.9 1.4)))
	(def (defq ring (part-node +craft_ring (rgb 0.1 0.1 0.12) (rgb 0.2 0.9 1.3) +tri_ring_film))
		:spin +tri_ring_spin)
	(. tri :add_node (.-> ring (:set_scale +tri_ring_scale +tri_ring_height +tri_ring_scale)
		(:set_translation +double_0 +tri_ring_drop +double_0)))
	(each (lambda ((x z))
			(. tri :add_node (.-> (part-node +craft_ball (rgb 0.3 0.3 0.3) (rgb 1.8 1.8 1.6) +double_0)
				(:set_scale +tri_lamp_scale +tri_lamp_scale +tri_lamp_scale)
				(:set_translation x +tri_lamp_rise z))))
		(list (list +double_0 +tri_nose) (list +tri_wing_x +tri_wing_z) (list (neg +tri_wing_x) +tri_wing_z)))
	tri)

(defun tube-verts (segs)
	; (tube-verts segs) -> doubles
	;Flight's beam: an open tube of segs sides, its top of radius 1 at 0 and
	;its foot a little wider at -1
	(defq out (list) flare +tube_flare)
	(each! (lambda ()
			(defq a (/ (* +double_2pi (n2d (!))) (n2d segs)) b (/ (* +double_2pi (n2d (inc (!)))) (n2d segs))
				ca (cos a) sa (sin a) cb (cos b) sb (sin b))
			(push out ca +double_0 sa (* ca flare) +double_-1 (* sa flare) (* cb flare) +double_-1 (* sb flare)
				ca +double_0 sa (* cb flare) +double_-1 (* sb flare) cb +double_0 sb))
		'() 0 segs)
	(apply (const doubles) out))

(defun visit-time (least more)
	; (visit-time least more) -> num
	;so long from now, at random
	(+ (pii-time) least (random more)))

(defun visit-start (eye forward right up)
	; (visit-start eye forward right up)
	;a visitor out of the dark, far ahead and off to a side
	;one draw for the craft, its side and across or up, not a coin each: the
	;low bits of draws one after another go together
	(defq pick (/ (random 1000000) 125000))
	(setq visitor (if (bits? pick 1) (make-scout) (make-triangle))
		visit_side (if (bits? pick 2) +double_1 +double_-1) visit_across (bits? pick 4)
		visit_at (nums-add eye (nums-add (nums-scale forward (* +visit_from +come_from))
			(nums-scale (if visit_across right up) (* visit_side +visit_from +come_from))))
		visit_way (sky-unit (nums-sub eye visit_at)) visit_phase :come visit_beam +double_0 visit_alarm 0 visit_goal :nil
		visit_until (+ (pii-time) +come_long)
		visit_up (cat up)))

(defun visit-goal (eye forward right up spread_x spread_y)
	; (visit-goal eye forward right up spread_x spread_y) -> doubles
	;where it makes for: alongside the ship pacing it, beside a rock looking
	;at it, or away
	(cond
		((find visit_phase '(:come :pace :probe :startled))
			;ahead of the ship on the side it came in from, keeping station near
			;the edge of the frame, of its size at that distance, all of it in,
			;out of the way of its beam across it
			(defq in (* (get :size visitor) +pace_in))
			(nums-add eye (nums-add (nums-scale forward +pace_ahead)
				(if visit_across
					(nums-add (nums-scale right (* visit_side (- (* +pace_ahead spread_x +pace_out) in)))
						(nums-scale up +pace_up))
					(nums-scale up (* visit_side (- (* +pace_ahead spread_y +pace_out) in)))))))
		((nums-add visit_at (nums-scale visit_way +double_10)))))

(defun visit-rock-at (eye &optional i)
	; (visit-rock-at eye [i]) -> doubles
	;where the rock it looks at, or rock i, is seen, as it is drawn: its
	;place nearest the eye, as the box goes on every way
	(bind '((x y z) &ignore) (elem-get rocks (ifn i visit_rock)))
	(bind '(ex ey ez) eye)
	(doubles (+ ex (near-way (- x ex))) (+ ey (near-way (- y ey))) (+ ez (near-way (- z ez)))))

(defun visit-blocker (i eye world)
	; (visit-blocker i eye world) -> :nil | num
	;the big rock in the way of a beam from it to rock i, :nil if none: the
	;rocks a ray to the rock meets, of the rocks' world, their bodies' owners,
	;from out past its own size, so not a rock it sits beside, on past each
	;small one, till rock i or a big one
	(defq d (nums-sub (visit-rock-at eye i) visit_at) far (sqrt (nums-dot d d)) at (get :size visitor)
		way (nums-scale d (/ +double_1 far)) owners (get :owners world) rock i)
	(while (and (< at far) (defq hit (. world :cast (nums-add visit_at (nums-scale way at)) way (- far at) +beam_ray))
			(/= (setq rock (elem-get owners (first hit))) i)
			(< (defq bulk (third (elem-get rocks rock))) +block_least))
		;past the small one, its width on
		(setq at (+ at (second hit) (* +double_2 bulk)) rock i))
	(if (/= rock i) rock))

(defun visit-rock (eye forward right up world)
	; (visit-rock eye forward right up world) -> :nil | num
	;the rock it looks at: the one that looks biggest to the ship, its size
	;over how far off it is, the more the nearer the middle of the view it
	;is, the ship's line of sight, of those in the frame, ahead and well
	;inside its sides, that its beam reaches, not so near it its beam would
	;hardly show, and with no rock between; where each is seen, nearest the
	;eye
	(defq ins (list))
	(bind '(ex ey ez) eye)
	(each (lambda (((x y z) & bulk kind &ignore))
			(defq seen (doubles (near-way (- x ex)) (near-way (- y ey)) (near-way (- z ez)))
				ahead (nums-dot seen forward) by (sqrt (nums-dot seen seen)))
			(and (> ahead +double_0)
				(< (abs (nums-dot seen right)) (* ahead +probe_wide))
				(< (abs (nums-dot seen up)) (* ahead +probe_high))
				(< (+ (* bulk (elem-get kind_reach kind)) +beam_least)
					(sqrt (nums-dot (defq d (nums-sub (nums-add eye seen) visit_at)) d)) +probe_reach)
				(push ins (list (* (/ bulk by) (- +double_1 (* +probe_middle
					(/ (+ (abs (nums-dot seen right)) (abs (nums-dot seen up))) ahead (+ +probe_wide +probe_high)))))
					(!)))))
		rocks)
	;the best first, the first the beam reaches clear
	(some (# (unless (visit-blocker (second %0) eye world) (second %0)))
		(sort ins (# (if (> (first %0) (first %1)) -1 1)))))

(defun visit-away (eye right up)
	; (visit-away eye right up) -> doubles
	;the way it leaves, away from the ship, and out to the side it came in
	;from, so it is seen to go and not just to shrink down the line of sight
	(sky-unit (nums-add (sky-unit (nums-sub visit_at eye))
		(nums-scale (if visit_across right up) (* visit_side +leave_aside)))))

(defun visitor-step (dt eye forward right up world spread_x spread_y)
	; (visitor-step dt eye forward right up world spread_x spread_y)
	;the visitor on, as it is doing: coming, pacing, going to a rock,
	;looking at it, leaving; or away, till it comes again. Never in a rock:
	;out of each it is in, of the rocks' world, lib/collide, so it slides
	;round them
	(defq now (pii-time))
	(cond
		((eql visit_phase :away)
			(cond
				((= visit_next 0) (setq visit_next (visit-time +visit_first_least +visit_first_more)))
				((> now visit_next) (visit-start eye forward right up))))
		(:t (defq goal (visit-goal eye forward right up spread_x spread_y) to (nums-sub goal visit_at) far (sqrt (nums-dot to to))
				speed (if (eql visit_phase :leave) (if (> visit_alarm now) +bolt_speed +leave_speed) +come_speed)
				;how fast it turns to what it looks at, quick once its beam is on
				ease (if (> visit_beam +double_1/2) +track_ease +up_ease))
			;on with what it makes for as that moves, the ship, and toward it,
			;eased as it nears, no faster than it goes; beaming a rock or
			;startled, still where it is, the ship no longer kept by, so it can
			;be turned to and hit; leaving, on its way, faster
			(cond
				((eql visit_phase :leave)
					(setq visit_at (nums-add visit_at (nums-scale visit_way (* speed dt))) visit_goal :nil))
				((find visit_phase '(:probe :startled))
					(setq visit_goal :nil))
				(:t (if visit_goal (setq visit_at (nums-add visit_at (nums-sub goal visit_goal))))
					(setq visit_goal goal to (nums-sub goal visit_at) far (sqrt (nums-dot to to)))
					(when (> far +double_0)
						(defq step (min (* far (min +double_1 (* dt +visit_ease))) (* speed dt)))
						(setq visit_at (nums-add visit_at (nums-scale to (/ step far))))
						(if (> step (* +come_speed dt +turn_least)) (setq visit_way (sky-unit to))))))
			;keeping station it turns its nose to the ship, curious; looking at a
			;rock, to the rock, and once on it, kept on it as it moves
			(when (find visit_phase '(:pace :probe :startled))
				(defq look (if (and visit_rock (eql visit_phase :probe))
						(nums-sub (visit-rock-at eye) visit_at) (nums-sub eye visit_at)))
				(setq visit_way (sky-unit (nums-add visit_way (nums-scale (nums-sub (sky-unit look) visit_way) (min +double_1 (* dt ease)))))))
			;where it is nearest the eye, as the box goes on every way, so the eye
			;going round it does not leave it behind
			(bind '(vx vy vz) visit_at)
			(bind '(ex ey ez) eye)
			(setq visit_at (doubles (+ ex (near-way (- vx ex))) (+ ey (near-way (- vy ey))) (+ ez (near-way (- vz ez)))))
			;out of the rocks it would be in, as far as it is in each
			(each (lambda ((id depth way &ignore))
					(setq visit_at (nums-sub visit_at (nums-scale way depth))))
				(. world :near visit_at (* (get :size visitor) +visitor_reach)))
			;its up eased toward what it wants: away from the rock it looks at, its
			;underside to it, else the ship's tipped toward the eye, its top seen
			(defq want_up (if (and visit_rock (eql visit_phase :probe))
					(sky-unit (nums-sub visit_at (visit-rock-at eye)))
					(sky-unit (nums-add up (nums-scale (sky-unit (nums-sub eye visit_at)) +tip_to_eye)))))
			(setq visit_up (sky-unit (nums-add visit_up (nums-scale (nums-sub want_up visit_up) (min +double_1 (* dt ease))))))
			;its beam eased on as it looks at a rock with its underside turned to
			;it, and held on unless it is turned well off; off at once else, as
			;it leaves or is hit
			(setq visit_beam (if (and (eql visit_phase :probe) (<= visit_alarm now)
					(> (nums-dot visit_up want_up) (if (> visit_beam +double_1/2) +beam_held +beam_facing)))
				(+ visit_beam (* (- +double_1 visit_beam) (min +double_1 (* dt +double_3))))
				+double_0))
			(case visit_phase
				(:come (when (or (< far +near_enough) (> now visit_until))
					(setq visit_phase :pace visit_until (visit-time +pace_least +pace_more))))
				(:pace (when (> now visit_until)
					(if (setq visit_rock (visit-rock eye forward right up world))
						(setq visit_phase :probe visit_until (+ now +probe_long) visit_bulk (third (elem-get rocks visit_rock)))
						(setq visit_phase :leave visit_way (visit-away eye right up)))))
				(:startled (when (> now visit_until)
					(setq visit_phase :leave visit_way (visit-away eye right up))))
				;a big rock come between, it looks at that one instead; done, or its
				;rock broken or gone, its size not as it was or it far off, its beam
				;off at once, and it leaves
				(:probe (when (defq rock (visit-blocker visit_rock eye world))
						(setq visit_rock rock visit_bulk (third (elem-get rocks rock))))
					(when (or (> now visit_until) (/= (third (elem-get rocks visit_rock)) visit_bulk)
						(> (sqrt (nums-dot (defq d (nums-sub (visit-rock-at eye) visit_at)) d)) +probe_reach))
					(setq visit_phase :leave visit_beam +double_0 visit_way (visit-away eye right up))))
				(:leave (when (> (sqrt (nums-dot (defq d (nums-sub visit_at eye)) d)) +visit_gone)
					(setq visitor :nil visit_phase :away visit_next (visit-time +visit_least +visit_more))))))))

(defun visitor-hit? (at)
	; (visitor-hit? at) -> :t | :nil
	;is a missile at a place on the visitor: if so, its lights red and its
	;beam off at once, startled, it stays a moment, its eye on the ship, then
	;away it bolts, away from the ship and out to its side. Hit as it goes,
	;its lights red again
	(when (and visitor (< (sqrt (nums-dot (defq d (nums-sub visit_at at)) d)) (get :size visitor)))
		(defq now (pii-time))
		(setq visit_alarm (+ now +startle_long +alarm_long))
		(unless (find visit_phase '(:startled :leave))
			(setq visit_phase :startled visit_until (+ now +startle_long)))
		:t))

(defun visitor-matrix ()
	; (visitor-matrix) -> mat4x4
	;the visitor where it is, its y its up, visitor-step, its z the way it
	;goes made square to that, its size
	(defq y visit_up x (sky-cross y visit_way))
	(if (< (nums-dot x x) +turn_least) (setq x (sky-cross y (doubles +double_1 +double_0 +double_0))))
	(defq x (sky-unit x) z (sky-cross x y) s (get :size visitor))
	(bind '(xx xy xz) (nums-scale x s))
	(bind '(yx yy yz) (nums-scale y s))
	(bind '(zx zy zz) (nums-scale z s))
	(bind '(px py pz) visit_at)
	(doubles xx yx zx px xy yy zy py xz yz zz pz +double_0 +double_0 +double_0 +double_1))

(defun visitor-draws (mesh_base had files pictures view eye up vvals pvals seconds)
	; (visitor-draws mesh_base had files pictures view eye up vvals pvals seconds) -> draws
	;a draw for each part of the visitor whose mesh has come, by the matrix
	;of it and of the parts it is in, the parts that spin spun; red, its
	;lights, while it is hit
	(defq out (list))
	(when visitor
		(defq mats (list (visitor-matrix)) red (> visit_alarm (pii-time)))
		(. visitor :walk_nodes
			(lambda (node &)
				(if (def? :spin node) (. node :set_rotation +double_0 (* seconds (get :spin node)) +double_0))
				(push mats (defq m (mat4x4-mul (last mats) (. node :get_matrix))))
				(and (def? :part node) (elem-get had (find (get :part node) visitor_parts))
					(push out (list (+ mesh_base (get :part node))
						(cat (list (list 'model m) (list 'lens view)) vvals)
						(cat pvals (list (list 'tint (get :tint node))
							(list 'glow (if (and red (> (nums-dot (get :glow node) (get :glow node)) +double_0))
								(rgb 2.0 0.1 0.08) (get :glow node)))
							(list 'film (get :film node)) (list 'panels (get :panels node))
							(list 'lamps (if red (rgb 2.0 0.1 0.08) (get :lamps node)))))
						pictures files)))
				:nil)
			(lambda (&ignore) (pop mats))))
	out)

(defun visitor-beam (mesh view eye files seconds)
	; (visitor-beam mesh view eye files seconds) -> draws
	;its beam on the rock it looks at, as bright as it is on: from under it
	;to the rock, a third as wide as the craft, pale blue
	(cond
		((and visitor visit_rock (> visit_beam +double_1/20))
			(bind '(& & bulk kind &ignore) (elem-get rocks visit_rock))
			;from its underside to the rock's face, where the rock is seen
			(defq at (visit-rock-at eye))
			(defq from (nums-sub visit_at (nums-scale visit_up (* (get :size visitor) +belly))) d (nums-sub at from)
				long (* (max +double_1 (- (sqrt (nums-dot d d)) (* bulk (elem-get kind_reach kind) +double_1/2))) visit_beam)
				d (sky-unit d)
				x (sky-unit (sky-cross d (if (> (abs (second d)) +beam_steep) (doubles +double_1 +double_0 +double_0)
					(doubles +double_0 +double_1 +double_0))))
				;square to it, as the tube's own are, not its mirror, so its faces
				;face out
				z (sky-cross d x) wide (* (get :size visitor) +visit_beam_wide))
			(bind '(xx xy xz) (nums-scale x wide))
			(bind '(yx yy yz) (nums-scale d (neg long)))
			(bind '(zx zy zz) (nums-scale z wide))
			(bind '(px py pz) from)
			(list (list mesh
				(list (list 'model (doubles xx yx zx px xy yy zy py xz yz zz pz +double_0 +double_0 +double_0 +double_1))
					(list 'lens view) (list 'eye eye))
				(list (list 'colour (nums-scale (rgb 0.7 0.9 1.0) (* visit_beam +beam_bright))) (list 'eye eye) (list 'time seconds))
				'() files)))
		((list))))

;module
(export-symbols
	'(visitor visit_phase visit_next visit_at visit_way visit_until visit_rock visit_beam visit_alarm visit_side visit_goal visit_up visit_across visit_bulk
	visitor_parts tube-verts visitor-step visitor-hit? visitor-draws visitor-beam))
(env-pop)
