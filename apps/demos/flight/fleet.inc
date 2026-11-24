;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The fleet: the craft that come about the flight, each by its own
; rules, none directed.
;
; A craft is a scene, lib/math/scene.inc, a tree of its parts, each
; part a mesh of the farm's, craft.inc, and how it looks to the craft's
; shaders. It flies by the rules of its kind, and stays in front of the
; flight to be seen, till its time is up: a scout and a blob come in out
; of the haze, keep ahead weaving, leaning into their turns as an
; aircraft does but harder, to show what they are, then turn and race at
; the flight and past it close, and away; an orb darts from place to
; place ahead and stops dead, as no aircraft can, then darts past and
; goes straight up. Its place in the world is a matrix made of its way
; and its lean, no angles, as the flight's own view is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +fleet_most 3
	;where a craft comes from, how far ahead and how far to a side, and how
	;far off it is gone
	+come_far (n2d 360) +come_aside (n2d 110) +gone_far (n2d 470)
	;how near the land a craft may go
	+craft_floor (n2d 8))

(defun rand-d (lo hi)
	; (rand-d lo hi) -> double
	(+ lo (* (- hi lo) (/ (n2d (random 1000)) (n2d 1000)))))

(defun rgb (r g b)
	; (rgb r g b) -> doubles
	(doubles (n2d r) (n2d g) (n2d b)))

(defun part-node (part tint glow film &optional panels lamps)
	; (part-node part tint glow film [panels lamps]) -> scene_node
	;a part of a craft: the farm's mesh of it, the colour of its metal,
	;the light it gives, how much of the film it has, how many panels
	;across, and the colour of the lamps round its rim
	(def (defq node (Scene-node)) :part part :tint tint :glow glow :film film
		:panels (ifn panels +double_0) :lamps (ifn lamps (rgb 0 0 0)))
	node)

(defun craft-node (kind size wait)
	; (craft-node kind size wait) -> scene_node
	;a craft, its kind, how big, and when it first comes, in microseconds
	(def (defq craft (Scene-node (str kind))) :kind kind :size (n2d size) :live :nil
		:wait (+ (pii-time) wait) :at (doubles +double_0 +double_0 +double_0)
		:head (doubles +double_1 +double_0) :lean +double_0 :turn +double_0 :climb +double_0)
	craft)

(defun make-scout ()
	; (make-scout) -> scene_node
	;a saucer, a ring of blobs that turns round it, and four lights under it,
	;the whole of it turning slowly
	(defq scout (craft-node :scout 8 1000000) dark (rgb 0 0 0))
	(def scout :spin (n2d 0.6) :cruise (n2d 55) :dash (n2d 130) :turn_most (n2d 1.1) :pull (n2d 30))
	(. scout :add_node (part-node +craft_saucer (rgb 0.55 0.58 0.62) dark (n2d 0.5)
		(n2d 4) (rgb 1.0 0.55 0.2)))
	(def (defq ring (part-node +craft_ring (rgb 0.3 0.32 0.36) (rgb 0.05 0.15 0.2) (n2d 0.9)
		(n2d 6) (rgb 1.0 0.55 0.2))) :spin (n2d 1.5))
	(. scout :add_node (. ring :set_scale (n2d 1.25) (n2d 1.25) (n2d 1.25)))
	(each (lambda ((x z))
			(. scout :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.45 1.0 0.8) +double_0)
				(:set_scale (n2d 0.09) (n2d 0.09) (n2d 0.09))
				(:set_translation x (n2d -0.12) z))))
		(list (list (n2d 0.6) +double_0) (list (n2d -0.6) +double_0)
			(list +double_0 (n2d 0.6)) (list +double_0 (n2d -0.6))))
	scout)

(defun make-orb ()
	; (make-orb) -> scene_node
	;a lattice ball that tumbles, a core in it of every colour in turn, and
	;three shards of light that go round it, each tumbling
	(defq orb (craft-node :orb 7 9000000) dark (rgb 0 0 0) lights (Scene-node))
	(def orb :flies :dart :ease (n2d 3) :wait_lo (n2d 1300000) :wait_hi (n2d 2400000) :hops_most 5)
	(def (defq shell (part-node +craft_lattice (rgb 0.32 0.27 0.2) dark (n2d 1.3)))
		:spin (n2d 0.4) :tumble (n2d 0.25))
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) dark +double_0)) :hues (n2d 1.3))
	(. orb :add_node shell)
	(. orb :add_node (. core :set_scale (n2d 0.42) (n2d 0.42) (n2d 0.42)))
	(def lights :spin (n2d 2.2))
	(each (lambda ((x z))
			(def (defq shard (part-node +craft_shard (rgb 0.3 0.2 0.3) (rgb 1.9 0.4 1.5) (n2d 1.2)))
				:spin (n2d 3.1) :tumble (n2d 2.3))
			(. lights :add_node (.-> shard
				(:set_scale (n2d 0.2) (n2d 0.2) (n2d 0.2))
				(:set_translation x +double_0 z))))
		(list (list (n2d 1.35) +double_0) (list (n2d -0.675) (n2d 1.17)) (list (n2d -0.675) (n2d -1.17))))
	(. orb :add_node lights)
	orb)

(defun make-blob ()
	; (make-blob) -> scene_node
	;a craft that is grown, not made: it breathes, and its film is thick
	(defq blob (craft-node :blob 9 17000000))
	(def blob :spin (n2d 0.25) :cruise (n2d 38) :dash (n2d 90) :turn_most (n2d 0.7) :pull (n2d 18))
	(def (defq body (part-node +craft_blob (rgb 0.2 0.42 0.3) (rgb 0.04 0.22 0.1) (n2d 1.6)))
		:breathe (n2d 0.07))
	(. blob :add_node body)
	(each (lambda ((x y z))
			(. blob :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.6 1.0 0.3) +double_0)
				(:set_scale (n2d 0.12) (n2d 0.12) (n2d 0.12))
				(:set_translation x y z))))
		(list (list (n2d 0.75) (n2d 0.15) +double_0) (list (n2d -0.4) (n2d 0.1) (n2d 0.65))))
	blob)

(defun make-triangle ()
	; (make-triangle) -> scene_node
	;a triangle of gunmetal, its nose ahead, ridged, a ring of light that
	;turns under its middle and a shard of light at each corner. It glides,
	;tipping slowly one way and the other to show itself, leans hard into
	;its turns, and goes as no aircraft can
	(defq tri (craft-node :triangle 11 25000000))
	(def tri :cruise (n2d 28) :dash (n2d 170) :turn_most (n2d 0.6) :pull (n2d 14) :show (n2d 0.7))
	(. tri :add_node (part-node +craft_delta (rgb 0.34 0.36 0.4) (rgb 0 0 0) (n2d 1.0)
		(n2d 7) (rgb 0.3 0.9 1.4)))
	(def (defq ring (part-node +craft_ring (rgb 0.1 0.1 0.12) (rgb 0.2 0.9 1.3) (n2d 0.6)))
		:spin (n2d -2.5))
	(. tri :add_node (.-> ring (:set_scale (n2d 0.32) (n2d 0.5) (n2d 0.32)) (:set_translation +double_0 (n2d -0.06) +double_0)))
	(each (lambda ((x z))
			(def (defq shard (part-node +craft_shard (rgb 0.3 0.3 0.3) (rgb 1.8 1.8 1.6) (n2d 0.8)))
				:spin (n2d 2.7) :tumble (n2d 1.9))
			(. tri :add_node (.-> shard (:set_scale (n2d 0.1) (n2d 0.1) (n2d 0.1))
				(:set_translation x (n2d 0.02) z))))
		(list (list +double_0 (n2d 0.8)) (list (n2d 0.7) (n2d -0.4)) (list (n2d -0.7) (n2d -0.4))))
	tri)

(defun make-jelly ()
	; (make-jelly) -> scene_node
	;a jellyfish: a glassy bell with a hem of waves, a core of every colour
	;in turn, and six tentacles, each a chain of shards, a link a node of
	;the one above it, each swaying, so the chain bends and whips
	(defq jelly (craft-node :jelly 9 33000000))
	(def jelly :flies :dart :ease (n2d 0.6) :wait_lo (n2d 2500000) :wait_hi (n2d 4000000) :hops_most 4
		:spin (n2d 0.2))
	(. jelly :add_node (part-node +craft_bell (rgb 0.45 0.55 0.7) (rgb 0.06 0.12 0.2) (n2d 1.5)))
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) (rgb 0 0 0) +double_0)) :hues (n2d 0.8))
	(. jelly :add_node (.-> core (:set_scale (n2d 0.3) (n2d 0.3) (n2d 0.3)) (:set_translation +double_0 (n2d 0.25) +double_0)))
	(each (lambda ((x z))
			(defq up (. (Scene-node) :set_translation x (n2d -0.12) z) at up)
			(. jelly :add_node up)
			(each (lambda (k)
					(def (defq link (. (Scene-node) :set_translation +double_0 (n2d -0.2) +double_0))
						:sway (list (n2d 0.35) (n2d 1.7) (+ (* (n2d k) (n2d 0.7)) x z)))
					(. link :add_node (. (part-node +craft_shard (rgb 0.3 0.2 0.3)
							(nums-scale (rgb 1.4 0.35 1.0) (- +double_1 (* (n2d 0.15) (n2d k)))) (n2d 1.0))
						:set_scale (n2d 0.07) (n2d 0.07) (n2d 0.07)))
					(. at :add_node link)
					(setq at link))
				'(0 1 2 3 4)))
		(list (list (n2d 0.5) +double_0) (list (n2d 0.25) (n2d 0.433)) (list (n2d -0.25) (n2d 0.433))
			(list (n2d -0.5) +double_0) (list (n2d -0.25) (n2d -0.433)) (list (n2d 0.25) (n2d -0.433))))
	jelly)

(defun make-top ()
	; (make-top) -> scene_node
	;two cones point to point, spinning fast and leaning as they spin, so
	;they wobble as a top does; it zigzags in quick darts
	(defq top (craft-node :top 6 41000000))
	(def top :flies :dart :ease (n2d 6) :wait_lo (n2d 500000) :wait_hi (n2d 1000000) :hops_most 9)
	(def (defq body (part-node +craft_top (rgb 0.7 0.55 0.25) (rgb 0.05 0.03 0) (n2d 0.8)
			(n2d 3) (rgb 0.3 1.0 0.6)))
		:spin (n2d 7) :tilt (n2d 0.2))
	(. top :add_node body)
	top)

(defun craft-start (craft)
	; (craft-start craft)
	;a craft comes: far ahead in the haze, off to a side, coming this way
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq side (if (= (random 2) 0) +double_1 (neg +double_1))
		far (rand-d (- +come_far (n2d 30)) (+ +come_far (n2d 40)))
		off (* side (rand-d (* +come_aside (n2d 0.5)) (* +come_aside (n2d 1.4))))
		x (+ px (* fx far) (* (neg fz) off)) z (+ pz (* fz far) (* fx off)))
	(def craft :live :t :phase :in :side side :hops 0 :next (pii-time) :born (pii-time)
		:until (+ (pii-time) (n2i (rand-d (n2d 9000000) (n2d 15000000))))
		:at (doubles x (+ (max height (land-at x z)) (rand-d (n2d 5) (n2d 40))) z)
		:head (doubles (neg fx) (neg fz)) :speed (ifn (get :cruise craft) +double_0)
		:lean +double_0 :turn +double_0 :climb +double_0))

(defun craft-gone (craft)
	; (craft-gone craft)
	;gone into the haze, and back after a while
	(def craft :live :nil :wait (+ (pii-time) (n2i (rand-d (n2d 3000000) (n2d 9000000))))))

(defun near-flight (ahead aside up)
	; (near-flight ahead aside up) -> (x y z)
	;a place by the flight: so far along its way, to its right, and above
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(list (+ px (* fx ahead) (* (neg fz) aside)) (+ height up) (+ pz (* fz ahead) (* fx aside))))

(defun swoop-goal (craft t)
	; (swoop-goal craft t) -> (x y z)
	;where a craft that swoops makes for, by what it is about, t seconds
	;since it came: a place ahead of the flight that weaves, then past the
	;flight close on a side, then away up and behind
	(defq side (get :side craft))
	(case (get :phase craft)
		(:out (near-flight (n2d -300) (* side (n2d 420)) (n2d 150)))
		(:pass (near-flight (n2d -80) (* side (n2d 14)) (n2d 3)))
		(:t (near-flight (+ (n2d 85) (* (n2d 25) (sin (* t (n2d 0.4)))))
			(* (n2d 45) (sin (+ (* t (n2d 0.6)) side)))
			(+ (n2d 9) (* (n2d 7) (sin (* t (n2d 0.9)))))))))

(defun swoop-fly (craft dt)
	; (swoop-fly craft dt)
	;in out of the haze, then ahead of the flight, weaving, at its pace,
	;till its time is up; then round and at the flight, past it close and
	;away, faster. It turns no faster than it may, eases into a turn, turns
	;the shorter way round to what is behind it, and leans into a turn, and
	;one that shows itself, :show, tips slowly one way and the other too
	(bind '(cx cy cz) (get :at craft))
	(bind '(hx hz) (get :head craft))
	(defq now (pii-time) phase (get :phase craft) t (/ (n2d (- now (get :born craft))) (n2d 1000000))
		goal (swoop-goal craft t))
	(bind '(gx gy gz) goal)
	(defq dx (- gx cx) dz (- gz cz) d (+ (sqrt (+ (* dx dx) (* dz dz))) (n2d 0.001))
		cross (/ (- (* hx dz) (* hz dx)) d) along (+ (* hx dx) (* hz dz))
		most (get :turn_most craft)
		want (if (< along +double_0) (if (< cross +double_0) most (neg most))
			(max (neg most) (min most (* (neg cross) (n2d 3)))))
		turn (+ (get :turn craft) (* (- want (get :turn craft)) (min +double_1 (* dt (n2d 2)))))
		a (* turn dt) ca (cos a) sa (sin a)
		nx (+ (* hx ca) (* hz sa)) nz (- (* hz ca) (* hx sa))
		pace (case phase
			(:in (get :cruise craft))
			(:pace (max (n2d 10) (min (get :cruise craft) (+ +speed (* along (n2d 0.9))))))
			(:t (get :dash craft)))
		speed (+ (get :speed craft) (* (- pace (get :speed craft)) (min +double_1 (* dt (n2d 0.8)))))
		climb (+ (get :climb craft) (* (- (max (n2d -15) (min (n2d 25) (* (- gy cy) (n2d 0.8)))) (get :climb craft))
			(min +double_1 (* dt (n2d 1.5)))))
		x (+ cx (* nx speed dt)) z (+ cz (* nz speed dt))
		y (max (+ cy (* climb dt)) (+ (land-at x z) +craft_floor)))
	(def craft :at (doubles x y z) :head (doubles nx nz) :turn turn :speed speed :climb climb
		:lean (max (n2d -2) (min (n2d 2) (+ (neg (/ (* speed turn) (get :pull craft)))
			(* (ifn (get :show craft) +double_0) (sin (* t (n2d 0.7))))))))
	;what it is about next
	(bind '(fx fz) way)
	(defq rx (- x (first place)) rz (- z (second place)))
	(case phase
		(:in (if (< (+ (* dx dx) (* dz dz) (* (- gy y) (- gy y))) (n2d 3600)) (def craft :phase :pace)))
		(:pace (if (> now (get :until craft)) (def craft :phase :pass)))
		(:pass (if (< (+ (* rx fx) (* rz fz)) (n2d -10)) (def craft :phase :out)))
		(:out (if (> (+ (* rx rx) (* rz rz)) (* +gone_far +gone_far)) (craft-gone craft)))))

(defun dart-fly (craft dt)
	; (dart-fly craft dt)
	;from place to place ahead of the flight, each reached by easing to it,
	;a dart that slows to a stop, or a drift, as quick as the craft's :ease,
	;a hover, then another; after its :hops, at the flight and past it
	;close, then straight up and gone
	(defq now (pii-time) hops (get :hops craft) most (get :hops_most craft))
	(when (> now (get :next craft))
		(def craft :next (+ now (n2i (if (= hops most) (n2d 1200000) (rand-d (get :wait_lo craft) (get :wait_hi craft)))))
			:hops (inc hops)
			:goal (cond
				((< hops most) (near-flight (rand-d (n2d 50) (n2d 140)) (rand-d (n2d -60) (n2d 60)) (rand-d (n2d -4) (n2d 30))))
				((= hops most) (near-flight (n2d -90) (* (get :side craft) (n2d 10)) (n2d 4)))
				((near-flight (n2d -60) +double_0 (n2d 900))))))
	(bind '(cx cy cz) (get :at craft))
	(bind '(gx gy gz) (get :goal craft))
	(defq k (min +double_1 (* dt (if (>= hops most) (n2d 3) (get :ease craft))))
		x (+ cx (* (- gx cx) k)) z (+ cz (* (- gz cz) k))
		y (max (+ cy (* (- gy cy) k)) (+ (land-at x z) +craft_floor)))
	(def craft :at (doubles x y z))
	(if (> (- y height) (n2d 400)) (craft-gone craft)))

(defun craft-matrix (craft seconds)
	; (craft-matrix craft seconds) -> mat4x4
	;the craft in the world: where it is, its way, its lean into a turn, and
	;its size. Its right and up are the level ones leant over, as the
	;flight's own are
	(bind '(x y z) (get :at craft))
	(bind '(hx hz) (get :head craft))
	(defq lean (get :lean craft) s (get :size craft)
		c (recip (sqrt (+ +double_1 (* lean lean)))) sn (* lean c)
		rx (* (neg hz) c) ry (neg sn) rz (* hx c)
		ux (* (neg hz) sn) uy c uz (* hx sn))
	(doubles (* rx s) (* ux s) (* hx s) x
		(* ry s) (* uy s) +double_0 y
		(* rz s) (* uz s) (* hz s) z
		+double_0 +double_0 +double_0 +double_1))

(defun craft-draws (craft view eye seconds)
	; (craft-draws craft view eye seconds) -> draws
	;a draw for each part of a craft whose mesh has come, by the matrix of
	;it and of the parts it is in. A part spins, tumbles or leans as it
	;spins, so it wobbles; sways, (amount speed phase); breathes; and has a
	;light of every colour in turn, as it says
	(defq mats (list (craft-matrix craft seconds)) out (list))
	(. craft :walk_nodes
		(lambda (node _)
			(cond
				((def? :sway node)
					(bind '(amount speed phase) (get :sway node))
					(defq at (+ (* seconds speed) phase))
					(. node :set_rotation (* amount (sin at)) +double_0
						(* amount (n2d 0.7) (cos (* at (n2d 0.8))))))
				((or (def? :spin node) (def? :tumble node) (def? :tilt node))
					(. node :set_rotation (+ (ifn (get :tilt node) +double_0) (* seconds (ifn (get :tumble node) +double_0)))
						(* seconds (ifn (get :spin node) +double_0)) +double_0)))
			(when (def? :breathe node)
				(defq b (+ +double_1 (* (get :breathe node) (sin (* seconds (n2d 2.3))))))
				(. node :set_scale b (/ +double_1 b) b))
			(if (def? :hues node)
				(def node :glow (doubles
					(+ (n2d 0.55) (* (n2d 0.45) (cos (* seconds (get :hues node)))))
					(+ (n2d 0.55) (* (n2d 0.45) (cos (+ (* seconds (get :hues node)) (n2d 2.094)))))
					(+ (n2d 0.55) (* (n2d 0.45) (cos (+ (* seconds (get :hues node)) (n2d 4.189))))))))
			(push mats (defq m (mat4x4-mul (last mats) (. node :get_matrix))))
			(and (def? :part node) (elem-get parts_had (defq part (get :part node)))
				(push out (list (+ +mesh_craft part)
					(list (list 'model m) (list 'lens view) (list 'eye eye))
					(list (list 'sun sun) (list 'haze haze) (list 'above above) (list 'eye eye)
						(list 'time seconds) (list 'tint (get :tint node)) (list 'glow (get :glow node))
						(list 'film (get :film node)) (list 'panels (get :panels node))
						(list 'lamps (get :lamps node)))
					'() craft_files)))
			:nil)
		(lambda (node _) (pop mats)))
	out)

(defun fleet-fly (dt)
	; (fleet-fly dt)
	;each craft that is about flies by its kind; one gone comes again when
	;its time is up, if there are not already as many about as may be
	(defq now (pii-time) about (reduce (lambda (n c) (if (get :live c) (inc n) n)) crafts 0))
	(each (lambda (craft)
			(cond
				((get :live craft)
					(if (eql (get :flies craft) :dart) (dart-fly craft dt) (swoop-fly craft dt)))
				((and (> now (get :wait craft)) (< about +fleet_most))
					(craft-start craft)
					(++ about))))
		crafts))

(defun fleet-draws (view eye seconds)
	; (fleet-draws view eye seconds) -> draws
	(reduce (lambda (draws craft)
			(if (get :live craft) (cat draws (craft-draws craft view eye seconds)) draws))
		crafts '()))
