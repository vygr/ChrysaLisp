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
	+come_far (n2d 470) +come_aside (n2d 110) +gone_far (n2d 620)
	;how far over the land a craft keeps, what it climbs to clear ahead of
	;it, and how near it may ever be, should that not be in time
	+craft_floor (n2d 8) +craft_last (n2d 3))

(defun rand-d (lo hi)
	; (rand-d lo hi) -> double
	(+ lo (* (- hi lo) (/ (n2d (random 1000)) (const (n2d 1000))))))

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
	(def scout :spin (const (n2d 0.6)) :cruise (const (n2d 55)) :dash (const (n2d 130)) :turn_most (const (n2d 1.1)) :pull (const (n2d 30))
		:light (rgb 1.0 0.6 0.25))
	(. scout :add_node (part-node +craft_saucer (rgb 0.55 0.58 0.62) dark +double_1/2
		+double_4 (rgb 1.0 0.55 0.2)))
	(def (defq ring (part-node +craft_ring (rgb 0.3 0.32 0.36) (rgb 0.05 0.15 0.2) (const (n2d 0.9))
		+double_6 (rgb 1.0 0.55 0.2))) :spin (const (n2d 1.5)))
	(. scout :add_node (. ring :set_scale (const (n2d 1.25)) (const (n2d 1.25)) (const (n2d 1.25))))
	(each (lambda ((x z))
			(. scout :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.45 1.0 0.8) +double_0)
				(:set_scale (const (n2d 0.09)) (const (n2d 0.09)) (const (n2d 0.09)))
				(:set_translation x (const (n2d -0.12)) z))))
		(list (list (const (n2d 0.6)) +double_0) (list (const (n2d -0.6)) +double_0)
			(list +double_0 (const (n2d 0.6))) (list +double_0 (const (n2d -0.6)))))
	scout)

(defun make-orb ()
	; (make-orb) -> scene_node
	;a lattice ball that tumbles, a core in it of every colour in turn, and
	;three shards of light that go round it, each tumbling
	(defq orb (craft-node :orb 7 9000000) dark (rgb 0 0 0) lights (Scene-node))
	(def orb :flies :dart :ease +double_3 :wait_lo (const (n2d 1300000)) :wait_hi (const (n2d 2400000)) :hops_most 5
		:light_hues (const (n2d 1.3)))
	(def (defq shell (part-node +craft_lattice (rgb 0.32 0.27 0.2) dark (const (n2d 1.3))))
		:spin (const (n2d 0.4)) :tumble (const (n2d 0.25)))
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) dark +double_0)) :hues (const (n2d 1.3)))
	(. orb :add_node shell)
	(. orb :add_node (. core :set_scale (const (n2d 0.42)) (const (n2d 0.42)) (const (n2d 0.42))))
	(def lights :spin (const (n2d 2.2)))
	(each (lambda ((x z))
			(def (defq shard (part-node +craft_shard (rgb 0.3 0.2 0.3) (rgb 1.9 0.4 1.5) (const (n2d 1.2))))
				:spin (const (n2d 3.1)) :tumble (const (n2d 2.3)))
			(. lights :add_node (.-> shard
				(:set_scale (const (n2d 0.2)) (const (n2d 0.2)) (const (n2d 0.2)))
				(:set_translation x +double_0 z))))
		(list (list (const (n2d 1.35)) +double_0) (list (const (n2d -0.675)) (const (n2d 1.17))) (list (const (n2d -0.675)) (const (n2d -1.17)))))
	(. orb :add_node lights)
	orb)

(defun make-blob ()
	; (make-blob) -> scene_node
	;a craft that is grown, not made: it breathes, and its film is thick
	(defq blob (craft-node :blob 9 17000000))
	(def blob :spin (const (n2d 0.25)) :cruise (const (n2d 38)) :dash (const (n2d 90)) :turn_most (const (n2d 0.7)) :pull (const (n2d 18))
		:light (rgb 0.5 1.0 0.3))
	(def (defq body (part-node +craft_blob (rgb 0.2 0.42 0.3) (rgb 0.04 0.22 0.1) (const (n2d 1.6))))
		:breathe (const (n2d 0.07)))
	(. blob :add_node body)
	(each (lambda ((x y z))
			(. blob :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.6 1.0 0.3) +double_0)
				(:set_scale (const (n2d 0.12)) (const (n2d 0.12)) (const (n2d 0.12)))
				(:set_translation x y z))))
		(list (list (const (n2d 0.75)) (const (n2d 0.15)) +double_0) (list (const (n2d -0.4)) (const (n2d 0.1)) (const (n2d 0.65)))))
	blob)

(defun make-triangle ()
	; (make-triangle) -> scene_node
	;a triangle of gunmetal, its nose ahead, ridged, a ring of light that
	;turns under its middle and a ball of light at each corner. It glides,
	;tipping slowly one way and the other to show itself, leans hard into
	;its turns, and goes as no aircraft can
	(defq tri (craft-node :triangle 11 25000000))
	(def tri :cruise (const (n2d 28)) :dash (const (n2d 170)) :turn_most (const (n2d 0.6)) :pull (const (n2d 14)) :show (const (n2d 0.7))
		:light (rgb 0.35 0.9 1.3))
	(. tri :add_node (part-node +craft_delta (rgb 0.34 0.36 0.4) (rgb 0 0 0) +double_1
		+double_7 (rgb 0.3 0.9 1.4)))
	(def (defq ring (part-node +craft_ring (rgb 0.1 0.1 0.12) (rgb 0.2 0.9 1.3) (const (n2d 0.6))))
		:spin (const (n2d -2.5)))
	(. tri :add_node (.-> ring (:set_scale (const (n2d 0.32)) +double_1/2 (const (n2d 0.32))) (:set_translation +double_0 (const (n2d -0.06)) +double_0)))
	(each (lambda ((x z))
			(. tri :add_node (.-> (part-node +craft_ball (rgb 0.3 0.3 0.3) (rgb 1.8 1.8 1.6) +double_0)
				(:set_scale (const (n2d 0.08)) (const (n2d 0.08)) (const (n2d 0.08)))
				(:set_translation x (const (n2d 0.02)) z))))
		(list (list +double_0 (const (n2d 0.8))) (list (const (n2d 0.7)) (const (n2d -0.4))) (list (const (n2d -0.7)) (const (n2d -0.4)))))
	tri)

(defun make-jelly ()
	; (make-jelly) -> scene_node
	;a jellyfish: a glassy bell with a hem of waves, a core of every colour
	;in turn, and six tentacles, each a chain of shards, a link a node of
	;the one above it, each swaying, so the chain bends and whips
	(defq jelly (craft-node :jelly 9 33000000))
	(def jelly :flies :dart :ease (const (n2d 0.6)) :wait_lo (const (n2d 2500000)) :wait_hi (const (n2d 4000000)) :hops_most 4
		:spin (const (n2d 0.2)) :light (rgb 1.3 0.35 1.0))
	(. jelly :add_node (part-node +craft_bell (rgb 0.45 0.55 0.7) (rgb 0.06 0.12 0.2) (const (n2d 1.5))))
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) (rgb 0 0 0) +double_0)) :hues (const (n2d 0.8)))
	(. jelly :add_node (.-> core (:set_scale (const (n2d 0.3)) (const (n2d 0.3)) (const (n2d 0.3))) (:set_translation +double_0 (const (n2d 0.25)) +double_0)))
	(each (lambda ((x z))
			(defq up (. (Scene-node) :set_translation x (const (n2d -0.12)) z) at up)
			(. jelly :add_node up)
			(each (lambda (k)
					(def (defq link (. (Scene-node) :set_translation +double_0 (const (n2d -0.2)) +double_0))
						:sway (list (const (n2d 0.35)) (const (n2d 1.7)) (+ (* (n2d k) (const (n2d 0.7))) x z)))
					(. link :add_node (. (part-node +craft_shard (rgb 0.3 0.2 0.3)
							(nums-scale (rgb 1.4 0.35 1.0) (- +double_1 (* (const (n2d 0.15)) (n2d k)))) +double_1)
						:set_scale (const (n2d 0.07)) (const (n2d 0.07)) (const (n2d 0.07))))
					(. at :add_node link)
					(setq at link))
				'(0 1 2 3 4)))
		(list (list +double_1/2 +double_0) (list (const (n2d 0.25)) (const (n2d 0.433))) (list (const (n2d -0.25)) (const (n2d 0.433)))
			(list +double_-1/2 +double_0) (list (const (n2d -0.25)) (const (n2d -0.433))) (list (const (n2d 0.25)) (const (n2d -0.433)))))
	jelly)

(defun make-top ()
	; (make-top) -> scene_node
	;two cones point to point, spinning fast and leaning as they spin, so
	;they wobble as a top does; it zigzags in quick darts
	(defq top (craft-node :top 6 41000000))
	(def top :flies :dart :ease +double_6 :wait_lo (const (n2d 500000)) :wait_hi (const (n2d 1000000)) :hops_most 9
		:light (rgb 0.4 1.1 0.6))
	(def (defq body (part-node +craft_top (rgb 0.7 0.55 0.25) (rgb 0.05 0.03 0) (const (n2d 0.8))
			+double_3 (rgb 0.3 1.0 0.6)))
		:spin +double_7 :tilt (const (n2d 0.2)))
	(. top :add_node body)
	top)

(defun skim? ()
	; (skim? ) -> :t | :nil
	;whether a craft keeps low over the land or up with the flight, by how
	;high the flight is over the land: low, most skim; high, most are up
	;with it, where it is; but always a quarter or more of each, so the land
	;has its craft seen from high and the sky its seen from low
	(defq over (- height (land-at (first place) (second place)))
		chance (max (const (n2d 0.25)) (min (const (n2d 0.75)) (- (const (n2d 1.1)) (/ over (const (n2d 80)))))))
	(< (rand-d +double_0 +double_1) chance))

(defun craft-start (craft)
	; (craft-start craft)
	;a craft comes: far ahead in the haze, off to a side, coming this way
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq side (if (= (random 2) 0) +double_1 (neg +double_1))
		far (rand-d (- +come_far (const (n2d 30))) (+ +come_far (const (n2d 40))))
		off (* side (rand-d (* +come_aside +double_1/2) (* +come_aside (const (n2d 1.4)))))
		x (+ px (* fx far) (* (neg fz) off)) z (+ pz (* fz far) (* fx off)))
	;low over the land this time, its light on it, or up with the flight,
	;the more of them up the higher the flight is
	(def craft :live :t :phase :in :side side :hops 0 :next (pii-time) :born (pii-time)
		;a beam on the land, now and then, from one that comes in low to stop
		;and use it, whatever the height of the flight
		:beam (defq beam (and (find (get :kind craft) '(:scout :triangle :blob)) (< (random 6) 5)))
		:skim (or beam (skim?)) :up (rand-d (const (n2d -12)) (const (n2d 45)))
		:until (+ (pii-time) (n2i (rand-d (const (n2d 9000000)) (const (n2d 15000000)))))
		:at (doubles x (+ (max height (land-at x z)) (rand-d +double_5 (const (n2d 40)))) z)
		:head (doubles (neg fx) (neg fz)) :speed (ifn (get :cruise craft) +double_0)
		:lean +double_0 :turn +double_0 :climb +double_0))

(defun craft-gone (craft)
	; (craft-gone craft)
	;gone into the haze, and back after a while, sooner after a snatch
	(def craft :live :nil :wait (+ (pii-time) (n2i (if (get :beam craft)
		(rand-d (const (n2d 2000000)) (const (n2d 5000000))) (rand-d (const (n2d 3000000)) (const (n2d 9000000))))))))

(defun near-flight (ahead aside up)
	; (near-flight ahead aside up) -> (x y z)
	;a place by the flight: so far along its way, to its right, and above
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(list (+ px (* fx ahead) (* (neg fz) aside)) (+ height up) (+ pz (* fz ahead) (* fx aside))))

(defun land-along (x z tx tz)
	; (land-along x z tx tz) -> double
	;the highest of the land from here to there, at a quarter of the way at
	;a time: what a craft going that way has to clear
	(defq dx (- tx x) dz (- tz z))
	(max (land-at x z)
		(land-at (+ x (* dx (const (n2d 0.25)))) (+ z (* dz (const (n2d 0.25)))))
		(land-at (+ x (* dx +double_1/2)) (+ z (* dz +double_1/2)))
		(land-at (+ x (* dx (const (n2d 0.75)))) (+ z (* dz (const (n2d 0.75)))))
		(land-at tx tz)))

(defun near-land (ahead aside up)
	; (near-land ahead aside up) -> (x y z)
	;a place by the flight, so far along its way and to its right, and so
	;far over the land there: low, for its light to fall on the land
	(bind '(x & z) (near-flight ahead aside +double_0))
	(list x (+ (land-at x z) up) z))

(defun swoop-goal (craft t)
	; (swoop-goal craft t) -> (x y z)
	;where a craft that swoops makes for, by what it is about, t seconds
	;since it came: a place ahead of the flight that weaves, low over the
	;land or up with the flight, as it came, then past the flight close
	;on a side, then away up and behind
	(defq side (get :side craft))
	(cond
		((eql (get :phase craft) :out) (near-flight (const (n2d -300)) (* side (const (n2d 420))) (const (n2d 150))))
		((eql (get :phase craft) :pass) (near-flight (const (n2d -80)) (* side (const (n2d 14))) +double_3))
		;one with a beam makes for a place over the land well ahead, to stop
		((get :beam craft) (near-land (const (n2d 230)) (* side +double_10) (const (n2d 14))))
		(:t (defq ahead (+ (const (n2d 85)) (* (const (n2d 25)) (sin (* t (const (n2d 0.4))))))
				aside (* (const (n2d 45)) (sin (+ (* t (const (n2d 0.6))) side))))
			(if (get :skim craft)
				(near-land ahead aside (+ (const (n2d 14)) (* +double_4 (sin (* t (const (n2d 0.9)))))))
				(near-flight ahead aside (+ (get :up craft) (* +double_7 (sin (* t (const (n2d 0.9)))))))))))

(defun flight-ahead (secs)
	; (flight-ahead secs) -> (x z fx fz)
	;where the flight will be in so many seconds, and its way then: its turn
	;now carried on, in steps, so what is put ahead of that is in front of
	;the eye then, turning or not
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq turn (neg (/ (* lean_want +pull) +speed)) dt (/ secs +double_10)
		a (* turn dt) ca (cos a) sa (sin a))
	(each! (lambda ()
			(setq px (+ px (* fx +speed dt)) pz (+ pz (* fz +speed dt)))
			(defq nx (+ (* fx ca) (* fz sa)) nz (- (* fz ca) (* fx sa)))
			(setq fx nx fz nz))
		'() 0 10)
	(list px pz fx fz))

(defun stop-place ()
	; (stop-place) -> :nil | (x y z)
	;where a craft stops to use its beam: in front of the eye as the snatch
	;ends, over land, cows do not live in water, but now and then over the
	;water, manatees do; and high enough to be seen over the land between,
	;a hill in the way along the valleys, its beam the longer. As far ahead
	;as is seen well from how high the eye is over the land: low along the
	;valleys near, up to twice as far from the eight's height, so it is not
	;under the nose. :nil if there is no land in front to be had
	(bind '(ex ez fx fz) (flight-ahead (const (n2d 3.5))))
	(defq wet (= (random 8) 0) at :nil
		far (max +double_1 (min +double_2 (+ +double_1
			(/ (- height (land-at (first place) (second place)) (const (n2d 24))) (const (n2d 60)))))))
	(some (lambda ((ahead aside))
			(setq ahead (* ahead far) aside (* aside far))
			(defq x (+ ex (* fx ahead) (* (neg fz) aside)) z (+ ez (* fz ahead) (* fx aside)))
			(if (or wet (> (land_height (doubles x z)) (const (n2d -2.5)))) (setq at (list x z))))
		(list (list (const (n2d 90)) +double_0) (list (const (n2d 76)) (const (n2d 13))) (list (const (n2d 76)) (const (n2d -13)))
			(list (const (n2d 106)) (const (n2d 15))) (list (const (n2d 106)) (const (n2d -15))) (list (const (n2d 72)) +double_0)
			(list (const (n2d 120)) +double_0) (list (const (n2d 90)) (const (n2d 24))) (list (const (n2d 90)) (const (n2d -24)))))
	(when at
		(bind '(x z) at)
		(defq water (<= (land_height (doubles x z)) (const (n2d -2.5))))
		(defq low (+ (land-at x z) (const (n2d 14))) y low)
		(each (lambda (f)
				(defq lx (+ ex (* (- x ex) f)) lz (+ ez (* (- z ez) f))
					over (+ (land-at lx lz) +double_3))
				;the line from the eye to the craft clears the land here
				(setq y (max y (+ height (/ (- over height) f)))))
			(list (const (n2d 0.3)) +double_1/2 (const (n2d 0.7)) (const (n2d 0.85)) (const (n2d 0.95))))
		(list x (min y (+ low (const (n2d 46)))) z water)))

(defun hover-fly (craft dt)
	; (hover-fly craft dt)
	;stopping at its place over the land, and still there: eased to it, its
	;way kept, its turn and lean let go; there, its beam on, snapped, for a
	;second or two, then off, and away with its cow, or on as before if it
	;had none
	(bind '(cx cy cz) (get :at craft))
	(bind '(gx gy gz) (get :stop craft))
	(defq k (min +double_1 (* dt (const (n2d 2.2)))) now (pii-time)
		x (+ cx (* (- gx cx) k)) y (+ cy (* (- gy cy) k)) z (+ cz (* (- gz cz) k)))
	(def craft :at (doubles x y z) :speed +double_0 :climb +double_0
		:turn (* (get :turn craft) (- +double_1 k)) :lean (* (get :lean craft) (- +double_1 k)))
	(case (get :phase craft)
		(:stopping
			(when (< (+ (* (- gx x) (- gx x)) (* (- gy y) (- gy y)) (* (- gz z) (- gz z))) +double_6)
				;over land a cow comes up the beam; over water nothing does, and
				;it gives up the sooner
				(def craft :phase :beam :beamed now
					:until (+ now (n2i (if (get :wet craft) (rand-d (const (n2d 1000000)) (const (n2d 1300000)))
						(rand-d (const (n2d 2400000)) (const (n2d 3200000)))))))))
		(:beam
			;a cow had, it is away at speed; nothing had, over the water, it
			;flies on as it would have, ahead a while, then past and away
			(when (> now (get :until craft))
				(if (get :wet craft)
					(def craft :phase :pace :beam :nil :speed (const (n2d 15))
						:until (+ now (n2i (rand-d (const (n2d 6000000)) (const (n2d 10000000))))))
					(def craft :phase :out :speed (const (n2d 45))))))))

(defun swoop-move (craft dt)
	; (swoop-move craft dt)
	;in out of the haze, then ahead of the flight, weaving, at its pace,
	;till its time is up; then round and at the flight, past it close and
	;away, faster. It turns no faster than it may, eases into a turn, turns
	;the shorter way round to what is behind it, and leans into a turn, and
	;one that shows itself, :show, tips slowly one way and the other too
	(bind '(cx cy cz) (get :at craft))
	(bind '(hx hz) (get :head craft))
	(defq now (pii-time) phase (get :phase craft) t (/ (n2d (- now (get :born craft))) (const (n2d 1000000)))
		goal (swoop-goal craft t))
	(bind '(gx gy gz) goal)
	(defq dx (- gx cx) dz (- gz cz) d (+ (sqrt (+ (* dx dx) (* dz dz))) (const (n2d 0.001)))
		cross (/ (- (* hx dz) (* hz dx)) d) along (+ (* hx dx) (* hz dz))
		most (get :turn_most craft)
		want (if (< along +double_0) (if (< cross +double_0) most (neg most))
			(max (neg most) (min most (* (neg cross) +double_3))))
		turn (+ (get :turn craft) (* (- want (get :turn craft)) (min +double_1 (* dt +double_2))))
		a (* turn dt) ca (cos a) sa (sin a)
		nx (+ (* hx ca) (* hz sa)) nz (- (* hz ca) (* hx sa))
		pace (case phase
			(:in (get :cruise craft))
			(:pace (max +double_10 (min (get :cruise craft) (+ +speed (* along (const (n2d 0.9)))))))
			(:t (get :dash craft)))
		;away from a stop it zooms, quicker to its dash
		speed (+ (get :speed craft) (* (- pace (get :speed craft)) (min +double_1 (* dt (if (get :beam craft) (const (n2d 1.8)) (const (n2d 0.8)))))))
		;what it makes for, but over the land it will be over in a while, so
		;it climbs to clear a hill in a curve before it is there, as the
		;flight does
		reach (max (const (n2d 30)) (* speed (const (n2d 1.8))))
		over (max gy (+ (land-along cx cz (+ cx (* nx reach)) (+ cz (* nz reach))) +craft_floor))
		climb (+ (get :climb craft) (* (- (max (const (n2d -15)) (min (const (n2d 30)) (* (- over cy) (const (n2d 0.9))))) (get :climb craft))
			(min +double_1 (* dt (const (n2d 1.5))))))
		x (+ cx (* nx speed dt)) z (+ cz (* nz speed dt))
		y (max (+ cy (* climb dt)) (+ (land-at x z) +craft_last)))
	(def craft :at (doubles x y z) :head (doubles nx nz) :turn turn :speed speed :climb climb
		:lean (max +double_-2 (min +double_2 (+ (neg (/ (* speed turn) (get :pull craft)))
			(* (ifn (get :show craft) +double_0) (sin (* t (const (n2d 0.7)))))))))
	;what it is about next
	(bind '(fx fz) way)
	(defq rx (- x (first place)) rz (- z (second place)))
	(case phase
		(:in (when (< (+ (* dx dx) (* dz dz) (* (- gy y) (- gy y))) (const (n2d 3600)))
			;one with a beam stops where it is making for, still on the land
			;as the flight comes on; the others keep station ahead
			;with no land in front for it, no snatch this time
			(if (and (get :beam craft) (defq stop (stop-place)))
				(def craft :phase :stopping :stop (slice stop 0 3) :wet (elem-get stop 3))
				(def craft :phase :pace :beam :nil))))
		(:pace (if (> now (get :until craft)) (def craft :phase :pass)))
		(:pass (if (< (+ (* rx fx) (* rz fz)) +double_-10) (def craft :phase :out)))
		(:out (if (> (+ (* rx rx) (* rz rz)) (* +gone_far +gone_far)) (craft-gone craft)))))


(defun swoop-fly (craft dt)
	; (swoop-fly craft dt)
	;a craft that swoops, stopped for its beam or on the move
	(if (find (get :phase craft) '(:stopping :beam)) (hover-fly craft dt) (swoop-move craft dt)))

(defun dart-fly (craft dt)
	; (dart-fly craft dt)
	;from place to place ahead of the flight, each reached by easing to it,
	;a dart that slows to a stop, or a drift, as quick as the craft's :ease,
	;a hover, then another; after its :hops, at the flight and past it
	;close, then straight up and gone
	(defq now (pii-time) hops (get :hops craft) most (get :hops_most craft))
	(when (> now (get :next craft))
		(def craft :next (+ now (n2i (if (= hops most) (const (n2d 1200000)) (rand-d (get :wait_lo craft) (get :wait_hi craft)))))
			:hops (inc hops)
			:goal (cond
				;each hop low over the land or high in the sky
				((< hops most)
					(if (skim?)
						(near-land (rand-d (const (n2d 50)) (const (n2d 140))) (rand-d (const (n2d -60)) (const (n2d 60))) (rand-d +double_9 (const (n2d 20))))
						(near-flight (rand-d (const (n2d 50)) (const (n2d 140))) (rand-d (const (n2d -60)) (const (n2d 60))) (rand-d (const (n2d -12)) (const (n2d 45))))))
				((= hops most) (near-flight (const (n2d -90)) (* (get :side craft) +double_10) +double_4))
				((near-flight (const (n2d -60)) +double_0 (const (n2d 900)))))))
	(bind '(cx cy cz) (get :at craft))
	(bind '(gx gy gz) (get :goal craft))
	(defq k (min +double_1 (* dt (if (>= hops most) +double_3 (get :ease craft))))
		;over the land between here and there, risen to as it goes
		over (max gy (+ (land-along cx cz gx gz) +craft_floor))
		x (+ cx (* (- gx cx) k)) z (+ cz (* (- gz cz) k))
		y (max (+ cy (* (- over cy) k)) (+ (land-at x z) +craft_last)))
	(def craft :at (doubles x y z))
	(if (> (- y height) (const (n2d 400))) (craft-gone craft)))

(defun craft-matrix (craft)
	; (craft-matrix craft) -> mat4x4
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

(defun hues (seconds rate)
	; (hues seconds rate) -> doubles
	;a light of every colour in turn, as it is now, round them at a rate
	(defq at (* seconds rate))
	(doubles (+ (const (n2d 0.55)) (* (const (n2d 0.45)) (cos at)))
		(+ (const (n2d 0.55)) (* (const (n2d 0.45)) (cos (+ at (const (n2d 2.094))))))
		(+ (const (n2d 0.55)) (* (const (n2d 0.45)) (cos (+ at (const (n2d 4.189))))))))

(defun craft-draws (craft world seconds)
	; (craft-draws craft world seconds) -> draws
	;a draw for each part of a craft whose mesh has come, by the matrix of
	;it and of the parts it is in. A part spins, tumbles or leans as it
	;spins, so it wobbles; sways, (amount speed phase); breathes; and has a
	;light of every colour in turn, as it says. world is what every part
	;is given the same, (lens eye pvals), (fleet-draws)
	(bind '(lens eye pvals) world)
	(defq mats (list (craft-matrix craft)) out (list))
	(. craft :walk_nodes
		(lambda (node &)
			(cond
				((def? :sway node)
					(bind '(amount speed phase) (get :sway node))
					(defq at (+ (* seconds speed) phase))
					(. node :set_rotation (* amount (sin at)) +double_0
						(* amount (const (n2d 0.7)) (cos (* at (const (n2d 0.8)))))))
				((or (def? :spin node) (def? :tumble node) (def? :tilt node))
					(. node :set_rotation (+ (ifn (get :tilt node) +double_0) (* seconds (ifn (get :tumble node) +double_0)))
						(* seconds (ifn (get :spin node) +double_0)) +double_0)))
			(when (def? :breathe node)
				(defq b (+ +double_1 (* (get :breathe node) (sin (* seconds (const (n2d 2.3)))))))
				(. node :set_scale b (/ +double_1 b) b))
			(if (def? :hues node) (def node :glow (hues seconds (get :hues node))))
			(push mats (defq m (mat4x4-mul (last mats) (. node :get_matrix))))
			(and (def? :part node) (elem-get parts_had (defq part (get :part node)))
				(push out (list (+ +mesh_craft part)
					(list (list 'model m) lens eye)
					(cat pvals (list (list 'tint (get :tint node)) (list 'glow (get :glow node))
						(list 'film (get :film node)) (list 'panels (get :panels node))
						(list 'lamps (get :lamps node)) (list 'patches +double_0)))
					'() craft_files)))
			:nil)
		(lambda (&ignore) (pop mats)))
	out)

(defun beaming? (craft)
	; (beaming? craft) -> :t | :nil
	;a craft with its beam on: one that has one, stopped
	(and (get :live craft) (eql (get :phase craft) :beam)))

(defun beam-ease (craft dt)
	; (beam-ease craft dt)
	;how far its beam is on, 0 to 1, eased to whether it is, quickly: it
	;snaps on, and off
	(defq power (ifn (get :beam_power craft) +double_0))
	(def craft :beam_power (+ power (* (- (if (beaming? craft) +double_1 +double_0) power)
		(min +double_1 (* dt (const (n2d 14))))))))

(defun fleet-fly (dt)
	; (fleet-fly dt)
	;each craft that is about flies by its kind; one gone comes again when
	;its time is up, if there are not already as many about as may be
	(defq now (pii-time) about (reduce (lambda (n c) (if (get :live c) (inc n) n)) crafts 0))
	(each (lambda (craft)
			(cond
				((get :live craft)
					(if (eql (get :flies craft) :dart) (dart-fly craft dt) (swoop-fly craft dt))
					(beam-ease craft dt))
				((and (> now (get :wait craft)) (< about +fleet_most))
					(craft-start craft)
					(++ about))))
		crafts))

(defun craft-light (craft seconds)
	; (craft-light craft seconds) -> doubles
	;the colour of the light a craft casts on the land: its :light, or
	;for one of every colour in turn, as its core is now
	(if (def? :light_hues craft) (hues seconds (get :light_hues craft)) (get :light craft)))

(defun fleet-lights (seconds)
	; (fleet-lights seconds) -> ((name val) ...)
	;the craft about for the land's shader, three, each a ball where it is
	;and as big as it, and its light; none, a ball of no size
	(defq out (list) names '((craft_a light_a) (craft_b light_b) (craft_c light_c)) i 0)
	(each (lambda (craft)
			(when (and (get :live craft) (< i 3))
				(bind '(x y z) (get :at craft))
				(bind '(cn ln) (elem-get names i))
				;a beam's pool of light on the land, white and bright
				;its own light at half while its beam is on, the beam's draw
				(push out (list cn (doubles x y z (* (get :size craft) (const (n2d 0.85)))))
					(list ln (nums-scale (craft-light craft seconds)
						(- +double_1 (* +double_1/2 (ifn (get :beam_power craft) +double_0))))))
				(++ i)))
		crafts)
	(while (< i 3)
		(bind '(cn ln) (elem-get names i))
		(push out (list cn (doubles +double_0 +double_0 +double_0 +double_0)) (list ln (rgb 0 0 0)))
		(++ i))
	;the pool of the brightest beam on, where its column meets the land
	(defq pool (doubles +double_0 +double_0 +double_0 +double_0))
	(each (lambda (craft)
			(when (and (get :live craft) (> (defq power (ifn (get :beam_power craft) +double_0)) (elem-get pool 3)))
				(bind '(x & z) (get :at craft))
				(setq pool (doubles x z (* (get :size craft) (const (n2d 0.4))) power))))
		crafts)
	(push out (list 'beam pool)))

(defun fleet-beams (view eye seconds)
	; (fleet-beams view eye seconds) -> draws
	;the beams of the craft that have them on: a column from under the craft
	;down to the land, as wide as a third of the craft, white,
	;not the colour of the craft's own light; drawn after all else, added
	(reduce (lambda (draws craft)
			(when (and (get :live craft) (> (defq power (ifn (get :beam_power craft) +double_0)) (const (n2d 0.02))))
				;as far down and as bright as it is on
				(bind '(x y z) (get :at craft))
				(defq top (- y (* (get :size craft) (const (n2d 0.12)))) long (* (- top (land-at x z)) power))
				(when (> long +double_1)
					(defq wide (* (get :size craft) (const (n2d 0.33))))
					(push draws (list +mesh_beam
						(list (list 'model (mat4x4-mul (mat4x4-translate x top z) (mat4x4-scale wide long wide)))
							(list 'lens view) (list 'eye eye))
						(list (list 'colour (nums-scale (rgb 0.95 0.97 1.0) power)) (list 'eye eye) (list 'time seconds))
						'() beam_files))))
			draws)
		crafts (list)))

(defun cow-draws (world seconds)
	; (cow-draws world seconds) -> draws
	;a cow coming up the beam of a craft at a snatch over land: from the
	;land to the craft as the beam is on, slow at first and at the last,
	;turning as it goes, and gone into the craft as the beam goes off
	(bind '(lens eye pvals) world)
	(reduce (lambda (draws craft)
			(when (and (beaming? craft) (not (get :wet craft)) (elem-get parts_had +craft_cow))
				(bind '(x y z) (get :at craft))
				(defq from (get :beamed craft) t (/ (n2d (- (pii-time) from)) (n2d (- (get :until craft) from)))
					t (min +double_1 t) t (* t t (- +double_3 (* +double_2 t)))
					ground (+ (land-at x z) (const (n2d 1.2))) top (- y (* (get :size craft) (const (n2d 0.25))))
					s (* (const (n2d 1.6)) (- +double_1 (* +double_1/2 (max +double_0 (- t (const (n2d 0.8)))) +double_5))))
				(push draws (list (+ +mesh_craft +craft_cow)
					(list (list 'model (mat4x4-mul (mat4x4-translate x (+ ground (* (- top ground) t)) z)
							(mat4x4-mul (mat4x4-roty (* seconds (const (n2d 1.3)))) (mat4x4-mul (mat4x4-rotx (* t (const (n2d 0.6))))
								(mat4x4-scale s s s)))))
						lens eye)
					(cat pvals (list (list 'tint (rgb 0.92 0.9 0.86)) (list 'glow (rgb 0 0 0))
						(list 'film +double_0) (list 'panels +double_0) (list 'lamps (rgb 0 0 0))
						(list 'patches +double_1)))
					'() craft_files)))
			draws)
		crafts (list)))

(defun craft-seen? (craft eye forward)
	; (craft-seen? craft eye forward) -> :t | :nil
	;a craft the eye may see any of: not behind it by more than the craft
	;reaches, twice its size, nor that far outside a cone round the way it
	;looks that holds all of the frame, however it leans. One that is not
	;is not drawn; its light on the land and its beam are their own
	(bind '(x y z) (get :at craft))
	(bind '(ex ey ez) eye)
	(bind '(fx fy fz) forward)
	(defq dx (- x ex) dy (- y ey) dz (- z ez) along (+ (* dx fx) (* dy fy) (* dz fz))
		reach (* (get :size craft) +double_2))
	(and (> along (neg reach))
		(<= (- (sqrt (max +double_0 (- (+ (* dx dx) (* dy dy) (* dz dz)) (* along along)))) (* along (const (n2d 0.9))))
			(* reach (const (n2d 1.4))))))

(defun fleet-draws (view eye forward seconds)
	; (fleet-draws view eye forward seconds) -> draws
	;the craft about that the eye may see, and a cow coming up a beam. What
	;every part of them is given the same is made the once, (lens eye
	;pvals), the one entry or list for them all
	(defq world (list (list 'lens view) (list 'eye eye)
		(list (list 'sun sun) (list 'haze haze) (list 'above above) (list 'eye eye) (list 'time seconds)
			(list 'daylight daylight) (list 'shine shine))))
	(cat (reduce (lambda (draws craft)
			(if (and (get :live craft) (craft-seen? craft eye forward))
				(cat draws (craft-draws craft world seconds)) draws))
		crafts '()) (cow-draws world seconds)))
