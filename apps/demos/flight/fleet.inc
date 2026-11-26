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

;module
(env-push)

(defq +fleet_most 3
	;where a craft comes from, how far ahead and how far to a side, and how
	;far off it is gone
	+come_far (n2d 470) +come_aside (n2d 110) +gone_far (n2d 620)
	;how far over the land a craft keeps, what it climbs to clear ahead of
	;it, and how near it may ever be, should that not be in time
	+craft_floor +double_8 +craft_last +double_3)

;the crafts, each its own: how it turns, how fast it cruises and dashes,
;how hard it turns and is pulled, how much film its parts have, how many
;panels, how they spin and tumble, their sizes and where they are; a
;darter's ease, its waits between darts and its colours' rate; a blob's
;breath; a triangle's show; a jelly's tentacles, their sway, its rate and
;lag down the chain, and how they fade; and the steps of (rand-d)
(defq +rand_steps (n2d 1000)
	+scout_spin (n2d 0.6) +scout_cruise (n2d 55) +scout_dash (n2d 130) +scout_turn_most (n2d 1.1)
	+scout_pull (n2d 30) +scout_film +double_1/2 +scout_panels +double_4 +scout_ring_film (n2d 0.9)
	+scout_ring_panels +double_6 +scout_ring_spin (n2d 1.5) +scout_ring_scale (n2d 1.25)
	+scout_lamp_scale (n2d 0.09) +scout_lamp_drop (n2d -0.12) +scout_lamp_out (n2d 0.6)
	+scout_lamp_out_neg (n2d -0.6)
	+orb_ease +double_3 +orb_wait_lo (n2d 1300000) +orb_wait_hi (n2d 2400000)
	+orb_light_hues (n2d 1.3) +orb_shell_film (n2d 1.3) +orb_shell_spin (n2d 0.4)
	+orb_shell_tumble +double_1/4 +orb_core_hues (n2d 1.3) +orb_core_scale (n2d 0.42)
	+orb_lights_spin (n2d 2.2) +orb_shard_film (n2d 1.2) +orb_shard_spin (n2d 3.1)
	+orb_shard_tumble (n2d 2.3) +orb_shard_scale +double_1/5 +orb_shard_out (n2d 1.35)
	+orb_shard_x (n2d -0.675) +orb_shard_z (n2d 1.17) +orb_shard_z_neg (n2d -1.17)
	+blob_spin +double_1/4 +blob_cruise (n2d 38) +blob_dash (n2d 90) +blob_turn_most (n2d 0.7)
	+blob_pull (n2d 18) +blob_film (n2d 1.6) +blob_breathe (n2d 0.07) +blob_ball_scale (n2d 0.12)
	+blob_ball_1_x (n2d 0.75) +blob_ball_1_y (n2d 0.15) +blob_ball_2_x (n2d -0.4)
	+blob_ball_2_y +double_1/10 +blob_ball_2_z (n2d 0.65)
	+tri_cruise (n2d 28) +tri_dash (n2d 170) +tri_turn_most (n2d 0.6) +tri_pull (n2d 14)
	+tri_show (n2d 0.7) +tri_film +double_1 +tri_panels +double_7 +tri_ring_film (n2d 0.6)
	+tri_ring_spin (n2d -2.5) +tri_ring_scale (n2d 0.32) +tri_ring_height +double_1/2
	+tri_ring_drop (n2d -0.06) +tri_lamp_scale (n2d 0.08) +tri_lamp_rise (n2d 0.02)
	+tri_nose (n2d 0.8) +tri_wing_x (n2d 0.7) +tri_wing_x_neg (n2d -0.7) +tri_wing_z (n2d -0.4)
	+jelly_ease (n2d 0.6) +jelly_wait_lo (n2d 2500000) +jelly_wait_hi (n2d 4000000)
	+jelly_spin +double_1/5 +jelly_bell_film (n2d 1.5) +jelly_core_hues (n2d 0.8)
	+jelly_core_scale (n2d 0.3) +jelly_core_rise +double_1/4 +jelly_hem_drop (n2d -0.12)
	+jelly_link_drop +double_-1/5 +jelly_sway (n2d 0.35) +jelly_sway_rate (n2d 1.7)
	+jelly_sway_lag (n2d 0.7) +jelly_fade (n2d 0.15) +jelly_film +double_1
	+jelly_link_scale (n2d 0.07) +jelly_hem_out +double_1/2 +jelly_hem_out_neg +double_-1/2
	+jelly_hem_x +double_1/4 +jelly_hem_x_neg +double_-1/4 +jelly_hem_z (n2d 0.433)
	+jelly_hem_z_neg (n2d -0.433)
	+top_ease +double_6 +top_wait_lo (n2d 500000) +top_wait_hi (n2d 1000000) +top_film (n2d 0.8)
	+top_panels +double_3 +top_spin +double_7 +top_tilt +double_1/5)

;and how they fly: the chance of skimming the land, the most and the
;least, and the height over it at which it falls by one; where a craft
;comes in, nearer and further than the most, the most of it to a side,
;how far up with the flight, how long ahead, and how far over the land;
;how long it is gone, after a snatch and not; three quarters of the way;
;where it goes, out, past and to stop, ahead, aside and up; how it weaves
;ahead, by how far and how fast, how low it skims and how it bobs; where
;it stops for its beam: how far ahead of the flight it looks, and how
;much further the higher the eye is; the height of the shore; the spots
;it may stop at, ahead and aside; how far over the land and the most
;over it, and where along the line from the eye it looks over the land;
;how fast it eases to its stop, how long its beam is on, over water and
;land, how fast it paces on over water and for how long, and away; the
;least distance; how its pace follows the flight; how fast it eases to
;its speed, away from a stop and not; how far it looks ahead, the least
;and of its speed; its climb, the least, the most, how hard and how
;quickly; how a craft that shows itself tips; how near is near enough;
;a darter's last wait, its darts, ahead, aside, low and up, its pass and
;its going, and how high it is gone; the colours
;in turn, their middle and swing, and a third and two thirds of a turn;
;a sway's cross way and its rate; a breath's rate; how fast a beam snaps;
;a light's reach and its pool's size; the least power of a beam seen,
;its top and its width; a cow's ground, size, when it shrinks and how
;fast, and its spin and tumble; and the cone and reach a craft is seen in
(defq +skim_most (n2d 0.75) +skim_low (n2d 1.1) +skim_fall (n2d 80)
	+come_nearer (n2d 30) +come_further (n2d 40) +come_aside_most (n2d 1.4) +up_least (n2d -12)
	+up_most (n2d 45) +ahead_least (n2d 9000000) +ahead_most (n2d 15000000)
	+come_over_least +double_5 +come_over_most (n2d 40) +gone_snatch_least (n2d 2000000)
	+gone_snatch_most (n2d 5000000) +gone_least (n2d 3000000) +gone_most (n2d 9000000)
	+along_three_quarters (n2d 0.75) +out_ahead (n2d -300) +out_aside (n2d 420) +out_up (n2d 150)
	+pass_ahead (n2d -80) +pass_aside (n2d 14) +pass_up +double_3 +stop_ahead (n2d 230)
	+stop_aside +double_10 +stop_up (n2d 14) +weave_ahead (n2d 85) +weave_ahead_swing (n2d 25)
	+weave_ahead_rate (n2d 0.4) +weave_aside (n2d 45) +weave_aside_rate (n2d 0.6) +skim_up (n2d 14)
	+skim_bob +double_4 +bob_rate (n2d 0.9) +up_bob +double_7 +stop_secs (n2d 3.5) +stop_low (n2d 24)
	+stop_higher (n2d 60) +shore (n2d -2.5) +spot_ahead (n2d 90) +spot_near (n2d 76)
	+spot_far (n2d 106) +spot_nearest (n2d 72) +spot_furthest (n2d 120) +spot_aside_near (n2d 13)
	+spot_aside_near_neg (n2d -13) +spot_aside_far (n2d 15) +spot_aside_far_neg (n2d -15)
	+spot_aside_wide (n2d 24) +spot_aside_wide_neg (n2d -24) +stop_over (n2d 14)
	+stop_high_most (n2d 46) +sight_near (n2d 0.3) +sight_mid (n2d 0.7) +sight_far (n2d 0.85)
	+sight_end (n2d 0.95) +hover_ease (n2d 2.2) +wet_beam_least (n2d 1000000)
	+wet_beam_most (n2d 1300000) +beam_least (n2d 2400000) +beam_most (n2d 3200000)
	+wet_pace_speed (n2d 15) +wet_pace_least (n2d 6000000) +wet_pace_most (n2d 10000000)
	+away_speed (n2d 45) +d_least (n2d 0.001) +pace_along (n2d 0.9)
	+zoom_ease (n2d 1.8) +speed_ease (n2d 0.8) +reach_least (n2d 30) +reach_secs (n2d 1.8)
	+climb_least (n2d -15) +climb_most (n2d 30) +climb_pull (n2d 0.9) +climb_ease (n2d 1.5)
	+show_rate (n2d 0.7) +near_enough (n2d 3600) +last_wait (n2d 1200000) +dart_ahead_least (n2d 50)
	+dart_ahead_most (n2d 140) +dart_aside_least (n2d -60) +dart_aside_most (n2d 60)
	+dart_low_least +double_9 +dart_low_most (n2d 20) +dart_pass_ahead (n2d -90)
	+dart_pass_aside +double_10 +dart_pass_up +double_4 +dart_gone_ahead (n2d -60)
	+dart_gone_up (n2d 900) +gone_up (n2d 400) +hue_mid (n2d 0.55) +hue_swing (n2d 0.45)
	+third_turn (n2d 2.094) +two_thirds_turn (n2d 4.189) +sway_cross (n2d 0.7)
	+sway_cross_rate (n2d 0.8) +breathe_rate (n2d 2.3) +beam_snap (n2d 14) +light_reach (n2d 0.85)
	+pool_size (n2d 0.4) +beam_least_power (n2d 0.02) +beam_top (n2d 0.12) +beam_wide (n2d 0.33)
	+cow_ground (n2d 1.2) +cow_size (n2d 1.6) +cow_shrink_at (n2d 0.8) +cow_shrink_rate +double_5
	+cow_spin (n2d 1.3) +cow_tumble (n2d 0.6) +seen_cone (n2d 0.9) +seen_reach (n2d 1.4))

(defun rand-d (lo hi)
	; (rand-d lo hi) -> double
	(+ lo (* (- hi lo) (/ (n2d (random 1000)) +rand_steps))))

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
	(def scout :spin +scout_spin :cruise +scout_cruise :dash +scout_dash :turn_most +scout_turn_most
		:pull +scout_pull
		:light (rgb 1.0 0.6 0.25))
	(. scout :add_node (part-node +craft_saucer (rgb 0.55 0.58 0.62) dark +scout_film
		+scout_panels (rgb 1.0 0.55 0.2)))
	(def (defq ring (part-node +craft_ring (rgb 0.3 0.32 0.36) (rgb 0.05 0.15 0.2) +scout_ring_film
		+scout_ring_panels (rgb 1.0 0.55 0.2))) :spin +scout_ring_spin)
	(. scout :add_node (. ring :set_scale +scout_ring_scale +scout_ring_scale +scout_ring_scale))
	(each (lambda ((x z))
			(. scout :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.45 1.0 0.8) +double_0)
				(:set_scale +scout_lamp_scale +scout_lamp_scale +scout_lamp_scale)
				(:set_translation x +scout_lamp_drop z))))
		(list (list +scout_lamp_out +double_0) (list +scout_lamp_out_neg +double_0)
			(list +double_0 +scout_lamp_out) (list +double_0 +scout_lamp_out_neg)))
	scout)

(defun make-orb ()
	; (make-orb) -> scene_node
	;a lattice ball that tumbles, a core in it of every colour in turn, and
	;three shards of light that go round it, each tumbling
	(defq orb (craft-node :orb 7 9000000) dark (rgb 0 0 0) lights (Scene-node))
	(def orb :flies :dart :ease +orb_ease :wait_lo +orb_wait_lo :wait_hi +orb_wait_hi :hops_most 5
		:light_hues +orb_light_hues)
	(def (defq shell (part-node +craft_lattice (rgb 0.32 0.27 0.2) dark +orb_shell_film))
		:spin +orb_shell_spin :tumble +orb_shell_tumble)
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) dark +double_0)) :hues +orb_core_hues)
	(. orb :add_node shell)
	(. orb :add_node (. core :set_scale +orb_core_scale +orb_core_scale +orb_core_scale))
	(def lights :spin +orb_lights_spin)
	(each (lambda ((x z))
			(def (defq shard (part-node +craft_shard (rgb 0.3 0.2 0.3) (rgb 1.9 0.4 1.5) +orb_shard_film))
				:spin +orb_shard_spin :tumble +orb_shard_tumble)
			(. lights :add_node (.-> shard
				(:set_scale +orb_shard_scale +orb_shard_scale +orb_shard_scale)
				(:set_translation x +double_0 z))))
		(list (list +orb_shard_out +double_0) (list +orb_shard_x +orb_shard_z)
			(list +orb_shard_x +orb_shard_z_neg)))
	(. orb :add_node lights)
	orb)

(defun make-blob ()
	; (make-blob) -> scene_node
	;a craft that is grown, not made: it breathes, and its film is thick
	(defq blob (craft-node :blob 9 17000000))
	(def blob :spin +blob_spin :cruise +blob_cruise :dash +blob_dash :turn_most +blob_turn_most
		:pull +blob_pull
		:light (rgb 0.5 1.0 0.3))
	(def (defq body (part-node +craft_blob (rgb 0.2 0.42 0.3) (rgb 0.04 0.22 0.1) +blob_film))
		:breathe +blob_breathe)
	(. blob :add_node body)
	(each (lambda ((x y z))
			(. blob :add_node (.-> (part-node +craft_ball (rgb 0.2 0.2 0.2) (rgb 0.6 1.0 0.3) +double_0)
				(:set_scale +blob_ball_scale +blob_ball_scale +blob_ball_scale)
				(:set_translation x y z))))
		(list (list +blob_ball_1_x +blob_ball_1_y +double_0)
			(list +blob_ball_2_x +blob_ball_2_y +blob_ball_2_z)))
	blob)

(defun make-triangle ()
	; (make-triangle) -> scene_node
	;a triangle of gunmetal, its nose ahead, ridged, a ring of light that
	;turns under its middle and a ball of light at each corner. It glides,
	;tipping slowly one way and the other to show itself, leans hard into
	;its turns, and goes as no aircraft can
	(defq tri (craft-node :triangle 11 25000000))
	(def tri :cruise +tri_cruise :dash +tri_dash :turn_most +tri_turn_most :pull +tri_pull
		:show +tri_show
		:light (rgb 0.35 0.9 1.3))
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
		(list (list +double_0 +tri_nose) (list +tri_wing_x +tri_wing_z) (list +tri_wing_x_neg +tri_wing_z)))
	tri)

(defun make-jelly ()
	; (make-jelly) -> scene_node
	;a jellyfish: a glassy bell with a hem of waves, a core of every colour
	;in turn, and six tentacles, each a chain of shards, a link a node of
	;the one above it, each swaying, so the chain bends and whips
	(defq jelly (craft-node :jelly 9 33000000))
	(def jelly :flies :dart :ease +jelly_ease :wait_lo +jelly_wait_lo :wait_hi +jelly_wait_hi :hops_most 4
		:spin +jelly_spin :light (rgb 1.3 0.35 1.0))
	(. jelly :add_node (part-node +craft_bell (rgb 0.45 0.55 0.7) (rgb 0.06 0.12 0.2) +jelly_bell_film))
	(def (defq core (part-node +craft_ball (rgb 0.1 0.1 0.1) (rgb 0 0 0) +double_0)) :hues +jelly_core_hues)
	(. jelly :add_node (.-> core (:set_scale +jelly_core_scale +jelly_core_scale +jelly_core_scale)
		(:set_translation +double_0 +jelly_core_rise +double_0)))
	(each (lambda ((x z))
			(defq up (. (Scene-node) :set_translation x +jelly_hem_drop z) at up)
			(. jelly :add_node up)
			(each (lambda (k)
					(def (defq link (. (Scene-node) :set_translation +double_0 +jelly_link_drop +double_0))
						:sway (list +jelly_sway +jelly_sway_rate (+ (* (n2d k) +jelly_sway_lag) x z)))
					(. link :add_node (. (part-node +craft_shard (rgb 0.3 0.2 0.3)
							(nums-scale (rgb 1.4 0.35 1.0) (- +double_1 (* +jelly_fade (n2d k)))) +jelly_film)
						:set_scale +jelly_link_scale +jelly_link_scale +jelly_link_scale))
					(. at :add_node link)
					(setq at link))
				'(0 1 2 3 4)))
		(list (list +jelly_hem_out +double_0) (list +jelly_hem_x +jelly_hem_z) (list +jelly_hem_x_neg +jelly_hem_z)
			(list +jelly_hem_out_neg +double_0) (list +jelly_hem_x_neg +jelly_hem_z_neg)
			(list +jelly_hem_x +jelly_hem_z_neg)))
	jelly)

(defun make-top ()
	; (make-top) -> scene_node
	;two cones point to point, spinning fast and leaning as they spin, so
	;they wobble as a top does; it zigzags in quick darts
	(defq top (craft-node :top 6 41000000))
	(def top :flies :dart :ease +top_ease :wait_lo +top_wait_lo :wait_hi +top_wait_hi :hops_most 9
		:light (rgb 0.4 1.1 0.6))
	(def (defq body (part-node +craft_top (rgb 0.7 0.55 0.25) (rgb 0.05 0.03 0) +top_film
			+top_panels (rgb 0.3 1.0 0.6)))
		:spin +top_spin :tilt +top_tilt)
	(. top :add_node body)
	top)

(defun skim? ()
	; (skim? ) -> :t | :nil
	;whether a craft keeps low over the land or up with the flight, by how
	;high the flight is over the land: low, most skim; high, most are up
	;with it, where it is; but always a quarter or more of each, so the land
	;has its craft seen from high and the sky its seen from low
	(defq over (- height (land-at (first place) (second place)))
		chance (max +double_1/4 (min +skim_most (- +skim_low (/ over +skim_fall)))))
	(< (rand-d +double_0 +double_1) chance))

(defun craft-start (craft)
	; (craft-start craft)
	;a craft comes: far ahead in the haze, off to a side, coming this way
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq side (if (= (random 2) 0) +double_1 (neg +double_1))
		far (rand-d (- +come_far +come_nearer) (+ +come_far +come_further))
		off (* side (rand-d (* +come_aside +double_1/2) (* +come_aside +come_aside_most)))
		x (+ px (* fx far) (* (neg fz) off)) z (+ pz (* fz far) (* fx off)))
	;low over the land this time, its light on it, or up with the flight,
	;the more of them up the higher the flight is
	(def craft :live :t :phase :in :side side :hops 0 :next (pii-time) :born (pii-time)
		;a beam on the land, now and then, from one that comes in low to stop
		;and use it, whatever the height of the flight
		:beam (defq beam (and (find (get :kind craft) '(:scout :triangle :blob)) (< (random 6) 5)))
		:skim (or beam (skim?)) :up (rand-d +up_least +up_most)
		:until (+ (pii-time) (n2i (rand-d +ahead_least +ahead_most)))
		:at (doubles x (+ (max height (land-at x z)) (rand-d +come_over_least +come_over_most)) z)
		:head (doubles (neg fx) (neg fz)) :speed (ifn (get :cruise craft) +double_0)
		:lean +double_0 :turn +double_0 :climb +double_0))

(defun craft-gone (craft)
	; (craft-gone craft)
	;gone into the haze, and back after a while, sooner after a snatch
	(def craft :live :nil :wait (+ (pii-time) (n2i (if (get :beam craft)
		(rand-d +gone_snatch_least +gone_snatch_most) (rand-d +gone_least +gone_most))))))

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
		(land-at (+ x (* dx +double_1/4)) (+ z (* dz +double_1/4)))
		(land-at (+ x (* dx +double_1/2)) (+ z (* dz +double_1/2)))
		(land-at (+ x (* dx +along_three_quarters)) (+ z (* dz +along_three_quarters)))
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
		((eql (get :phase craft) :out) (near-flight +out_ahead (* side +out_aside) +out_up))
		((eql (get :phase craft) :pass) (near-flight +pass_ahead (* side +pass_aside) +pass_up))
		;one with a beam makes for a place over the land well ahead, to stop
		((get :beam craft) (near-land +stop_ahead (* side +stop_aside) +stop_up))
		(:t (defq ahead (+ +weave_ahead (* +weave_ahead_swing (sin (* t +weave_ahead_rate))))
				aside (* +weave_aside (sin (+ (* t +weave_aside_rate) side))))
			(if (get :skim craft)
				(near-land ahead aside (+ +skim_up (* +skim_bob (sin (* t +bob_rate)))))
				(near-flight ahead aside (+ (get :up craft) (* +up_bob (sin (* t +bob_rate)))))))))

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
	(bind '(ex ez fx fz) (flight-ahead +stop_secs))
	(defq wet (= (random 8) 0) at :nil
		far (max +double_1 (min +double_2 (+ +double_1
			(/ (- height (land-at (first place) (second place)) +stop_low) +stop_higher)))))
	(some (lambda ((ahead aside))
			(setq ahead (* ahead far) aside (* aside far))
			(defq x (+ ex (* fx ahead) (* (neg fz) aside)) z (+ ez (* fz ahead) (* fx aside)))
			(if (or wet (> (land_height (doubles x z)) +shore)) (setq at (list x z))))
		(list (list +spot_ahead +double_0) (list +spot_near +spot_aside_near)
			(list +spot_near +spot_aside_near_neg) (list +spot_far +spot_aside_far)
			(list +spot_far +spot_aside_far_neg) (list +spot_nearest +double_0) (list +spot_furthest +double_0)
			(list +spot_ahead +spot_aside_wide) (list +spot_ahead +spot_aside_wide_neg)))
	(when at
		(bind '(x z) at)
		(defq water (<= (land_height (doubles x z)) +shore))
		(defq low (+ (land-at x z) +stop_over) y low)
		(each (lambda (f)
				(defq lx (+ ex (* (- x ex) f)) lz (+ ez (* (- z ez) f))
					over (+ (land-at lx lz) +double_3))
				;the line from the eye to the craft clears the land here
				(setq y (max y (+ height (/ (- over height) f)))))
			(list +sight_near +double_1/2 +sight_mid +sight_far +sight_end))
		(list x (min y (+ low +stop_high_most)) z water)))

(defun hover-fly (craft dt)
	; (hover-fly craft dt)
	;stopping at its place over the land, and still there: eased to it, its
	;way kept, its turn and lean let go; there, its beam on, snapped, for a
	;second or two, then off, and away with its cow, or on as before if it
	;had none
	(bind '(cx cy cz) (get :at craft))
	(bind '(gx gy gz) (get :stop craft))
	(defq k (min +double_1 (* dt +hover_ease)) now (pii-time)
		x (+ cx (* (- gx cx) k)) y (+ cy (* (- gy cy) k)) z (+ cz (* (- gz cz) k)))
	(def craft :at (doubles x y z) :speed +double_0 :climb +double_0
		:turn (* (get :turn craft) (- +double_1 k)) :lean (* (get :lean craft) (- +double_1 k)))
	(case (get :phase craft)
		(:stopping
			(when (< (+ (* (- gx x) (- gx x)) (* (- gy y) (- gy y)) (* (- gz z) (- gz z))) +double_6)
				;over land a cow comes up the beam; over water nothing does, and
				;it gives up the sooner
				(def craft :phase :beam :beamed now
					:until (+ now (n2i (if (get :wet craft) (rand-d +wet_beam_least +wet_beam_most)
						(rand-d +beam_least +beam_most)))))))
		(:beam
			;a cow had, it is away at speed; nothing had, over the water, it
			;flies on as it would have, ahead a while, then past and away
			(when (> now (get :until craft))
				(if (get :wet craft)
					(def craft :phase :pace :beam :nil :speed +wet_pace_speed
						:until (+ now (n2i (rand-d +wet_pace_least +wet_pace_most))))
					(def craft :phase :out :speed +away_speed))))))

(defun swoop-move (craft dt)
	; (swoop-move craft dt)
	;in out of the haze, then ahead of the flight, weaving, at its pace,
	;till its time is up; then round and at the flight, past it close and
	;away, faster. It turns no faster than it may, eases into a turn, turns
	;the shorter way round to what is behind it, and leans into a turn, and
	;one that shows itself, :show, tips slowly one way and the other too
	(bind '(cx cy cz) (get :at craft))
	(bind '(hx hz) (get :head craft))
	(defq now (pii-time) phase (get :phase craft) t (/ (n2d (- now (get :born craft))) +second)
		goal (swoop-goal craft t))
	(bind '(gx gy gz) goal)
	(defq dx (- gx cx) dz (- gz cz) d (+ (sqrt (+ (* dx dx) (* dz dz))) +d_least)
		cross (/ (- (* hx dz) (* hz dx)) d) along (+ (* hx dx) (* hz dz))
		most (get :turn_most craft)
		want (if (< along +double_0) (if (< cross +double_0) most (neg most))
			(max (neg most) (min most (* (neg cross) +double_3))))
		turn (+ (get :turn craft) (* (- want (get :turn craft)) (min +double_1 (* dt +double_2))))
		a (* turn dt) ca (cos a) sa (sin a)
		nx (+ (* hx ca) (* hz sa)) nz (- (* hz ca) (* hx sa))
		pace (case phase
			(:in (get :cruise craft))
			(:pace (max +double_10 (min (get :cruise craft) (+ +speed (* along +pace_along)))))
			(:t (get :dash craft)))
		;away from a stop it zooms, quicker to its dash
		speed (+ (get :speed craft) (* (- pace (get :speed craft)) (min +double_1 (* dt (if (get :beam craft) +zoom_ease +speed_ease)))))
		;what it makes for, but over the land it will be over in a while, so
		;it climbs to clear a hill in a curve before it is there, as the
		;flight does
		reach (max +reach_least (* speed +reach_secs))
		over (max gy (+ (land-along cx cz (+ cx (* nx reach)) (+ cz (* nz reach))) +craft_floor))
		climb (+ (get :climb craft) (* (- (max +climb_least (min +climb_most (* (- over cy) +climb_pull))) (get :climb craft))
			(min +double_1 (* dt +climb_ease))))
		x (+ cx (* nx speed dt)) z (+ cz (* nz speed dt))
		y (max (+ cy (* climb dt)) (+ (land-at x z) +craft_last)))
	(def craft :at (doubles x y z) :head (doubles nx nz) :turn turn :speed speed :climb climb
		:lean (max +double_-2 (min +double_2 (+ (neg (/ (* speed turn) (get :pull craft)))
			(* (ifn (get :show craft) +double_0) (sin (* t +show_rate)))))))
	;what it is about next
	(bind '(fx fz) way)
	(defq rx (- x (first place)) rz (- z (second place)))
	(case phase
		(:in (when (< (+ (* dx dx) (* dz dz) (* (- gy y) (- gy y))) +near_enough)
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
		(def craft :next (+ now (n2i (if (= hops most) +last_wait (rand-d (get :wait_lo craft) (get :wait_hi craft)))))
			:hops (inc hops)
			:goal (cond
				;each hop low over the land or high in the sky
				((< hops most)
					(if (skim?)
						(near-land (rand-d +dart_ahead_least +dart_ahead_most) (rand-d +dart_aside_least +dart_aside_most)
							(rand-d +dart_low_least +dart_low_most))
						(near-flight (rand-d +dart_ahead_least +dart_ahead_most) (rand-d +dart_aside_least +dart_aside_most)
							(rand-d +up_least +up_most))))
				((= hops most) (near-flight +dart_pass_ahead (* (get :side craft) +dart_pass_aside) +dart_pass_up))
				((near-flight +dart_gone_ahead +double_0 +dart_gone_up)))))
	(bind '(cx cy cz) (get :at craft))
	(bind '(gx gy gz) (get :goal craft))
	(defq k (min +double_1 (* dt (if (>= hops most) +double_3 (get :ease craft))))
		;over the land between here and there, risen to as it goes
		over (max gy (+ (land-along cx cz gx gz) +craft_floor))
		x (+ cx (* (- gx cx) k)) z (+ cz (* (- gz cz) k))
		y (max (+ cy (* (- over cy) k)) (+ (land-at x z) +craft_last)))
	(def craft :at (doubles x y z))
	(if (> (- y height) +gone_up) (craft-gone craft)))

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
	(doubles (+ +hue_mid (* +hue_swing (cos at)))
		(+ +hue_mid (* +hue_swing (cos (+ at +third_turn))))
		(+ +hue_mid (* +hue_swing (cos (+ at +two_thirds_turn))))))

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
						(* amount +sway_cross (cos (* at +sway_cross_rate)))))
				((or (def? :spin node) (def? :tumble node) (def? :tilt node))
					(. node :set_rotation (+ (ifn (get :tilt node) +double_0) (* seconds (ifn (get :tumble node) +double_0)))
						(* seconds (ifn (get :spin node) +double_0)) +double_0)))
			(when (def? :breathe node)
				(defq b (+ +double_1 (* (get :breathe node) (sin (* seconds +breathe_rate)))))
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
		(min +double_1 (* dt +beam_snap))))))

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
				(push out (list cn (doubles x y z (* (get :size craft) +light_reach)))
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
				(setq pool (doubles x z (* (get :size craft) +pool_size) power))))
		crafts)
	(push out (list 'beam pool)))

(defun fleet-beams (view eye seconds)
	; (fleet-beams view eye seconds) -> draws
	;the beams of the craft that have them on: a column from under the craft
	;down to the land, as wide as a third of the craft, white,
	;not the colour of the craft's own light; drawn after all else, added
	(reduce (lambda (draws craft)
			(when (and (get :live craft) (> (defq power (ifn (get :beam_power craft) +double_0)) +beam_least_power))
				;as far down and as bright as it is on
				(bind '(x y z) (get :at craft))
				(defq top (- y (* (get :size craft) +beam_top)) long (* (- top (land-at x z)) power))
				(when (> long +double_1)
					(defq wide (* (get :size craft) +beam_wide))
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
					ground (+ (land-at x z) +cow_ground) top (- y (* (get :size craft) +double_1/4))
					s (* +cow_size (- +double_1 (* +double_1/2 (max +double_0 (- t +cow_shrink_at)) +cow_shrink_rate))))
				(push draws (list (+ +mesh_craft +craft_cow)
					(list (list 'model (mat4x4-mul (mat4x4-translate x (+ ground (* (- top ground) t)) z)
							(mat4x4-mul (mat4x4-roty (* seconds +cow_spin)) (mat4x4-mul (mat4x4-rotx (* t +cow_tumble))
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
		(<= (- (sqrt (max +double_0 (- (+ (* dx dx) (* dy dy) (* dz dz)) (* along along)))) (* along +seen_cone))
			(* reach +seen_reach))))

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

;module, the crafts and how they fly and are drawn: their numbers,
;prebound, go with it
(export-symbols '(make-scout make-orb make-blob make-triangle make-jelly make-top
	fleet-fly fleet-lights fleet-beams fleet-draws))
(env-pop)
