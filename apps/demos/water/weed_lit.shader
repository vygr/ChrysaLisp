;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a clump of weed, a pixel shader. It goes with weed_vertex.shader.
;
; Kelp is olive to brown, sea grass green, both darker at the foot and
; paler to the tips. A blade is thin, seen from either side, so its
; normal is turned to the eye, and the sun comes through it as well as
; off it, a glow when it is between the eye and the light. Lit as the
; reef is: the water round it as much as the sky reaches where it
; grows, the sun through the water and its caustics, the diver's lamp,
; and the water between hiding it with distance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/look.shader")

;where the eye is and the way it looks, the time, seconds, the ripples
;move by, how bright the lamp is, how fast the water hides things, and
;how much of the water's light reaches the eye
(definput eye :vec3)
(definput forward :vec3)
(definput time :float)
(definput lamp :float)
(definput murk :float)
(definput open :float)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying lit :vec2)
(defvarying grain :vec2)

(defconst kelp_foot (vec3 0.22 0.18 0.06))
(defconst kelp_tip (vec3 0.55 0.5 0.18))
(defconst grass_foot (vec3 0.08 0.2 0.07))
(defconst grass_tip (vec3 0.32 0.55 0.2))

(defun main :vec4 ((frag :vec2))
	(defq to (- eye place) d (length to) l (/ to d) n (normalize facing)
		sun (:x lit) sky (:y lit) grass (:x grain) up_blade (:y grain)
		depth (clamp (/ (- surface (:y place)) 100.0) 0.0 1.0)
		;streaks along the blade, and its colour foot to tip
		streak (+ 0.85 (* 0.3 (noise3 (* place (vec3 9.0 1.5 9.0)))))
		base (* (mix (mix kelp_foot kelp_tip up_blade) (mix grass_foot grass_tip up_blade) grass) streak))
	;seen from either side
	(if (< (dot n l) 0.0) (setq n (- n)))
	(defq down (* sun (exp (* -2.2 depth)))
		diffuse (* (max (dot n sun_way) 0.0) down)
		;the sun through the blade, the more the more it is behind it
		through (* down 0.6 (ease 0.0 1.0 (- (dot (- l) sun_way))))
		caus (* (caustic (:xz place) time (clamp (+ (/ far 70.0) (* 0.6 depth)) 0.0 0.85)) down (* 0.5 (abs (:y n))))
		fill (mix 0.55 0.12 (ease 0.0 0.35 depth))
		round (* (water_colour (:y place) 1.0) (+ fill (* 0.8 sky)) 1.7)
		cone (ease 0.35 0.87 (dot (- l) forward))
		lamped (* lamp cone (max (dot n l) 0.0) (/ 1.0 (+ 1.0 (* 0.004 d d))))
		light (+ round (* sun_light (+ (* 1.2 diffuse) through (* 1.4 caus))))
		col (* base (+ light (* (vec3 1.0 0.95 0.85) lamped))))
	(defq k (seen_through to murk))
	(vec4 (+ (mix (water_seen eye (- l) time murk open) col k) (lamp_glow (- l) forward lamp (- 1.0 k))) 1.0))
