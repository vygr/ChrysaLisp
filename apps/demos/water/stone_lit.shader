;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a stone, a pixel shader. It goes with stone_vertex.shader.
;
; Sandstone is layered: thin beds along the stone's own up, so they tilt
; as it does, each stone from a bed of its own, so which shows on top is
; its own, its top eaten down in patches to the beds under, each bed its own shade of buff to
; rust, a little wavy. Basalt is near black, a grey speckle in it. The
; sand has settled round the foot of each, and on sandstone facing up. Both are made rough close to, as the reef is, and
; lit as it is: the water round it as much as the sky reaches where it
; lies, the sun through the water and its caustics on what faces up,
; the diver's lamp, and the water between hiding it with distance.
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
(defvarying bedding :float)

(defconst sand_pale (vec3 0.8 0.6 0.4))
(defconst sand_rust (vec3 0.6 0.33 0.2))
(defconst sand_dark (vec3 0.42 0.3 0.22))
(defconst basalt_dark (vec3 0.1 0.1 0.11))
(defconst sand_colour (vec3 0.78 0.7 0.52))
(defconst basalt_grey (vec3 0.3 0.29 0.28))

(defun main :vec4 ((frag :vec2))
	(defq to (- eye place) d (length to) l (/ to d) n (normalize facing)
		sun (:x lit) sky (:y lit) basalt (:x grain) floor_at (:y grain)
		depth (clamp (/ (- surface (:y place)) 100.0) 0.0 1.0)
		;the beds of sandstone, which one by how high in its beds, along
		;its own up, a little wavy, and the shade of each its own
		strata (+ (* bedding 9.0) (* 1.2 (noise3 (* place (vec3 0.6 0.2 0.6))))
			;more on what faces up, patches of the beds under showing through
			(* 3.0 (ease 0.55 0.95 (:y n)) (noise3 (* place 0.7))))
		layer (floor strata) seam (fract strata)
		shade (fract (* (sin (* layer 12.9898)) 43758.5453))
		;each bed its shade, a dark line where one bed meets the next, and
		;fine laminations in it
		layered (* (mix (mix sand_rust sand_pale shade) sand_dark (* 0.4 (ease 0.8 1.0 shade)))
			(+ 0.6 (* 0.4 (ease 0.0 0.12 seam) (- 1.0 (ease 0.9 1.0 seam))))
			(+ 0.9 (* 0.1 (sin (+ (* bedding 60.0) (* 4.0 (noise3 (* place 0.8))))))))
		;basalt fine grained, a faint grey grain and the dark holes of gas
		speckle (* 0.35 (+ (noise3 (* place 31.0)) (* 0.5 (noise3 (* place 67.0)))))
		holes (ease 0.68 0.78 (noise3 (* place 13.0)))
		;the sand settled round its foot, and on what faces up of sandstone
		settled (max (* 0.85 (- 1.0 (ease 0.0 0.12 (- (:y place) floor_at))) (ease -0.15 0.0 (- (:y place) floor_at)))
			(* (- 1.0 basalt) 0.35 (ease 0.8 0.97 (:y n)) (noise3 (* place 1.5))))
		base (mix (mix layered (* (mix basalt_dark basalt_grey speckle) (- 1.0 (* 0.6 holes))) basalt) sand_colour settled))
	;rough close to, rougher the basalt
	(defq close (- 1.0 (ease 3.0 12.0 far)))
	(when (< far 70.0)
		(setq n (bumped place n 0.0 (* (mix 0.6 0.9 basalt) (- 1.0 (ease 35.0 70.0 far))) close)))
	(setq base (* base (+ 1.0 (* close (- (* 0.3 (+ (noise3 (* place 14.0)) (* 0.5 (noise3 (* place 33.0))))) 0.22)))))
	(defq down (* sun (exp (* -2.2 depth)))
		diffuse (* (max (dot n sun_way) 0.0) down)
		caus (* (caustic (:xz place) time (clamp (+ (/ far 70.0) (* 0.6 depth)) 0.0 0.85)) down (max (:y n) 0.0))
		fill (mix 0.55 0.12 (ease 0.0 0.35 depth))
		round (+ (* (water_colour (:y place) 1.0) (+ fill (* 0.8 sky)) 1.7)
			(* water_shallow (* 0.25 (- 1.0 depth) (max (- (:y n)) 0.0))))
		cone (ease 0.35 0.87 (dot (- l) forward))
		lamped (* lamp cone (max (dot n l) 0.0) (/ 1.0 (+ 1.0 (* 0.004 d d))))
		light (+ round (* sun_light (+ (* 1.3 diffuse) (* 1.8 caus))))
		col (* base (+ light (* (vec3 1.0 0.95 0.85) lamped))))
	(defq k (seen_through to murk))
	(vec4 (+ (mix (water_seen eye (- l) time murk open) col k) (lamp_glow (- l) forward lamp (- 1.0 k))) 1.0))
