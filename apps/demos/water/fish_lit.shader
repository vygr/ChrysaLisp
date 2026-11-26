;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a fish, a pixel shader. It goes with fish_vertex.shader.
;
; Dark on the back and pale on the belly, as fish are, so seen from
; below it is lost in the light and from above in the deep: silver
; fish blue grey over silver, reef fish yellow banded black, a shark
; slate over white. Its fins thin, seen from either side. Lit by the
; water round it, as open as the water is where the eye is, the sun
; from above, a glint of it off the scales, and the diver's lamp; the
; water between hiding it with distance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/look.shader")

(definput eye :vec3)
(definput forward :vec3)
(definput time :float)
(definput lamp :float)
(definput murk :float)
(definput open :float)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying local :vec3)
(defvarying kind :float)

(defconst silver_back (vec3 0.18 0.28 0.36))
(defconst silver_belly (vec3 0.7 0.74 0.78))
(defconst reef_back (vec3 0.55 0.42 0.08))
(defconst reef_belly (vec3 0.95 0.82 0.25))
(defconst shark_back (vec3 0.24 0.27 0.31))
(defconst shark_belly (vec3 0.78 0.8 0.8))

(defun main :vec4 ((frag :vec2))
	(defq to (- eye place) d (length to) l (/ to d) n (normalize facing)
		depth (clamp (/ (- surface (:y place)) 100.0) 0.0 1.0)
		;the back to the belly, by how high on the fish
		belly (ease 0.03 -0.04 (:y local))
		reef (- 1.0 (abs (- kind 1.0))) shark (clamp (- kind 1.0) 0.0 1.0) silver (clamp (- 1.0 kind) 0.0 1.0)
		;a reef fish's bands
		band (* reef (ease 0.4 0.5 (fract (* (+ (:x local) 0.3) 4.0))) (- 1.0 (ease 0.62 0.72 (fract (* (+ (:x local) 0.3) 4.0)))))
		base (* (+ (* silver (mix silver_back silver_belly belly)) (* reef (mix reef_back reef_belly belly))
			(* shark (mix shark_back shark_belly belly))) (- 1.0 (* 0.8 band))))
	(if (< (dot n l) 0.0) (setq n (- n)))
	(defq down (* open (exp (* -2.2 depth)))
		diffuse (* (max (dot n sun_way) 0.0) down)
		glint (* (+ (* silver 0.9) (* reef 0.4) (* shark 0.15)) down
			(pow (max (dot (- (* n (* 2.0 (dot n l))) l) sun_way) 0.0) 24.0))
		fill (mix 0.55 0.12 (ease 0.0 0.35 depth))
		round (* (water_colour (:y place) 1.0) (+ fill (* 0.8 open)) 1.7)
		cone (ease 0.35 0.87 (dot (- l) forward))
		lamped (* lamp cone (max (dot n l) 0.0) (/ 1.0 (+ 1.0 (* 0.004 d d))))
		col (+ (* base (+ round (* sun_light (* 1.2 diffuse)) (* (vec3 1.0 0.95 0.85) lamped))) (* sun_light glint)))
	(defq k (seen_through to murk))
	(vec4 (+ (mix (water_seen eye (- l) time murk open) col k) (lamp_glow (- l) forward lamp (- 1.0 k))) 1.0))
