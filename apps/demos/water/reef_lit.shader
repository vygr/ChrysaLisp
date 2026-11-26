;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, the reef, a pixel shader. It goes with reef_vertex.shader.
;
; Sand where the rock faces up in the open, banded rock elsewhere. Lit
; by the water round it, as much as the sky reaches the place, and by
; the sun through the water, less the deeper, its caustics rippling
; over what faces up, both as the shadows of the rock let them, baked
; into each vertex by its chunk, chunk.inc. The diver's lamp lights
; a cone ahead, all there is in the caves. The water between hides it
; all the further it is. What is over the surface is seen through it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/look.shader")

;where the eye is and the way it looks, the time, seconds, the ripples
;move by, how bright the lamp is, how fast the water hides things, and
;how much of the water's light reaches the eye, little in the caves
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

(defconst rock_dark (vec3 0.3 0.25 0.21))
(defconst rock_pale (vec3 0.52 0.44 0.35))
(defconst sand_colour (vec3 0.78 0.7 0.52))

(defun main :vec4 ((frag :vec2))
	(defq to (- eye place) d (length to) l (/ to d) n (normalize facing))
	;what is over the surface is hidden by it, a mirror from under it, but
	;in the window of the sky straight up, where it is dark against it
	(when (> (:y place) surface)
		(return (vec4 (* (water_seen eye (- l) time murk open) (- 1.0 (* 0.7 (ease 0.62 0.78 (- (:y l))))))
			1.0)))
	(defq sun (:x lit) sky (:y lit)
		depth (clamp (/ (- surface (:y place)) 100.0) 0.0 1.0)
		band (noise3 (* place (vec3 0.25 1.6 0.25)))
		;how high over the seabed under it: sand on the low ground, open to
		;the sky and facing up, rock as the reef rises, rougher the higher,
		;and the rock of the caves, under the bed, rough too
		high (- (:y place) (bed (:xz place)))
		sand (* (ease 0.6 0.85 (:y n)) (ease 0.3 0.7 sky) (- 1.0 (ease 1.5 7.0 high)))
		rough (+ 0.3 (* 0.7 (ease 0.0 12.0 (abs high))))
		base (mix (mix rock_dark rock_pale band) sand_colour sand))
	;the little bumps near the eye, fading out further away, gentle on the
	;sand and rough on the rock, finer still right before the eye
	(defq close (- 1.0 (ease 3.0 12.0 far)))
	(when (< far 40.0)
		(setq n (bumped place n sand (* (mix (* 0.85 rough) 0.3 sand) (- 1.0 (ease 15.0 40.0 far))) close)))
	;and grit in its colour close up
	(setq base (* base (+ 1.0 (* close (- (* 0.3 (+ (noise3 (* place 14.0)) (* 0.5 (noise3 (* place 33.0))))) 0.22)))))
	;the sun through the water, and its caustics on what faces up
	(defq down (* sun (exp (* -2.2 depth)))
		diffuse (* (max (dot n sun_way) 0.0) down)
		caus (* (caustic (:xz place) time (clamp (+ (/ far 70.0) (* 0.6 depth)) 0.0 0.85)) down (max (:y n) 0.0))
		;the water's own light, as much as the sky reaches here, and in the
		;shallows light from every side, so a rock that breaks the surface is
		;seen clearly under it, little deep down and in the caves; and the
		;light the sand and the water throw back up, under an overhang too
		fill (mix 0.55 0.12 (ease 0.0 0.35 depth))
		round (+ (* (water_colour (:y place) 1.0) (+ fill (* 0.8 sky)) 1.7)
			(* water_shallow (* 0.25 (- 1.0 depth) (max (- (:y n)) 0.0))))
		;the lamp, a wide soft cone ahead, fading with distance
		cone (ease 0.35 0.87 (dot (- l) forward))
		lamped (* lamp cone (max (dot n l) 0.0) (/ 1.0 (+ 1.0 (* 0.004 d d))))
		light (+ round (* sun_light (+ (* 1.3 diffuse) (* 1.8 caus))))
		col (* base (+ light (* (vec3 1.0 0.95 0.85) lamped))))
	;the water between, as lit as the eye is, and the lamp's glow in it
	(defq k (seen_through to murk))
	(vec4 (+ (mix (water_seen eye (- l) time murk open) col k) (lamp_glow (- l) forward lamp (- 1.0 k))) 1.0))
