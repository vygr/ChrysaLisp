;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a missile, a pixel shader. It goes with the rocks'
; rock_vertex.shader, the missile being a mesh of the farm's as they
; are, up its length.
;
; Painted metal, as the buoy is: light grey, the sky seen in it the more
; at its edges and the sun a hard glint; a red band behind its nose; its
; fins dark; and its nozzle at the tail hot, white to orange, as bright
; as the burn.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/sphere.shader")

;the nebula of all of the sky, what is seen in it
(deftexture nebula)

(definput sun :vec3)
(definput eye :vec3)
(definput sun_light :vec3)
(definput pillars_at :vec3)
(definput pillars_light :vec3)
(definput nebula_pixels :vec2)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying local :vec3)
(defvarying far :float)

;the sky in a way, smooth: the four pixels of the map round it mixed
(defun sky_in :vec3 ((d :vec3))
	(defq uv (sphere_uv d) p (- (* uv nebula_pixels) (vec2 0.5)) f (fract p) step (/ (vec2 1.0) nebula_pixels)
		a (* (+ (floor p) (vec2 0.5)) step)
		u0 (fract (:x a)) u1 (fract (+ (:x a) (:x step)))
		v0 (clamp (:y a) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step))))
		v1 (clamp (+ (:y a) (:y step)) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step)))))
	(mix (mix (:xyz (texture nebula (vec2 u0 v0))) (:xyz (texture nebula (vec2 u1 v0))) (:x f))
		(mix (:xyz (texture nebula (vec2 u0 v1))) (:xyz (texture nebula (vec2 u1 v1))) (:x f)) (:y f)))

(defun main :vec4 ((frag :vec2))
	(defq n (normalize facing) to_eye (normalize (- eye place)) face (max (dot n to_eye) 0.0)
		y (:y local) r (length (:xz local)))
	;the nozzle, hot
	(if (and (< y -0.7) (< r 0.15))
		(return (vec4 (mix (vec3 1.0 0.45 0.1) (vec3 1.0 0.95 0.8) (* face face)) 1.0)))
	(defq bounce (reflect (- to_eye) n)
		seen (+ (* (sky_in bounce) 2.2) (* sun_light (* 5.0 (pow (max (dot bounce sun) 0.0) 120.0))))
		glance (+ 0.08 (* 0.92 (pow (- 1.0 face) 5.0)))
		light (+ (* sun_light (max (dot n sun) 0.0)) (* pillars_light (max (dot n pillars_at) 0.0)) (vec3 0.05 0.05 0.06))
		paint (vec3 0.8 0.8 0.82))
	(cond
		((> r 0.17) (setq paint (vec3 0.12 0.12 0.13)))
		((< (abs (- y 0.28)) 0.05) (setq paint (vec3 0.75 0.08 0.06))))
	(vec4 (min (+ (* paint light) (* seen glance)) (vec3 1.0)) 1.0))
