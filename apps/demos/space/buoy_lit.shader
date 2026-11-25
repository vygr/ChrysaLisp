;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a marker buoy, a pixel shader. It goes with the rocks'
; rock_vertex.shader, the buoy being a mesh of the farm's as they are.
;
; Metal shows the world round it: the sky, the nebula's colours, is
; seen in it where the way from the eye bounces off it, and the sun as
; a hard glint. Its mast bare polished metal, all reflection; its float
; glossy white paint, lit by the sun and the nebula, the sky in it the
; more at its edges, as paint is seen glancing; its fender ring dark and
; matt, rubber. Its lamp on top lit from within, of its light's colour,
; as bright as it is now, a flash or the dim glow between, the hotter
; toward its middle as it faces the eye.
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
;the lamp's colour, as bright as it is now
(definput lamp :vec3)

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
	(defq n (normalize facing) to_eye (normalize (- eye place)) face (max (dot n to_eye) 0.0))
	;the lamp, hot at its middle, its colour deeper to its edge
	(if (> (:y local) 0.56)
		(return (vec4 (min (+ (* lamp (+ 0.7 (* 1.3 face face))) (* (vec3 0.25) face face (:x lamp))) (vec3 1.0)) 1.0)))
	(defq bounce (reflect (- to_eye) n)
		;the world seen in it, the sky and the sun's glint
		seen (+ (* (sky_in bounce) 2.2) (* sun_light (* 5.0 (pow (max (dot bounce sun) 0.0) 120.0))))
		;how much a glancing look sees, more at its edges
		glance (+ 0.06 (* 0.94 (pow (- 1.0 face) 5.0)))
		light (+ (* sun_light (max (dot n sun) 0.0)) (* pillars_light (max (dot n pillars_at) 0.0)) (vec3 0.03 0.03 0.04))
		colour (vec3 0.0))
	(cond
		((< (abs (+ (:y local) 0.3)) 0.1)
			;the fender, matt rubber
			(setq colour (* (vec3 0.08 0.08 0.09) light)))
		((> (:y local) 0.1)
			;the mast, polished metal, the world in it, tinted
			(setq colour (+ (* (vec3 0.75 0.77 0.8) seen) (* (vec3 0.05) light))))
		(:t ;the float, glossy white paint
			(setq colour (+ (* (vec3 0.86 0.86 0.83) light) (* seen glance)))))
	(vec4 (min colour (vec3 1.0)) 1.0))
