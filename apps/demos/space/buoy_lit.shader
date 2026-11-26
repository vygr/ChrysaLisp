;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a marker buoy, a pixel shader. It goes with the rocks'
; rock_vertex.shader, the buoy being a mesh of the farm's as they are.
;
; Metal shows the world round it: the sky as the sky shader has it, the
; nebula, the Pillars over it and the stars, is seen in it where the way
; from the eye bounces off it, and the sun as a hard glint. Its mast
; bare polished metal, all reflection; its float polished metal too, a
; little warmer and lit by the sun and the Pillars as well, the sky in
; it the more at its edges; its fender ring dark and matt, rubber. Its
; lamp on top lit from within, of its light's colour, as bright as it
; is now, a flash or the dim glow between, the hotter toward its middle
; as it faces the eye. Lit as a rock is by what is near: a missile's or
; a blast's light, else the nearest other buoy's lamp, and the ship's
; ping, a glint of each in its metal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")
(import "lib/gpu/shaders/sphere.shader")

;the sky's pictures, as the sky shader has them: the Pillars, the nebula
;of all of the sky, and its stars
(deftexture pillars)
(deftexture nebula)
(deftexture stars)

(definput sun :vec3)
(definput eye :vec3)
(definput sun_light :vec3)
(definput pillars_light :vec3)
;the way to the middle of the Pillars, its across and up, and how far it
;reaches across and up, at a distance of 1, as the sky shader has them
(definput pillars_at :vec3)
(definput pillars_across :vec3)
(definput pillars_up :vec3)
(definput pillars_size :vec2)
;the sizes of the three pictures, pixels
(definput pillars_pixels :vec2)
(definput nebula_pixels :vec2)
(definput stars_pixels :vec2)
;the lamp's colour, as bright as it is now
(definput lamp :vec3)
;the ship's ping, where and as bright as it is now
(definput ping_at :vec3)
(definput ping :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying local :vec3)
(defvarying far :float)
;the light near it, a blast's or another buoy's, rock-buoy
(defvarying buoy_at :vec3)
(defvarying buoy_light :vec3)

;how far a near light reaches, as the rocks have it
(defconst nav_reach 80.0)

;a near light on a face, as the rocks have it, wrapping a little round
(defun near_light :vec3 ((p :vec3) (n :vec3) (at :vec3) (colour :vec3))
	(defq v (- at p) d (length v) turned (dot n (/ v d)))
	(* colour (/ (+ (max turned 0.0) (* 0.35 (max (/ (+ turned 0.6) 1.6) 0.0)))
		(+ 1.0 (* (/ d nav_reach) (/ d nav_reach))))))

;a near light seen in polished metal, a hot glint where the bounce goes
;toward it, fading as it is further
(defun glint :vec3 ((p :vec3) (bounce :vec3) (at :vec3) (colour :vec3))
	(defq v (- at p) d (length v))
	(/ (* colour (* 3.0 (pow (max (dot bounce (/ v d)) 0.0) 60.0)))
		(+ 1.0 (* (/ d nav_reach) (/ d nav_reach)))))

;a picture's colour at a place, smooth, the four pixels round it mixed,
;as the sky shader reads them: down it stops at the edge, across too for
;the Pillars, but the nebula and the stars go round, as the sky does
(defun smooth :vec3 ((which :int) (uv :vec2) (pixels :vec2))
	(defq p (- (* uv pixels) (vec2 0.5)) f (fract p) step (/ (vec2 1.0) pixels)
		a (* (+ (floor p) (vec2 0.5)) step)
		u0 (fract (:x a)) u1 (fract (+ (:x a) (:x step))))
	(when (= which 0)
		(setq u0 (clamp (:x a) (* 0.5 (:x step)) (- 1.0 (* 0.5 (:x step))))
			u1 (clamp (+ (:x a) (:x step)) (* 0.5 (:x step)) (- 1.0 (* 0.5 (:x step))))))
	(defq
		v0 (clamp (:y a) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step))))
		v1 (clamp (+ (:y a) (:y step)) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step))))
		c00 (vec3 0.0) c10 (vec3 0.0) c01 (vec3 0.0) c11 (vec3 0.0))
	(cond
		((= which 0)
			(setq c00 (:xyz (texture pillars (vec2 u0 v0))) c10 (:xyz (texture pillars (vec2 u1 v0)))
				c01 (:xyz (texture pillars (vec2 u0 v1))) c11 (:xyz (texture pillars (vec2 u1 v1)))))
		((= which 1)
			(setq c00 (:xyz (texture nebula (vec2 u0 v0))) c10 (:xyz (texture nebula (vec2 u1 v0)))
				c01 (:xyz (texture nebula (vec2 u0 v1))) c11 (:xyz (texture nebula (vec2 u1 v1)))))
		(:t (setq c00 (:xyz (texture stars (vec2 u0 v0))) c10 (:xyz (texture stars (vec2 u1 v0)))
				c01 (:xyz (texture stars (vec2 u0 v1))) c11 (:xyz (texture stars (vec2 u1 v1))))))
	(mix (mix c00 c10 (:x f)) (mix c01 c11 (:x f)) (:y f)))

;the sky a way, as the sky shader makes it: the nebula, the Pillars over
;it where the way is toward them, their edge uneven, and the stars, few
;in the picture, which has its own
(defun sky_in :vec3 ((d :vec3))
	(defq at (sphere_uv d) colour (smooth 1 at nebula_pixels) seen 0.0 toward (dot d pillars_at))
	(when (> toward 0.5)
		(defq x (/ (dot d pillars_across) (* toward (:x pillars_size)))
			y (/ (dot d pillars_up) (* toward (:y pillars_size)))
			out (+ (max (abs x) (abs y)) (* 0.16 (- (noise3 (* d 7.0)) 0.5))))
		(setq seen (- 1.0 (ease 0.72 0.97 out)))
		(if (> seen 0.0)
			(setq colour (mix colour (smooth 0 (vec2 (+ 0.5 (* 0.5 x)) (- 0.5 (* 0.5 y))) pillars_pixels) seen))))
	(+ colour (* (smooth 2 at stars_pixels) (- 1.0 (* 0.85 seen)))))

(defun main :vec4 ((frag :vec2))
	(defq n (normalize facing) to_eye (normalize (- eye place)) face (max (dot n to_eye) 0.0))
	;the lamp, hot at its middle, its colour deeper to its edge
	(if (> (:y local) 0.56)
		(return (vec4 (min (+ (* lamp (+ 0.7 (* 1.3 face face))) (* (vec3 0.25) face face (:x lamp))) (vec3 1.0)) 1.0)))
	(defq bounce (reflect (- to_eye) n)
		;the world seen in it, the sky and the sun's glint
		seen (+ (* (sky_in bounce) 1.6) (* sun_light (* 5.0 (pow (max (dot bounce sun) 0.0) 120.0)))
			(glint place bounce buoy_at buoy_light) (glint place bounce ping_at ping))
		;how much a glancing look sees, more at its edges
		glance (pow (- 1.0 face) 5.0)
		light (+ (* sun_light (max (dot n sun) 0.0)) (* pillars_light (max (dot n pillars_at) 0.0)) (vec3 0.03 0.03 0.04)
			(near_light place n buoy_at buoy_light) (near_light place n ping_at ping))
		colour (vec3 0.0))
	(cond
		((< (abs (+ (:y local) 0.3)) 0.1)
			;the fender, matt rubber
			(setq colour (* (vec3 0.08 0.08 0.09) light)))
		((> (:y local) 0.1)
			;the mast, polished metal, the world in it, tinted
			(setq colour (+ (* (vec3 0.75 0.77 0.8) seen) (* (vec3 0.05) light))))
		(:t ;the float, polished metal a little warmer, lit as well, the world
			;in it the more at its edges
			(setq colour (+ (* (vec3 0.62 0.6 0.56) seen (+ 0.7 (* 0.3 glance))) (* (vec3 0.3 0.3 0.28) light)))))
	(vec4 (min colour (vec3 1.0)) 1.0))
