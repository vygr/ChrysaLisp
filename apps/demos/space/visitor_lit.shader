;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a visitor, a pixel shader. It goes with Flight's
; craft_vertex.shader, a part of one of Flight's craft come to look
; round the field.
;
; Its metal shows the world round it as a buoy's does, the sky shader's
; sky, the nebula, the Pillars and the stars, where the way from the eye
; bounces off it, the sun a hard glint and the ship's ping a glint too;
; of the part's tint, with Flight's film over it whose colours run with
; the angle it is seen at. Seams between its panels, and flecks of
; lamps round its rim lit by a wave that runs over them; its lights
; glow of themselves and pulse. Far off it goes into the dark, its
; lights last.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")
(import "lib/gpu/shaders/sphere.shader")

;the sky's pictures, as the sky shader has them: the Pillars, the nebula
;of all of the sky, and its stars
(deftexture pillars)
(deftexture nebula)
(deftexture stars)

;the way to the sun and its light, the Pillars' light, where the eye is,
;the time, seconds
(definput sun :vec3)
(definput sun_light :vec3)
(definput pillars_light :vec3)
(definput eye :vec3)
(definput time :float)
;where the Pillars are on the sky, and their pictures' sizes
(definput pillars_at :vec3)
(definput pillars_across :vec3)
(definput pillars_up :vec3)
(definput pillars_size :vec2)
(definput pillars_pixels :vec2)
(definput nebula_pixels :vec2)
(definput stars_pixels :vec2)
;the ship's ping, where and as bright as it is
(definput ping_at :vec3)
(definput ping :vec3)
;the part: the colour of its metal, the light it gives, how much of the
;film it has, how many panels across it, 0 for none, and the colour of
;the lamps round its rim, black for none
(definput tint :vec3)
(definput glow :vec3)
(definput film :float)
(definput panels :float)
(definput lamps :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)
(defvarying local :vec3)

;how far a near light reaches, as the rocks have it, and the dark
(defconst nav_reach 80.0)
(defconst dark (vec3 0.0 0.0 0.0))

;a near light seen in polished metal, a hot glint where the bounce goes
;toward it, fading as it is further, as a buoy has it
(defun glint :vec3 ((p :vec3) (bounce :vec3) (at :vec3) (colour :vec3))
	(defq v (- at p) d (length v))
	(/ (* colour (* 3.0 (pow (max (dot bounce (/ v d)) 0.0) 60.0)))
		(+ 1.0 (* (/ d nav_reach) (/ d nav_reach)))))

;a picture's colour at a place, smooth, as a buoy reads them
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

;the sky a way, as the sky shader and a buoy make it
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

;how near a line of a grid of n across a value is, 1 on it
(defun seam :float ((v :float) (n :float))
	(- 1.0 (ease 0.0 0.06 (abs (- (fract (+ (* v n) 0.5)) 0.5)))))

(defun main :vec4 ((frag :vec2))
	(defq n (normalize facing) to_eye (normalize (- eye place)) face (max (dot n to_eye) 0.0)
		bounce (reflect (- to_eye) n)
		;the world seen in it, as in a buoy: the sky, the sun's glint, the ping's
		seen (+ (* (sky_in bounce) 1.6) (* sun_light (* 5.0 (pow (max (dot bounce sun) 0.0) 120.0)))
			(glint place bounce ping_at ping))
		light (+ (* sun_light (max (dot n sun) 0.0)) (* pillars_light (max (dot n pillars_at) 0.0)) (vec3 0.03 0.03 0.04))
		edge (pow (- 1.0 face) 2.0)
		;the film's colours, round the hues as the angle goes
		hue (* 6.283 (+ (* edge 1.5) (* 0.15 (:y n))))
		oil (+ (vec3 0.5) (* 0.5 (vec3 (cos hue) (cos (+ hue 2.094)) (cos (+ hue 4.189)))))
		;the panels, rings round it and lines across, darker at their seams
		round (length (:xz local))
		seams (* (min panels 1.0) (max (seam round panels) (max (seam (:x local) panels) (seam (:z local) panels))))
		metal (* (+ tint (* oil film)) (- 1.0 (* 0.55 seams)))
		colour (+ (* metal seen (+ 0.7 (* 0.5 edge))) (* metal light 0.3))
		pulse (+ 0.7 (* 0.3 (cos (* time 4.0))))
		;the lamps, flecks on the rim, each lit as a wave goes over
		spots (pow (max (* (cos (* (:x local) 22.0)) (cos (* (:z local) 22.0))) 0.0) 4.0)
		rim (* (ease 0.62 0.72 round) (- 1.0 (ease 0.04 0.14 (abs (:y local)))))
		wave (pow (+ 0.5 (* 0.5 (cos (- (* time 5.0) (* 3.0 (+ (:x local) (:z local))))))) 3.0)
		lit (+ (* glow pulse) (* lamps (* spots rim (+ 0.25 (* 1.5 wave))))))
	(vec4 (min (+ (mix colour dark fade) (* lit (- 1.0 (* 0.6 fade)))) (vec3 1.0)) 1.0))
