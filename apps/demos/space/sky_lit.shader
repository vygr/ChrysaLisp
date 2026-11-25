;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the sky, a pixel shader. It goes with the library's
; sky_vertex.shader.
;
; The way a pixel looks is made from the eye's forward, right and up
; and where on the frame it is. The sky is deep space, near black,
; the galaxy's band across it, and its stars, fixed in the sky as the
; eye turns. In one way of it the Pillars of Creation, the picture laid
; on the sky as a window of it is seen through, flat, and past its
; edges its nebula, thinning out into the dark, so it has no frame. In
; another the sun, its disc and the glow round it.
;
; Behind it all the nebula, a map of all of the sky made once from the
; picture and kept, bake_nebula.lisp, where on it a way is by sphere_uv. The
; picture is laid over it where it is, from three quarters of the way
; out to its edge, and the nebula there is a blur of it, so its edge is
; not seen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")
(import "lib/gpu/shaders/sphere.shader")

;the picture, the nebula of all of the sky, and its stars
(deftexture pillars)
(deftexture nebula)
(deftexture stars)

;the eye's ways, how wide and high the frame is at a distance of 1, and
;the way to the sun
(definput forward :vec3)
(definput right :vec3)
(definput up :vec3)
(definput spread :vec2)
(definput sun :vec3)
;the way to the middle of the picture, its across and up, and how far it
;reaches across and up, at a distance of 1
(definput pillars_at :vec3)
(definput pillars_across :vec3)
(definput pillars_up :vec3)
(definput pillars_size :vec2)
;the sizes of the three pictures, pixels
(definput pillars_pixels :vec2)
(definput nebula_pixels :vec2)
(definput stars_pixels :vec2)

(defvarying screen :vec2)

(defconst sun_colour (vec3 1.0 0.94 0.84))
(defconst deep (vec3 0.004 0.005 0.010))

;a picture's colour at a place, smooth: a texture is read the one pixel
;a place is in, so the four round it are read and mixed by how near it is
;to each, as a GPU's linear filter. Down it stops at the edge; across too
;for the Pillars, but the nebula goes round, as the sky does
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

(defun main :vec4 ((frag :vec2))
	(defq d (normalize (+ forward (* (:x screen) (:x spread) right) (* (:y screen) (:y spread) up)))
		at (sphere_uv d) colour deep seen 0.0)
	;the nebula, and the picture over it, where the way is toward it:
	;across and up on the flat of it a distance of 1 away, as a fraction
	;of how far it reaches, its edge made uneven
	(setq colour (+ colour (smooth 1 at nebula_pixels)))
	(defq toward (dot d pillars_at))
	(when (> toward 0.5)
		(defq x (/ (dot d pillars_across) (* toward (:x pillars_size)))
			y (/ (dot d pillars_up) (* toward (:y pillars_size)))
			out (+ (max (abs x) (abs y)) (* 0.16 (- (noise3 (* d 7.0)) 0.5))))
		(setq seen (- 1.0 (ease 0.72 0.97 out)))
		(if (> seen 0.0)
			(setq colour (mix colour (smooth 0 (vec2 (+ 0.5 (* 0.5 x)) (- 0.5 (* 0.5 y))) pillars_pixels) seen))))
	;the stars, the map of them, bake_stars.lisp, but few in the picture, which
	;has its own; and the sun
	(defq s (max (dot d sun) 0.0) lit (* (smooth 2 at stars_pixels) (- 1.0 (* 0.85 seen))))
	(vec4 (min (+ colour lit (* sun_colour (+ (* 4.0 (pow s 4000.0)) (* 0.7 (pow s 300.0)) (* 0.12 (pow s 18.0)))))
		(vec3 1.0)) 1.0))
