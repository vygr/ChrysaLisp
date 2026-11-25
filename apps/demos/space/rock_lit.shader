;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a rock, a pixel shader. It goes with rock_vertex.shader.
;
; The rock's face is the moon's, its map laid on it from the three ways
; of the rock, each as much as the face is turned to it, at a place on
; the map and a size the rock's own, so no two rocks are alike. It is
; made rough by a bump, a fine noise's slope across it tilting it, as
; Flight's land is, less of it the further off. It is matt, lit as much
; as it is turned to each light: the sun's, warm, and the nebulas', the
; Pillars' teal a quarter as bright from their way and the far one's
; amber fainter from its, so its side away from the sun is not black but
; the nebula's colour. Near to, the lamp of the nearest marker buoy
; lights it in its colour as it flashes, and the ship's white ping, each
; as a lamp does, the nearer the more. Far off it goes into the sky behind
; it, the nebula's colour that way, so it is not seen to come or go.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")
(import "lib/gpu/shaders/sphere.shader")

;the moon's map, and the nebula of all of the sky
(deftexture surface)
(deftexture nebula)

;the way to the sun, where the eye is, where on the moon's map this rock
;is and how much of it, and how rough it is, 0 for none
(definput sun :vec3)
;the lights, the sun's colour and the nebulas', and the ways to them
(definput sun_light :vec3)
(definput pillars_at :vec3)
(definput pillars_light :vec3)
(definput far_at :vec3)
(definput far_light :vec3)
;the ship's ping, where and as bright, black between pings
(definput ping_at :vec3)
(definput ping :vec3)
(definput eye :vec3)
(definput rough :float 0.0 0.0 1.0)
;the sizes of the two maps, pixels
(definput surface_pixels :vec2)
(definput nebula_pixels :vec2)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying local :vec3)
(defvarying far :float)
;what is the rock's own, from rock_vertex.shader, the same over all of it,
;so all rocks have the one list of inputs here: where on the moon's map it
;is, how big it is, across, so its fine detail is of a size in the world,
;not of the rock, and the lamp of the buoy nearest, where it is and its
;colour as bright as it is now, black for none near
(defvarying patch :vec3)
(defvarying bulk :float)
(defvarying buoy_at :vec3)
(defvarying buoy_light :vec3)

;how far off the bump is whole and gone, and the moon's map's fine
;detail, a second reading of it five times finer
(defconst bump_near 60.0)
(defconst bump_far 260.0)
(defconst fine_near 20.0)
(defconst fine_far 120.0)
;how far a lamp reaches, a buoy's or the ship's, half as bright there,
;and past which it is not worked out
(defconst nav_reach 80.0)
(defconst nav_far 250.0)
;how far off the rock starts to go into the sky and is gone into it: the
;app has it gone before a rock is far enough to be drawn where it is next
;as the field goes on, half its box away
(definput fade_near :float)
(definput fade_far :float)
(defconst eps 0.06)

;a map's colour at a place, smooth: a texture is read the one pixel a
;place is in, so the four round it are read and mixed by how near it is
;to each. Both maps go round across, as the moon and the sky do
(defun smooth :vec3 ((which :int) (uv :vec2) (pixels :vec2))
	(defq p (- (* uv pixels) (vec2 0.5)) f (fract p) step (/ (vec2 1.0) pixels)
		a (* (+ (floor p) (vec2 0.5)) step)
		u0 (fract (:x a)) u1 (fract (+ (:x a) (:x step)))
		v0 (clamp (:y a) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step))))
		v1 (clamp (+ (:y a) (:y step)) (* 0.5 (:y step)) (- 1.0 (* 0.5 (:y step))))
		c00 (vec3 0.0) c10 (vec3 0.0) c01 (vec3 0.0) c11 (vec3 0.0))
	(if (= which 0)
		(setq c00 (:xyz (texture surface (vec2 u0 v0))) c10 (:xyz (texture surface (vec2 u1 v0)))
			c01 (:xyz (texture surface (vec2 u0 v1))) c11 (:xyz (texture surface (vec2 u1 v1))))
		(setq c00 (:xyz (texture nebula (vec2 u0 v0))) c10 (:xyz (texture nebula (vec2 u1 v0)))
			c01 (:xyz (texture nebula (vec2 u0 v1))) c11 (:xyz (texture nebula (vec2 u1 v1)))))
	(mix (mix c00 c10 (:x f)) (mix c01 c11 (:x f)) (:y f)))

;the moon on the rock, from its three ways, each as much as the face is
;turned to it, the side ways at places of their own on the map, so much
;of the map to the rock and from a place moved by shift
(defun moon_on :float ((p :vec3) (n :vec3) (scale :float) (shift :vec2))
	(defq w (abs n))
	(setq w (* w w w w))
	(setq w (/ w (+ (:x w) (:y w) (:z w))))
	(defq s (* (:z patch) scale) o (+ (:xy patch) shift) a (+ (* (:yz p) s) o)
		b (+ (* (:xz p) s) o (vec2 0.31 0.17)) c (+ (* (:xy p) s) o (vec2 0.57 0.41)))
	(+ (* (:x w) (:x (smooth 0 a surface_pixels))) (* (:y w) (:x (smooth 0 b surface_pixels)))
		(* (:z w) (:x (smooth 0 c surface_pixels)))))

;the fine noise the face is made rough by, at three sizes, of a place in
;the world's units
(defun grain :float ((p :vec3))
	(+ (* (noise3 (* p 0.35)) 0.5) (* (noise3 (* p 0.9)) 0.3) (* (noise3 (* p 2.3)) 0.2)))

;the face tilted by the slope of the grain across it, Flight's bumpmap
(defun bumped :vec3 ((p :vec3) (n :vec3) (by :float))
	;the slope of the grain one sided, four readings of it not six
	(defq g (grain p) grad (/ (vec3
		(- (grain (+ p (vec3 eps 0.0 0.0))) g)
		(- (grain (+ p (vec3 0.0 eps 0.0))) g)
		(- (grain (+ p (vec3 0.0 0.0 eps))) g)) eps))
	(setq grad (- grad (* n (dot n grad))))
	(normalize (- n (* by grad))))

;the light of a lamp at a place on a face, as much as the face is turned
;to it and a glow that wraps round past its side, and less the further
;off
(defun lamp :vec3 ((p :vec3) (n :vec3) (at :vec3) (colour :vec3))
	(defq v (- at p) d (length v) turned (dot n (/ v d)))
	(* colour (/ (+ (max turned 0.0) (* 0.35 (max (/ (+ turned 0.6) 1.6) 0.0)))
		(+ 1.0 (* (/ d nav_reach) (/ d nav_reach))))))

;how much of a great glow of sky a face takes from its way, wrapped round
;past its side as far as a third of the way back
(defun wrapped :float ((n :vec3) (way :vec3))
	(max (/ (+ (dot n way) 0.35) 1.35) 0.0))

(defun main :vec4 ((frag :vec2))
	(defq to_eye (- place eye) way (normalize to_eye)
		fade (ease fade_near fade_far far)
		sky (smooth 1 (sphere_uv way) nebula_pixels))
	;all sky: nothing of the rock is worked out
	(if (> fade 0.998) (return (vec4 sky 1.0)))
	(defq n (normalize facing) bump (* rough (- 1.0 (ease bump_near bump_far far)))
		fine (* rough (- 1.0 (ease fine_near fine_far far))))
	(if (> bump 0.0) (setq n (bumped (* local bulk) n (* bump 1.6))))
	(defq grey (moon_on local (normalize facing) 1.0 (vec2 0.0)))
	;close to, the map's fine detail over it, small craters
	(if (> fine 0.0)
		(setq grey (* grey (+ 1.0 (* fine 1.2 (- (moon_on local (normalize facing) 5.0 (vec2 0.43 0.29)) 0.45))))))
	(defq
		;matt: as much of each light as it is turned to it, the nebulas'
		;wrapping round as far as a face turned a third of the way from them,
		;as the light of a great glow of sky does, and a little from all round
		light (+ (* sun_light (* 1.25 (max (dot n sun) 0.0)))
			(* pillars_light (wrapped n pillars_at)) (* far_light (wrapped n far_at))
			(vec3 0.015 0.017 0.022))
		colour (vec3 0.0))
	;the buoy's lamp and the ship's ping, near to
	(if (< far nav_far)
		(setq light (+ light (lamp place n buoy_at buoy_light) (lamp place n ping_at ping))))
	(setq colour (* (vec3 grey (* grey 0.97) (* grey 0.93)) light))
	(vec4 (mix colour sky fade) 1.0))
