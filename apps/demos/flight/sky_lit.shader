;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The sky, a pixel shader. It goes with sky_vertex.shader.
;
; The way a pixel looks is made from the eye's forward, right and up
; and where on the frame it is, as the Surface demo's pixel_ray. The
; sky is the haze at the horizon going up to the blue above, with the
; sun's disc and the glow round it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the eye's ways, how wide and high the frame is at a distance of 1, the
;way to the sun, the haze and the sky above
(import "apps/demos/flight/land.shader")

(definput forward :vec3)
(definput right :vec3)
(definput up :vec3)
(definput spread :vec2)
(definput sun :vec3)
(definput haze :vec3)
(definput above :vec3)
;the way to the moon, and how much it is night, its glow round it
(definput moon :vec3)
(definput night :float)
;1 for stars, 0 for none, for a GPU that can not keep up
(definput starry :float)
;the time, seconds, the stars twinkle by
(definput time :float)

(defvarying screen :vec2)

(defconst sun_colour (vec3 1.0 0.92 0.75))
(defconst moon_colour (vec3 0.75 0.82 1.0))

;the stars of a layer: the sky cut into cells by the way to them, a hash
;of each cell whether it has a star, where in it, how bright and of what
;colour, blue-white to warm, each twinkling at its own rate; fixed in the
;sky as the eye turns, the same each night
(defun stars :vec3 ((d :vec3) (cells :float) (chance :float) (size :float) (bright :float))
	(defq p (* d cells) c (floor p) f (- (fract p) (vec3 0.5)) h (lattice3 c))
	(if (< h chance) (return (vec3 0.0)))
	(defq j (- (vec3 (lattice3 (+ c (vec3 17.0 0.0 0.0))) (lattice3 (+ c (vec3 0.0 31.0 0.0)))
				(lattice3 (+ c (vec3 0.0 0.0 47.0)))) (vec3 0.5))
		k (/ (- h chance) (- 1.0 chance))
		twinkle (+ 0.7 (* 0.3 (sin (+ (* time (+ 1.5 (* 3.0 k))) (* h 300.0)))))
		colour (mix (vec3 0.72 0.82 1.0) (vec3 1.0 0.88 0.72) (fract (* h 37.0))))
	(* colour (* bright (+ 0.35 (* 0.65 k)) twinkle (- 1.0 (ease 0.0 size (length (- f (* j 0.6))))))))

(defun main :vec4 ((frag :vec2))
	(defq d (normalize (+ forward (* (:x screen) (:x spread) right) (* (:y screen) (:y spread) up)))
		high (max (:y d) 0.0) s (max (dot d sun) 0.0) m (max (dot d moon) 0.0))
;the stars by night, many faint and a few bright, fading to the haze at
	;the horizon and in the moon's light near it
	(defq starlight (vec3 0.0))
	(if (> (* starry night) 0.0)
		(setq starlight (* (+ (stars d 150.0 0.965 0.24 0.8) (stars d 55.0 0.982 0.15 1.7))
			night (ease 0.03 0.22 (:y d)) (- 1.0 (pow m 120.0)))))
	(vec4 (min (+ (mix haze above (pow high 0.6)) starlight
			(* sun_colour (+ (* 2.0 (pow s 900.0)) (* 0.35 (pow s 12.0))))
			(* moon_colour night (+ (* 0.12 (pow m 40.0)) (* 0.08 (pow m 6.0)))))
		(vec3 1.0)) 1.0))
