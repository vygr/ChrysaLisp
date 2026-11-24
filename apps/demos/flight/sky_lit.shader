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
(definput forward :vec3)
(definput right :vec3)
(definput up :vec3)
(definput spread :vec2)
(definput sun :vec3)
(definput haze :vec3)
(definput above :vec3)

(defvarying screen :vec2)

(defconst sun_colour (vec3 1.0 0.92 0.75))

(defun main :vec4 ((frag :vec2))
	(defq d (normalize (+ forward (* (:x screen) (:x spread) right) (* (:y screen) (:y spread) up)))
		high (max (:y d) 0.0) s (max (dot d sun) 0.0))
	(vec4 (min (+ (mix haze above (pow high 0.6))
			(* sun_colour (+ (* 2.0 (pow s 900.0)) (* 0.35 (pow s 12.0)))))
		(vec3 1.0)) 1.0))
