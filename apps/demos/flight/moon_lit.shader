;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The moon, a pixel shader. It goes with moon_vertex.shader.
;
; The moon by day is seen through the sky, as it is: the sky's colour
; where it is, the haze at the horizon to the blue above, and the light
; of its map where the sun is on it added to that, a phase, its dark
; side the sky's own. That light is thinned by the air it is seen
; through, more of it the lower the moon.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the map of the moon, east to the right and north at the top
(deftexture surface)

;the way to the sun, the sky the moon is seen through, the haze at the
;horizon and the blue above, and how high the moon is, 0 to 1 up
(definput sun :vec3)
(definput haze :vec3)
(definput above :vec3)
(definput rise :float)

(defvarying uv :vec2)
(defvarying facing :vec3)

(defconst bright 1.5)

(defun main :vec4 ((frag :vec2))
	(defq sky (mix haze above (pow rise 0.6))
		tint (:xyz (texture surface uv))
		lit (max (dot (normalize facing) sun) 0.0)
		thin (- 0.85 (* 0.5 (- 1.0 rise))))
	(vec4 (min (+ sky (* tint bright lit thin)) (vec3 1.0)) 1.0))
