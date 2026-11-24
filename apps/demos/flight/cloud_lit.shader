;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The clouds, a pixel shader. It goes with cloud_vertex.shader.
;
; Cloud where a noise of three octaves, drifting in the wind, is high
; enough, clear sky between, soft at its edges; thick cloud darker under
; it, seen from below, and bright on top, seen from above; its thin edges
; shining near the sun. Lit by the light of the day, white, red at dusk,
; dim by the moon, and into the haze far off. Drawn over what is behind
; it, as much as it is cloud, its colour as much of it as there is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the way to the light, the haze, the light of the day and how it shines,
;where the eye is, and the time, seconds
(definput sun :vec3)
(definput haze :vec3)
(definput daylight :vec3)
(definput shine :vec3)
(definput eye :vec3)
(definput time :float)

(defvarying place :vec3)
(defvarying fade :float)

(defconst cloud_base 150.0)

(defun main :vec4 ((frag :vec2))
	(defq p (+ (* (:xz place) 0.0035) (vec2 (* time 0.004) (* time 0.002)))
		n (+ (* (noise2 p) 0.55) (* (noise2 (+ (* p 2.03) (vec2 5.2 1.3))) 0.28)
			(* (noise2 (+ (* p 4.1) (vec2 1.7 9.2))) 0.17))
		cover (ease 0.48 0.72 n) thick (ease 0.56 0.86 n)
		;1 seen from under, 0 from over
		under (clamp (- cloud_base (:y eye)) 0.0 1.0)
		shade (mix (mix 1.0 0.85 thick) (mix 0.95 0.55 thick) under)
		s (max (dot (normalize (- place eye)) sun) 0.0)
		colour (+ (* (vec3 0.97 0.97 1.0) daylight shade) (* shine (* (pow s 8.0) 0.45 (- 1.0 thick))))
		hazed (mix colour haze fade)
		alpha (* cover 0.9 (- 1.0 (* fade 0.6))))
	(vec4 (* hazed alpha) alpha))
