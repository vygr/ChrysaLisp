;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the grains a hit throws out, a pixel shader. It goes with
; grain_vertex.shader.
;
; A grain is a speck of rock, round and soft edged, lit by the sun and
; the nebula, each its own shade, fading as the hit grows old. Drawn
; over what is behind it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")

;how far on the hit is, 0 new to 1 gone, and the light
(definput age :float)
(definput sun_light :vec3)
(definput pillars_light :vec3)

(defvarying square :vec2)
(defvarying shade :float)

(defconst rock (vec3 0.62 0.56 0.48))

(defun main :vec4 ((frag :vec2))
	(defq r (length square))
	(if (>= r 1.0) (return (vec4 0.0)))
	(defq alpha (* (- 1.0 (ease 0.4 1.0 r)) (- 1.0 (* age age)))
		colour (* rock shade (+ (* sun_light 1.1) pillars_light 0.08)))
	(vec4 (* colour alpha) alpha))
