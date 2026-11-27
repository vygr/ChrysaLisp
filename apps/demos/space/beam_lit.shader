;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a visitor's beam, a pixel shader. It goes with Space's
; beam_vertex.shader.
;
; Flight's beam made for how a pixel is laid over what is behind it: its
; colour and how much of it there is, its alpha, the back ends multiplying
; the one by the other and taking as much of what is behind away. So the
; beam is its colour, as much of it as it is strong: the most where the
; eye looks through the middle of the tube, none at its edges, a soft
; shaft, the middle and edges taken across its axis, so it is a shaft
; seen from any side; the brighter the longer the way the eye looks
; through it, so seen along it, it glows the more, not less; brighter at
; the craft than at the rock, faint bands running down it, and into the
; dark with distance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the colour of the beam, where the eye is, and the time, seconds
(definput colour :vec3)
(definput eye :vec3)
(definput time :float)

(defvarying along :float)
(defvarying facing :vec3)
(defvarying place :vec3)
(defvarying fade :float)
(defvarying axis :vec3)

(defun main :vec4 ((frag :vec2))
	(defq to_eye (normalize (- eye place)) along_way (normalize axis)
		;the way to the eye across the axis, and how long it is, the less
		;the more the eye looks along the beam
		across (- to_eye (* along_way (dot to_eye along_way))) side (length across)
		edge (pow (abs (dot (normalize facing) (/ across (max side 0.001)))) 2.0)
		through (min (/ 1.0 (max side 0.001)) 3.0)
		bands (+ 0.75 (* 0.25 (sin (- (* along 18.0) (* time 6.0)))))
		strength (* edge through bands (+ 0.45 (* 0.55 (- 1.0 along))) (- 1.0 fade)))
	(vec4 colour (min strength 0.8)))
