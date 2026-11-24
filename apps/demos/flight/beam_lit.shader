;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A craft's beam, a pixel shader. It goes with beam_vertex.shader.
;
; Light, added to what is behind it: the most where the eye looks
; through the middle of the cone, none at its edges, so it is a soft
; shaft; brighter at the craft than at the land, faint bands running
; down it, and into the haze with distance. Drawn after all else, its
; colour added and almost nothing taken away: a pixel quite clear is
; thrown away by some back ends, so it is a little less than clear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the colour of the beam, where the eye is, and the time, seconds
(definput colour :vec3)
(definput eye :vec3)
(definput time :float)

(defvarying along :float)
(defvarying facing :vec3)
(defvarying place :vec3)
(defvarying fade :float)

(defun main :vec4 ((frag :vec2))
	(defq to_eye (normalize (- eye place))
		edge (pow (abs (dot (normalize facing) to_eye)) 1.5)
		bands (+ 0.8 (* 0.2 (sin (- (* along 18.0) (* time 6.0)))))
		strength (* edge bands (+ 0.3 (* 0.7 (- 1.0 along))) (- 1.0 fade) 0.6))
	(vec4 (* colour strength) 0.005))
