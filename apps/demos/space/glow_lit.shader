;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the glow round a buoy's lamp, a pixel shader. It goes with
; glow_vertex.shader.
;
; Light round the lamp, bright at it and falling away, added to what
; is behind it, as light is: an alpha of all but none, just over the
; least a pixel may have and be drawn, 1 in 255, as Flight's beam.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definput lamp :vec3)

(defvarying square :vec2)

(defun main :vec4 ((frag :vec2))
	(defq r (length square))
	(if (>= r 1.0) (return (vec4 0.0)))
	(defq k (- 1.0 r))
	(vec4 (* lamp (+ (* 0.6 k k k k) (* 0.25 k k))) 0.005))
