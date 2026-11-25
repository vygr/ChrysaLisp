;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A ball's map, functions for a shader to import: the angle of a
; slope, and where on a map of all of a ball a way from its middle
; is. The map is the way round across it, twice as wide as it is
; high, minus z in its middle and x a quarter of the way on, and up
; to down down it, as a map of the sky round the eye or of a globe.
; No inputs, no main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the angle of y over x, as C's atan2, -pi to pi: there is no arc
;tangent among the shader's own, so a polynomial of the slope from 0 to
;1, its error under 1e-5 of a radian, then turned to the octant
(defun angle2 :float ((y :float) (x :float))
	(defq ax (abs x) ay (abs y) big (max ax ay))
	(if (<= big 0.0) (return 0.0))
	(defq t (/ (min ax ay) big) t2 (* t t)
		a (* t (+ "0.99997726" (* t2 (+ "-0.33262347" (* t2 (+ "0.19354346"
			(* t2 (+ "-0.11643287" (* t2 (+ "0.05265332" (* t2 "-0.01172120"))))))))))))
	(if (> ay ax) (setq a (- "1.5707963" a)))
	(if (< x 0.0) (setq a (- "3.1415927" a)))
	(if (< y 0.0) (setq a (- a)))
	a)

;where on the map a way is, across and down, 0 to 1. The way need not be
;of length 1
(defun sphere_uv :vec2 ((d :vec3))
	(vec2 (+ 0.5 (* "0.15915494" (angle2 (:x d) (- (:z d)))))
		(- 0.5 (* "0.31830989" (angle2 (:y d) (length (:xz d)))))))
