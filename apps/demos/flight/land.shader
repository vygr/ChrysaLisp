;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The land of the flight, functions for a shader to import. How
; high the ground is at a place, rolling hills of noise at a few
; sizes, a big one and smaller ones laid over it, and water where it
; goes down under the water's level. No inputs, no main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")

;how high the water is, and how big the hills are and how high
(defconst water -4.0)
(defconst hill_size 0.009)
(defconst hill_height 95.0)
(defconst hill_floor -20.0)

;noise at a few sizes, each half the one before and half as high, from
;0 to near 1. Each is turned and made twice as fine by the one step, so
;the sizes do not line up
(defun fbm :float ((p :vec2))
	(defq v 0.0 a 0.5 q p)
	(for (i 0 5)
		(setq v (+ v (* a (noise2 q)))
			q (vec2 (- (* (:x q) 1.6) (* (:y q) 1.2)) (+ (* (:x q) 1.2) (* (:y q) 1.6)))
			a (* a 0.5)))
	v)

;how high the ground is at a place, across and along: the noise to a
;power, so the low land stays low and the high is lifted into peaks
(defun ground :float ((p :vec2))
	(+ hill_floor (* hill_height (pow (fbm (* p hill_size)) 1.6))))
