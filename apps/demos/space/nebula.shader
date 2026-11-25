;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the nebula round the Pillars, functions for bake_nebula.lisp, which
; calls them from Lisp as it makes the map of the nebula. No inputs, no
; main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")

;how thick the nebula is in a way, 0 to near 1: noise through the sky at
;three sizes, so it is clouds and strands and not a smear, its edges
;taken in, so there is clear sky between them
(defun wisp :float ((d :vec3))
	(defq n (+ (* 0.55 (noise3 (* d 2.6))) (* 0.3 (noise3 (+ (* d 6.1) (vec3 13.0 7.0 3.0))))
			(* 0.15 (noise3 (+ (* d 14.0) (vec3 5.0 19.0 11.0))))))
	(ease 0.3 0.75 n))

;how clear a way of the galaxy's band is, 1 clear to 0 a dark lane of its
;dust: finer noise, stretched along the band less than across it
(defun dust :float ((d :vec3))
	(defq n (+ (* 0.6 (noise3 (+ (* d 8.0) (vec3 3.0 17.0 29.0)))) (* 0.4 (noise3 (+ (* d 19.0) (vec3 23.0 5.0 13.0))))))
	(ease 0.38 0.62 n))
