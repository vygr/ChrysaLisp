;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The land of the flight, functions for a shader to import. How
; high the ground is at a place, rolling hills of noise at a few
; sizes, a big one and smaller ones laid over it, and water where it
; goes down under the water's level. No inputs, no main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;how high the water is, and how big the hills are and how high
(defconst water -4.0)
(defconst hill_size 0.009)
(defconst hill_height 95.0)
(defconst hill_floor -20.0)

;a number from 0 to 1 that looks random, of a place on the lattice, the
;same on the GPU and the CPU to the bit. sdf.shader's hash, of the sine of
;a number in the thousands times 43758, is the GPU's sine's last bits made
;big, which are not the VP's: the hills differed. This is the permute of
;Gustavson's noise, (34 x + 1) x mod 289, on whole numbers no bigger than a
;float holds exactly, so every step is exact everywhere. It repeats every
;289 of the lattice, 32000 across at the size of the hills
(defun permute :float ((x :float))
	(mod (* (+ (* 34.0 x) 1.0) x) 289.0))

(defun lattice2 :float ((i :vec2))
	(/ (permute (+ (permute (mod (:x i) 289.0)) (mod (:y i) 289.0))) 289.0))

(defun lattice3 :float ((i :vec3))
	(/ (permute (+ (permute (+ (permute (mod (:x i) 289.0)) (mod (:y i) 289.0))) (mod (:z i) 289.0))) 289.0))

;value noise, smooth from 0 to 1, across the land and through space
(defun noise2 :float ((p :vec2))
	(defq i (floor p) f (fract p))
	(setq f (* f f (- 3.0 (* 2.0 f))))
	(mix (mix (lattice2 i) (lattice2 (+ i (vec2 1.0 0.0))) (:x f))
		(mix (lattice2 (+ i (vec2 0.0 1.0))) (lattice2 (+ i (vec2 1.0 1.0))) (:x f)) (:y f)))

(defun noise3 :float ((p :vec3))
	(defq i (floor p) f (fract p))
	(setq f (* f f (- 3.0 (* 2.0 f))))
	(mix
		(mix (mix (lattice3 i) (lattice3 (+ i (vec3 1.0 0.0 0.0))) (:x f))
			(mix (lattice3 (+ i (vec3 0.0 1.0 0.0))) (lattice3 (+ i (vec3 1.0 1.0 0.0))) (:x f)) (:y f))
		(mix (mix (lattice3 (+ i (vec3 0.0 0.0 1.0))) (lattice3 (+ i (vec3 1.0 0.0 1.0))) (:x f))
			(mix (lattice3 (+ i (vec3 0.0 1.0 1.0))) (lattice3 (+ i (vec3 1.0 1.0 1.0))) (:x f)) (:y f))
		(:z f)))

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

;0 below lo, 1 above hi, and smooth between
(defun ease :float ((lo :float) (hi :float) (x :float))
	(defq t (clamp (/ (- x lo) (- hi lo)) 0.0 1.0))
	(* t t (- 3.0 (* 2.0 t))))
