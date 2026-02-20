;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "./vector.inc")

(defun Mat4x4-unity ()
	(reals +real_1 +real_0 +real_0 +real_0
		+real_0 +real_1 +real_0 +real_0
		+real_0 +real_0 +real_1 +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-rotx (a)
	(defq s (sin a) c (cos a))
	(reals +real_1 +real_0 +real_0 +real_0
		+real_0 c (neg s) +real_0
		+real_0 s c +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-roty (a)
	(defq s (sin a) c (cos a))
	(reals c +real_0 s +real_0
		+real_0 +real_1 +real_0 +real_0
		(neg s) +real_0 c +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-rotz (a)
	(defq s (sin a) c (cos a))
	(reals c (neg s) +real_0 +real_0
		s c +real_0 +real_0
		+real_0 +real_0 +real_1 +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-translate (x y z)
	(reals +real_1 +real_0 +real_0 x
		+real_0 +real_1 +real_0 y
		+real_0 +real_0 +real_1 z
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-scale (x y z)
	(reals x +real_0 +real_0 +real_0
		+real_0 y +real_0 +real_0
		+real_0 +real_0 z +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat4x4-frustum (left right top bottom near far)
	(defq rl (recip (- right left))
		tb (recip (- top bottom))
		nf (recip (- near far)))
	(reals (* (* +real_2 near) rl) +real_0 (* (+ left right) rl) +real_0
		+real_0 (* (* +real_2 near) tb) (* (+ top bottom) tb) +real_0
		+real_0 +real_0 (* (+ near far) nf) (* (* (* +real_2 near) far) nf)
		+real_0 +real_0 +real_-1 +real_0))
