;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "./vector.inc")

(defun Mat3x3-unity ()
	(reals +real_1 +real_0 +real_0
		+real_0 +real_1 +real_0
		+real_0 +real_0 +real_1))

(defun Mat4x4-unity ()
	(reals +real_1 +real_0 +real_0 +real_0
		+real_0 +real_1 +real_0 +real_0
		+real_0 +real_0 +real_1 +real_0
		+real_0 +real_0 +real_0 +real_1))

(defun Mat3x3-rotx (a)
	(defq s (sin a) c (cos a))
	(reals +real_1 +real_0 +real_0
		+real_0 c (neg s)
		+real_0 s c))

(defun Mat3x3-roty (a)
	(defq s (sin a) c (cos a))
	(reals c +real_0 s
		+real_0 +real_1 +real_0
		(neg s) +real_0 c))

(defun Mat3x3-rotz (a)
	(defq s (sin a) c (cos a))
	(reals c (neg s) +real_0
		s c +real_0
		+real_0 +real_0 +real_1))

(defun Mat3x3-scale (s)
	(reals s +real_0 +real_0
		+real_0 s +real_0
		+real_0 +real_0 s))

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

(defun mat4x4-vec3-mul (m v)
	(Vec3-r (nums-dot v (slice m 0 4))
		(nums-dot v (slice m 4 8))
		(nums-dot v (slice m 8 12))))

(defun mat4x4-invert (m)
	; (mat4x4-invert mat4x4) -> mat4x4
	(bind '(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m)
	(defq
		a2323 (- (* m22 m33) (* m23 m32))
		a1323 (- (* m21 m33) (* m23 m31))
		a1223 (- (* m21 m32) (* m22 m31))
		a0323 (- (* m20 m33) (* m23 m30))
		a0223 (- (* m20 m32) (* m22 m30))
		a0123 (- (* m20 m31) (* m21 m30))
		a2313 (- (* m12 m33) (* m13 m32))
		a1313 (- (* m11 m33) (* m13 m31))
		a1213 (- (* m11 m32) (* m12 m31))
		a2312 (- (* m12 m23) (* m13 m22))
		a1312 (- (* m11 m23) (* m13 m21))
		a1212 (- (* m11 m22) (* m12 m21))
		a0313 (- (* m10 m33) (* m13 m30))
		a0213 (- (* m10 m32) (* m12 m30))
		a0312 (- (* m10 m23) (* m13 m20))
		a0212 (- (* m10 m22) (* m12 m20))
		a0113 (- (* m10 m31) (* m11 m30))
		a0112 (- (* m10 m21) (* m11 m20))
		det (recip (+
			(* m00 (+ (* m11 a2323) (neg (* m12 a1323)) (* m13 a1223)))
			(* m01 (- (* m12 a0323) (* m10 a2323) (* m13 a0223)))
			(* m02 (+ (* m10 a1323) (neg (* m11 a0323)) (* m13 a0123)))
			(* m03 (+ (* m11 a0223) (* m10 a1223) (* m12 a0123))))))
	(nums-scale (reals
		(+ (* m11 a2323) (neg (* m12 a1323)) (* m13 a1223))
		(- (* m02 a1323) (* m01 a2323) (* m03 a1223))
		(+ (* m01 a2313) (neg (* m02 a1313)) (* m03 a1213))
		(- (* m02 a1312) (* m01 a2312) (* m03 a1212))
		(- (* m12 a0323) (* m10 a2323) (* m13 a0223))
		(+ (* m00 a2323) (neg (* m02 a0323)) (* m03 a0223))
		(- (* m02 a0313) (* m00 a2313) (* m03 a0213))
		(+ (* m00 a2312) (neg (* m02 a0312)) (* m03 a0212))
		(+ (* m10 a1323) (neg (* m11 a0323)) (* m13 a0123))
		(- (* m01 a0323) (* m00 a1323) (* m03 a0123))
		(+ (* m00 a1313) (neg (* m01 a0313)) (* m03 a0113))
		(- (* m01 a0312) (* m00 a1312) (* m03 a0112))
		(- (* m11 a0223) (* m10 a1223) (* m12 a0123))
		(+ (* m00 a1223) (neg (* m01 a0223)) (* m02 a0123))
		(- (* m01 a0213) (* m00 a1213) (* m02 a0113))
		(+ (* m00 a1212) (neg (* m01 a0212)) (* m02 a0112))) det))

(defun print-mat (m)
	(cond
		((= (length m) 9)
			(print (slice m 0 3))
			(print (slice m 3 6))
			(print (slice m 6 9)))
		((= (length m) 16)
			(print (slice m 0 4))
			(print (slice m 4 8))
			(print (slice m 8 12))
			(print (slice m 12 16)))))
