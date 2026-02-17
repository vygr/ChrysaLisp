;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "./vector.inc")

(enums +mat3x3 0
	(enum r0 r1 r2))

(enums +mat4x4 0
	(enum r0 r1 r2 r3))

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

(defun mat3x3-vec3-mul (m v)
	(bind '(m00 m01 m02 m10 m11 m12 m20 m21 m22) m)
	(bind '(vx vy vz) v)
	(Vec3-r (+ (* m00 vx) (* m01 vy) (* m02 vz))
		(+ (* m10 vx) (* m11 vy) (* m12 vz))
		(+ (* m20 vx) (* m21 vy) (* m22 vz))))

(defun mat4x4-vec3-mul (m v)
	(bind '(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23) m)
	(bind '(vx vy vz) v)
	(Vec3-r (+ (* m00 vx) (* m01 vy) (* m02 vz))
		(+ (* m10 vx) (* m11 vy) (* m12 vz))
		(+ (* m20 vx) (* m21 vy) (* m22 vz))))

(defun mat4x4-vec4-mul (m v)
	(bind '(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m)
	(bind '(vx vy vz vw) v)
	(Vec4-r (+ (* m00 vx) (* m01 vy) (* m02 vz) (* m03 vw))
		(+ (* m10 vx) (* m11 vy) (* m12 vz) (* m13 vw))
		(+ (* m20 vx) (* m21 vy) (* m22 vz) (* m23 vw))
		(+ (* m30 vx) (* m31 vy) (* m32 vz) (* m33 vw))))

(defun mat3x3-mul (ma mb)
	(bind '(a00 a01 a02 a10 a11 a12 a20 a21 a22) ma)
	(bind '(b00 b01 b02 b10 b11 b12 b20 b21 b22) mb)
	(reals
		(+ (* a00 b00) (* a01 b10) (* a02 b20))
		(+ (* a00 b01) (* a01 b11) (* a02 b21))
		(+ (* a00 b02) (* a01 b12) (* a02 b22))
		(+ (* a10 b00) (* a11 b10) (* a12 b20))
		(+ (* a10 b01) (* a11 b11) (* a12 b21))
		(+ (* a10 b02) (* a11 b12) (* a12 b22))
		(+ (* a20 b00) (* a21 b10) (* a22 b20))
		(+ (* a20 b01) (* a21 b11) (* a22 b21))
		(+ (* a20 b02) (* a21 b12) (* a22 b22))))

(defun mat4x4-mul (ma mb)
	(bind '(a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33) ma)
	(bind '(b00 b01 b02 b03 b10 b11 b12 b13 b20 b21 b22 b23 b30 b31 b32 b33) mb)
	(reals
		(+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
		(+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31))
		(+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32))
		(+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))
		(+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
		(+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31))
		(+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32))
		(+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))
		(+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
		(+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31))
		(+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32))
		(+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))
		(+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))
		(+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31))
		(+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32))
		(+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))))

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
		((= (count m) 9)
			(bind '(m00 m01 m02 m10 m11 m12 m20 m21 m22) m)
			(print (Vec3-r m00 m01 m02))
			(print (Vec3-r m10 m11 m12))
			(print (Vec3-r m20 m21 m22)))
		((= (count m) 16)
			(bind '(m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) m)
			(print (Vec4-r m00 m01 m02 m03))
			(print (Vec4-r m10 m11 m12 m13))
			(print (Vec4-r m20 m21 m22 v23))
			(print (Vec4-r m30 m31 m32 m33)))))
