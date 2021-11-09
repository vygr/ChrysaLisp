;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "lib/math/vector.inc")

(enums +mat3x3 0
	(enum r0 r1 r2))

(enums +mat4x4 0
	(enum r0 r1 r2 r3))

(defun Mat3x3-unity ()
	(list (Vec3-r +real_1 +real_0 +real_0)
		(Vec3-r +real_0 +real_1 +real_0)
		(Vec3-r +real_0 +real_0 +real_1)))

(defun Mat4x4-unity ()
	(list (Vec4-r +real_1 +real_0 +real_0 +real_0)
		(Vec4-r +real_0 +real_1 +real_0 +real_0)
		(Vec4-r +real_0 +real_0 +real_1 +real_0)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat3x3-rotx (a)
	(list (Vec3-r +real_1 +real_0 +real_0)
		(Vec3-r +real_0 (cos a) (* (sin a) +real_-1))
		(Vec3-r +real_0 (sin a) (cos a))))

(defun Mat3x3-roty (a)
	(list (Vec3-r (cos a) +real_0 (sin a))
		(Vec3-r +real_0 +real_1 +real_0)
		(Vec3-r (* (sin a) +real_-1) +real_0 (cos a))))

(defun Mat3x3-rotz (a)
	(list (Vec3-r (cos a) (* (sin a) +real_-1) +real_0)
		(Vec3-r (sin a) (cos a) +real_0)
		(Vec3-r +real_0 +real_0 +real_1)))

(defun Mat3x3-scale (s)
	(list (Vec3-r s +real_0 +real_0)
		(Vec3-r +real_0 s +real_0)
		(Vec3-r +real_0 +real_0 s)))

(defun Mat4x4-rotx (a)
	(list (Vec4-r +real_1 +real_0 +real_0 +real_0)
		(Vec4-r +real_0 (cos a) (* (sin a) +real_-1) +real_0)
		(Vec4-r +real_0 (sin a) (cos a) +real_0)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-roty (a)
	(list (Vec4-r (cos a) +real_0 (sin a) +real_0)
		(Vec4-r +real_0 +real_1 +real_0 +real_0)
		(Vec4-r (* (sin a) +real_-1) +real_0 (cos a) +real_0)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-rotz (a)
	(list (Vec4-r (cos a) (* (sin a) +real_-1) +real_0 +real_0)
		(Vec4-r (sin a) (cos a) +real_0 +real_0)
		(Vec4-r +real_0 +real_0 +real_1 +real_0)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-translate (x y z)
	(list (Vec4-r +real_1 +real_0 +real_0 x)
		(Vec4-r +real_0 +real_1 +real_0 y)
		(Vec4-r +real_0 +real_0 +real_1 z)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-scale (x y z)
	(list (Vec4-r x +real_0 +real_0 +real_0)
		(Vec4-r +real_0 y +real_0 +real_0)
		(Vec4-r +real_0 +real_0 z +real_0)
		(Vec4-r +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-frustum (left right top bottom near far)
	(list (Vec4-r (/ (* +real_2 near) (- right left)) +real_0 (/ (+ left right) (- right left)) +real_0)
		(Vec4-r +real_0 (/ (* +real_2 near) (- top bottom)) (/ (+ top bottom) (- top bottom)) +real_0)
		(Vec4-r +real_0 +real_0 (/ (+ near far) (- near far)) (/ (* +real_2 near far) (- near far)))
		(Vec4-r +real_0 +real_0 +real_-1 +real_0)))

(defun mat3x3-vec3-mul (m v)
	(Vec3-r (vec-dot v (elem-get +mat3x3_r0 m))
		(vec-dot v (elem-get +mat3x3_r1 m))
		(vec-dot v (elem-get +mat3x3_r2 m))))

(defun mat4x4-vec3-mul (m v)
	(Vec3-r (vec-dot v (elem-get +mat4x4_r0 m))
		(vec-dot v (elem-get +mat4x4_r1 m))
		(vec-dot v (elem-get +mat4x4_r2 m))))

(defun mat4x4-vec4-mul (m v)
	(Vec4-r (vec-dot v (elem-get +mat4x4_r0 m))
		(vec-dot v (elem-get +mat4x4_r1 m))
		(vec-dot v (elem-get +mat4x4_r2 m))
		(vec-dot v (elem-get +mat4x4_r3 m))))

(defun mat3x3-mul (ma mb)
	; (mat3x3-mul mat3x3_a mat3x3_b) -> mat3x3
	(bind '(mar0 mar1 mar2) ma)
	(bind '(mbr0 mbr1 mbr2) mb)
	(defq mbc0 (Vec3-r (elem-get 0 mbr0) (elem-get 0 mbr1) (elem-get 0 mbr2))
		mbc1 (Vec3-r (elem-get 1 mbr0) (elem-get 1 mbr1) (elem-get 1 mbr2))
		mbc2 (Vec3-r (elem-get 2 mbr0) (elem-get 2 mbr1) (elem-get 2 mbr2)))
	(list (Vec3-r (vec-dot mar0 mbc0) (vec-dot mar0 mbc1) (vec-dot mar0 mbc2))
		(Vec3-r (vec-dot mar1 mbc0) (vec-dot mar1 mbc1) (vec-dot mar1 mbc2))
		(Vec3-r (vec-dot mar2 mbc0) (vec-dot mar2 mbc1) (vec-dot mar2 mbc2))))

(defun mat4x4-mul (ma mb)
	; (mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
	(bind '(mar0 mar1 mar2 mar3) ma)
	(bind '(mbr0 mbr1 mbr2 mbr3) mb)
	(defq mbc0 (Vec4-r (elem-get 0 mbr0) (elem-get 0 mbr1) (elem-get 0 mbr2) (elem-get 0 mbr3))
		mbc1 (Vec4-r (elem-get 1 mbr0) (elem-get 1 mbr1) (elem-get 1 mbr2) (elem-get 1 mbr3))
		mbc2 (Vec4-r (elem-get 2 mbr0) (elem-get 2 mbr1) (elem-get 2 mbr2) (elem-get 2 mbr3))
		mbc3 (Vec4-r (elem-get 3 mbr0) (elem-get 3 mbr1) (elem-get 3 mbr2) (elem-get 3 mbr3)))
	(list (Vec4-r (vec-dot mar0 mbc0) (vec-dot mar0 mbc1) (vec-dot mar0 mbc2) (vec-dot mar0 mbc3))
		(Vec4-r (vec-dot mar1 mbc0) (vec-dot mar1 mbc1) (vec-dot mar1 mbc2) (vec-dot mar1 mbc3))
		(Vec4-r (vec-dot mar2 mbc0) (vec-dot mar2 mbc1) (vec-dot mar2 mbc2) (vec-dot mar2 mbc3))
		(Vec4-r (vec-dot mar3 mbc0) (vec-dot mar3 mbc1) (vec-dot mar3 mbc2) (vec-dot mar3 mbc3))))

(defun mat4x4-invert (m)
	; (mat4x4-invert mat4x4) -> mat4x4
	(bind '(mr0 mr1 mr2 mr3) m)
	(bind '(m00 m01 m02 m03) mr0)
	(bind '(m10 m11 m12 m13) mr1)
	(bind '(m20 m21 m22 m23) mr2)
	(bind '(m30 m31 m32 m33) mr3)
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
	(list
		(vec-scale (Vec4-r
			(+ (* m11 a2323) (neg (* m12 a1323)) (* m13 a1223))
			(- (* m02 a1323) (* m01 a2323) (* m03 a1223))
			(+ (* m01 a2313) (neg (* m02 a1313)) (* m03 a1213))
			(- (* m02 a1312) (* m01 a2312) (* m03 a1212))) det)
		(vec-scale (Vec4-r
			(- (* m12 a0323) (* m10 a2323) (* m13 a0223))
			(+ (* m00 a2323) (neg (* m02 a0323)) (* m03 a0223))
			(- (* m02 a0313) (* m00 a2313) (* m03 a0213))
			(+ (* m00 a2312) (neg (* m02 a0312)) (* m03 a0212))) det)
		(vec-scale (Vec4-r
			(+ (* m10 a1323) (neg (* m11 a0323)) (* m13 a0123))
			(- (* m01 a0323) (* m00 a1323) (* m03 a0123))
			(+ (* m00 a1313) (neg (* m01 a0313)) (* m03 a0113))
			(- (* m01 a0312) (* m00 a1312) (* m03 a0112))) det)
		(vec-scale (Vec4-r
			(- (* m11 a0223) (* m10 a1223) (* m12 a0123))
			(+ (* m00 a1223) (neg (* m01 a0223)) (* m02 a0123))
			(- (* m01 a0213) (* m00 a1213) (* m02 a0113))
			(+ (* m00 a1212) (neg (* m01 a0212)) (* m02 a0112))) det)))

(defun print-mat (m)
	(each (# (print-vec %0)) m))
