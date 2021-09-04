;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "lib/math/vector.inc")

;useful real constants
(defmacro def_real (c s e f)
	`(bind (map (# (sym (str ,c %0))) (range ,s ,e)) (map ,f (range ,s ,e))))
;[0:10]
(def_real "+real_" 0 11 (# (i2r %0)))
;[-1:-10]
(def_real "+real_" -10 0 (# (i2r %0)))
;[1/2:1/20]
(def_real "+real_1/" 2 21 (# (/ (const (i2r 1)) (i2r %0))))
;[-1/2:-1/20]
(def_real "+real_-1/" 2 21 (# (/ (const (i2r -1)) (i2r %0))))
;pi and temp vec3, vec4
(defq +real_pi (f2r +fp_pi) +real_2pi (f2r +fp_2pi))
(undef (env) 'def_real)

(enums +vec3 0
	(enum x y z))

(enums +vec4 0
	(enum x y z w))

(enums +mat3x3 0
	(enum r0 r1 r2))

(enums +mat4x4 0
	(enum r0 r1 r2 r3))

(defun Vec3-r (x y z)
	(reals x y z))

(defun Vec3-f (x y z)
	(reals (f2r x) (f2r y) (f2r z)))

(defun Vec4-r (x y z w)
	(reals x y z w))

(defun Vec4-f (x y z w)
	(reals (f2r x) (f2r y) (f2r z) w))

(defun Mat3x3-unity ()
	(list (reals +real_1 +real_0 +real_0)
		(reals +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_1)))

(defun Mat4x4-unity ()
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat3x3-rotx (a)
	(list (reals +real_1 +real_0 +real_0)
		(reals +real_0 (cos a) (* (sin a) +real_-1))
		(reals +real_0 (sin a) (cos a))))

(defun Mat3x3-roty (a)
	(list (reals (cos a) +real_0 (sin a))
		(reals +real_0 +real_1 +real_0)
		(reals (* (sin a) +real_-1) +real_0 (cos a))))

(defun Mat3x3-rotz (a)
	(list (reals (cos a) (* (sin a) +real_-1) +real_0)
		(reals (sin a) (cos a) +real_0)
		(reals +real_0 +real_0 +real_1)))

(defun Mat3x3-scale (s)
	(list (reals s +real_0 +real_0)
		(reals +real_0 s +real_0)
		(reals +real_0 +real_0 s)))

(defun Mat4x4-rotx (a)
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 (cos a) (* (sin a) +real_-1) +real_0)
		(reals +real_0 (sin a) (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-roty (a)
	(list (reals (cos a) +real_0 (sin a) +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals (* (sin a) +real_-1) +real_0 (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-rotz (a)
	(list (reals (cos a) (* (sin a) +real_-1) +real_0 +real_0)
		(reals (sin a) (cos a) +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-translate (x y z)
	(list (reals +real_1 +real_0 +real_0 x)
		(reals +real_0 +real_1 +real_0 y)
		(reals +real_0 +real_0 +real_1 z)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-scale (x y z)
	(list (reals x +real_0 +real_0 +real_0)
		(reals +real_0 y +real_0 +real_0)
		(reals +real_0 +real_0 z +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun Mat4x4-frustum (left right top bottom near far)
	(list (reals (/ (* +real_2 near) (- right left)) +real_0 (/ (+ left right) (- right left)) +real_0)
		(reals +real_0 (/ (* +real_2 near) (- top bottom)) (/ (+ top bottom) (- top bottom)) +real_0)
		(reals +real_0 +real_0 (/ (+ near far) (- near far)) (/ (* +real_2 near far) (- near far)))
		(reals +real_0 +real_0 +real_-1 +real_0)))

(defun mat3x3-vec3-mul (m v)
	(reals (vec-dot v (elem +mat3x3_r0 m))
		(vec-dot v (elem +mat3x3_r1 m))
		(vec-dot v (elem +mat3x3_r2 m))))

(defun mat4x4-vec3-mul (m v)
	(reals (vec-dot v (elem +mat4x4_r0 m))
		(vec-dot v (elem +mat4x4_r1 m))
		(vec-dot v (elem +mat4x4_r2 m))))

(defun mat4x4-vec4-mul (m v)
	(reals (vec-dot v (elem +mat4x4_r0 m))
		(vec-dot v (elem +mat4x4_r1 m))
		(vec-dot v (elem +mat4x4_r2 m))
		(vec-dot v (elem +mat4x4_r3 m))))

(defun mat3x3-mul (ma mb)
	; (mat3x3-mul mat3x3_a mat3x3_b) -> mat3x3
	(bind '(mar0 mar1 mar2) ma)
	(bind '(mbr0 mbr1 mbr2) mb)
	(defq mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2)))
	(list (reals (vec-dot mar0 mbc0) (vec-dot mar0 mbc1) (vec-dot mar0 mbc2))
		(reals (vec-dot mar1 mbc0) (vec-dot mar1 mbc1) (vec-dot mar1 mbc2))
		(reals (vec-dot mar2 mbc0) (vec-dot mar2 mbc1) (vec-dot mar2 mbc2))))

(defun mat4x4-mul (ma mb)
	; (mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
	(bind '(mar0 mar1 mar2 mar3) ma)
	(bind '(mbr0 mbr1 mbr2 mbr3) mb)
	(defq mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2) (elem 0 mbr3))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2) (elem 1 mbr3))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2) (elem 2 mbr3))
		mbc3 (reals (elem 3 mbr0) (elem 3 mbr1) (elem 3 mbr2) (elem 3 mbr3)))
	(list (reals (vec-dot mar0 mbc0) (vec-dot mar0 mbc1) (vec-dot mar0 mbc2) (vec-dot mar0 mbc3))
		(reals (vec-dot mar1 mbc0) (vec-dot mar1 mbc1) (vec-dot mar1 mbc2) (vec-dot mar1 mbc3))
		(reals (vec-dot mar2 mbc0) (vec-dot mar2 mbc1) (vec-dot mar2 mbc2) (vec-dot mar2 mbc3))
		(reals (vec-dot mar3 mbc0) (vec-dot mar3 mbc1) (vec-dot mar3 mbc2) (vec-dot mar3 mbc3))))

(defun bounding-box (verts fnc)
	; (bounding-box verts vec3-extract-fnc) -> (min_v3 max_v3)
	(defq min_v (cat (fnc (elem 0 verts))) max_v (cat min_v))
	(each! 1 -1 (#
		(vec-min (defq v (fnc %0)) min_v min_v)
		(vec-max v max_v max_v)) (list verts))
	(list min_v max_v))

(defun bounding-sphere (verts fnc)
	; (bounding-sphere verts vec3-extract-fnc) -> (center_v3 radius)
	(bind '(min_v max_v) (bounding-box verts fnc))
	(defq center (vec-scale (vec-add min_v max_v) +real_1/2)
		radius (* (reduce max (vec-sub max_v min_v)) +real_1/2))
	(each (#
		(defq p (fnc %0) pv (vec-sub p center) rv (vec-length pv))
		(when (> rv radius)
			(setq radius (* (+ radius rv) +real_1/2))
			(vec-sub p (vec-scale (vec-norm pv) radius +reals_tmp3) center))) verts)
	(list center radius))

(defun print-vec (v)
	(prin (join (map (# (pad (r2f %0) 8)) v) " "))
	(print))

(defun print-mat (m)
	(each (# (print-vec %0)) m))
