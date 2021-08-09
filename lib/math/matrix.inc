;;;;;;
; math
;;;;;;

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
(defq +real_pi (f2r +fp_pi) +real_2pi (f2r +fp_2pi)
	+reals_tmp3 (reals +real_0 +real_0 +real_0)
	+reals_tmp4 (reals +real_0 +real_0 +real_0 +real_0))

(enums +vec3 0
	(enum x y z))

(enums +vec4 0
	(enum x y z w))

(enums +mat3x3 0
	(enum r0 r1 r2))

(enums +mat4x4 0
	(enum r0 r1 r2 r3))

(defun vec4-r (x y z)
	(reals x y z +real_1))

(defun vec4-f (x y z)
	(reals (f2r x) (f2r y) (f2r z) +real_1))

(defun mat3x3-unity ()
	(list (reals +real_1 +real_0 +real_0)
		(reals +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_1)))

(defun mat4x4-unity ()
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun mat3x3-rotx (a)
	(list (reals +real_1 +real_0 +real_0)
		(reals +real_0 (cos a) (* (sin a) +real_-1))
		(reals +real_0 (sin a) (cos a))))

(defun mat3x3-roty (a)
	(list (reals (cos a) +real_0 (sin a))
		(reals +real_0 +real_1 +real_0)
		(reals (* (sin a) +real_-1) +real_0 (cos a))))

(defun mat3x3-rotz (a)
	(list (reals (cos a) (* (sin a) +real_-1) +real_0)
		(reals (sin a) (cos a) +real_0)
		(reals +real_0 +real_0 +real_1)))

(defun mat4x4-rotx (a)
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 (cos a) (* (sin a) +real_-1) +real_0)
		(reals +real_0 (sin a) (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun mat4x4-roty (a)
	(list (reals (cos a) +real_0 (sin a) +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals (* (sin a) +real_-1) +real_0 (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun mat4x4-rotz (a)
	(list (reals (cos a) (* (sin a) +real_-1) +real_0 +real_0)
		(reals (sin a) (cos a) +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun mat4x4-translate (x y z)
	(list (reals +real_1 +real_0 +real_0 x)
		(reals +real_0 +real_1 +real_0 y)
		(reals +real_0 +real_0 +real_1 z)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun mat4x4-frustum (left right top bottom near far)
	(list (reals (/ (* +real_2 near) (- right left)) +real_0 (/ (+ left right) (- right left)) +real_0)
		(reals +real_0 (/ (* +real_2 near) (- top bottom)) (/ (+ top bottom) (- top bottom)) +real_0)
		(reals +real_0 +real_0 (/ (+ near far) (- near far)) (/ (* +real_2 near far) (- near far)))
		(reals +real_0 +real_0 +real_-1 +real_0)))

(defun vec3-mul (m v)
	(reals (vec-dot (elem +mat3x3_r0 m) v +reals_tmp3)
		(vec-dot (elem +mat3x3_r1 m) v +reals_tmp3)
		(vec-dot (elem +mat3x3_r2 m) v +reals_tmp3)))

(defun vec4-mul (m v)
	(reals (vec-dot (elem +mat4x4_r0 m) v +reals_tmp4)
		(vec-dot (elem +mat4x4_r1 m) v +reals_tmp4)
		(vec-dot (elem +mat4x4_r2 m) v +reals_tmp4)
		(vec-dot (elem +mat4x4_r3 m) v +reals_tmp4)))

(defun mat3x3-mul (ma mb)
	; (mat3x3-mul mat3x3_a mat3x3_b) -> mat3x3
	(defq mar0 (elem +mat3x3_r0 ma) mar1 (elem +mat3x3_r1 ma) mar2 (elem +mat3x3_r2 ma)
		mbr0 (elem +mat3x3_r0 mb) mbr1 (elem +mat3x3_r1 mb) mbr2 (elem +mat3x3_r2 mb)
		mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2))
	(list (reals (vec-dot mar0 mbc0 +reals_tmp3)
			 (vec-dot mar0 mbc1 +reals_tmp3)
			 (vec-dot mar0 mbc2 +reals_tmp3))
		(reals (vec-dot mar1 mbc0 +reals_tmp3)
			 (vec-dot mar1 mbc1 +reals_tmp3)
			 (vec-dot mar1 mbc2 +reals_tmp3))
		(reals (vec-dot mar2 mbc0 +reals_tmp3)
			 (vec-dot mar2 mbc1 +reals_tmp3)
			 (vec-dot mar2 mbc2 +reals_tmp3)))))

(defun mat4x4-mul (ma mb)
	; (mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
	(defq mar0 (elem +mat4x4_r0 ma) mar1 (elem +mat4x4_r1 ma)
		mar2 (elem +mat4x4_r2 ma) mar3 (elem +mat4x4_r3 ma)
		mbr0 (elem +mat4x4_r0 mb) mbr1 (elem +mat4x4_r1 mb)
		mbr2 (elem +mat4x4_r2 mb) mbr3 (elem +mat4x4_r3 mb)
		mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2) (elem 0 mbr3))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2) (elem 1 mbr3))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2) (elem 2 mbr3))
		mbc3 (reals (elem 3 mbr0) (elem 3 mbr1) (elem 3 mbr2) (elem 3 mbr3)))
	(list (reals (vec-dot mar0 mbc0 +reals_tmp4)
			 (vec-dot mar0 mbc1 +reals_tmp4)
			 (vec-dot mar0 mbc2 +reals_tmp4)
			 (vec-dot mar0 mbc3 +reals_tmp4))
		(reals (vec-dot mar1 mbc0 +reals_tmp4)
			 (vec-dot mar1 mbc1 +reals_tmp4)
			 (vec-dot mar1 mbc2 +reals_tmp4)
			 (vec-dot mar1 mbc3 +reals_tmp4))
		(reals (vec-dot mar2 mbc0 +reals_tmp4)
			 (vec-dot mar2 mbc1 +reals_tmp4)
			 (vec-dot mar2 mbc2 +reals_tmp4)
			 (vec-dot mar2 mbc3 +reals_tmp4))
		(reals (vec-dot mar3 mbc0 +reals_tmp4)
			 (vec-dot mar3 mbc1 +reals_tmp4)
			 (vec-dot mar3 mbc2 +reals_tmp4)
			 (vec-dot mar3 mbc3 +reals_tmp4))))

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
		(defq p (fnc %0) pv (vec-sub p center) rv (vec-length pv +reals_tmp3))
		(when (> rv radius)
			(setq radius (* (+ radius rv) +real_1/2))
			(vec-sub p (vec-scale (vec-norm pv) radius +reals_tmp3) center))) verts)
	(list center radius))
