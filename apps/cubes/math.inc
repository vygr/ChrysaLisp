;;;;;;
; math
;;;;;;

(import "lib/math/math.inc")

(defq +real_0 (i2r 0) +real_1 (i2r 1) +real_2 (i2r 2)
	+real_3 (i2r 3) +real_4 (i2r 4) +real_5 (i2r 5)
	+real_6 (i2r 6) +real_7 (i2r 7) +real_8 (i2r 8)
	+real_9 (i2r 9) +real_10 (i2r 10)
	+real_-1 (i2r -1) +real_-2 (i2r -2) +real_-3 (i2r -3)
	+real_1/2 (f2r 0.5) +real_1/4 (f2r 0.25) +real_3/4 (f2r 0.75)
	+reals_tmp (reals +real_0 +real_0 +real_0 +real_0))

(enums +vertex 0
	(enum x y z w))

(enums +matrix 0
	(enum r0 r1 r2 r3))

(defun vertex-f (x y z)
	(reals (f2r x) (f2r y) (f2r z) +real_1))

(defun matrix-unity ()
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun matrix-rotx (a)
	(list (reals +real_1 +real_0 +real_0 +real_0)
		(reals +real_0 (cos a) (* (sin a) +real_-1) +real_0)
		(reals +real_0 (sin a) (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun matrix-roty (a)
	(list (reals (cos a) +real_0 (sin a) +real_0)
		(reals +real_0 +real_1 +real_0 +real_0)
		(reals (* (sin a) +real_-1) +real_0 (cos a) +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun matrix-rotz (a)
	(list (reals (cos a) (* (sin a) +real_-1) +real_0 +real_0)
		(reals (sin a) (cos a) +real_0 +real_0)
		(reals +real_0 +real_0 +real_1 +real_0)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun matrix-translate (x y z)
	(list (reals +real_1 +real_0 +real_0 x)
		(reals +real_0 +real_1 +real_0 y)
		(reals +real_0 +real_0 +real_1 z)
		(reals +real_0 +real_0 +real_0 +real_1)))

(defun matrix-frustum (left right top bottom near far)
	(list (reals (/ (* +real_2 near) (- right left)) +real_0 (/ (+ left right) (- right left)) +real_0)
		(reals +real_0 (/ (* +real_2 near) (- top bottom)) (/ (+ top bottom) (- top bottom)) +real_0)
		(reals +real_0 +real_0 (/ (+ near far) (- near far)) (/ (* +real_2 near far) (- near far)))
		(reals +real_0 +real_0 +real_-1 +real_0)))

(defun vertex-mul (m vs)
	(map (lambda (v)
		(reals (vec-dot (elem +matrix_r0 m) v +reals_tmp)
			(vec-dot (elem +matrix_r1 m) v +reals_tmp)
			(vec-dot (elem +matrix_r2 m) v +reals_tmp)
			(vec-dot (elem +matrix_r3 m) v +reals_tmp))) vs))

(defun matrix-mul (ma mb)
	(defq mar0 (elem +matrix_r0 ma) mar1 (elem +matrix_r1 ma)
		mar2 (elem +matrix_r2 ma) mar3 (elem +matrix_r3 ma)
		mbr0 (elem +matrix_r0 mb) mbr1 (elem +matrix_r1 mb)
		mbr2 (elem +matrix_r2 mb) mbr3 (elem +matrix_r3 mb)
		mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2) (elem 0 mbr3))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2) (elem 1 mbr3))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2) (elem 2 mbr3))
		mbc3 (reals (elem 3 mbr0) (elem 3 mbr1) (elem 3 mbr2) (elem 3 mbr3)))
	(list (reals (vec-dot mar0 mbc0 +reals_tmp)
			 (vec-dot mar0 mbc1 +reals_tmp)
			 (vec-dot mar0 mbc2 +reals_tmp)
			 (vec-dot mar0 mbc3 +reals_tmp))
		(reals (vec-dot mar1 mbc0 +reals_tmp)
			 (vec-dot mar1 mbc1 +reals_tmp)
			 (vec-dot mar1 mbc2 +reals_tmp)
			 (vec-dot mar1 mbc3 +reals_tmp))
		(reals (vec-dot mar2 mbc0 +reals_tmp)
			 (vec-dot mar2 mbc1 +reals_tmp)
			 (vec-dot mar2 mbc2 +reals_tmp)
			 (vec-dot mar2 mbc3 +reals_tmp))
		(reals (vec-dot mar3 mbc0 +reals_tmp)
			 (vec-dot mar3 mbc1 +reals_tmp)
			 (vec-dot mar3 mbc2 +reals_tmp)
			 (vec-dot mar3 mbc3 +reals_tmp))))
