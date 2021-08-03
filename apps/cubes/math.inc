;;;;;;
; math
;;;;;;

(import "lib/math/math.inc")

(defq +real_0 (i2r 0) +real_1 (i2r 1) +real_2 (i2r 2)
	+real_3 (i2r 3) +real_4 (i2r 4) +real_5 (i2r 5) +real_10 (i2r 10)
	+real_-1 (i2r -1) +real_-2 (i2r -2)
	+real_1/2 (f2r 0.5) +real_3/4 (f2r 0.75)
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

(defun matrix-frustrum (left right top bottom near far)
	(list (reals (/ (* +real_2 near) (- right left)) +real_0 +real_0 +real_0)
		(reals +real_0 (/ (* +real_2 near) (- bottom top)) +real_0 +real_0)
		(reals (/ (+ left right) (- right left)) (/ (+ top bottom) (- bottom top))
			(/ (+ near far) (- far near)) +real_-1)
		(reals +real_0 +real_0 (/ (* +real_2 near far) (- far near)) +real_0)))

(defun vertex-mul (m vs)
	(map (lambda (v)
		(reals (vec-sum (vec-mul (elem +matrix_r0 m) v +reals_tmp))
			(vec-sum (vec-mul (elem +matrix_r1 m) v +reals_tmp))
			(vec-sum (vec-mul (elem +matrix_r2 m) v +reals_tmp))
			(vec-sum (vec-mul (elem +matrix_r3 m) v +reals_tmp)))) vs))

(defun matrix-mul (ma mb)
	(defq mar0 (elem +matrix_r0 ma) mar1 (elem +matrix_r1 ma) mar2 (elem +matrix_r2 ma) mar3 (elem +matrix_r3 ma)
		mbr0 (elem +matrix_r0 mb) mbr1 (elem +matrix_r1 mb) mbr2 (elem +matrix_r2 mb) mbr3 (elem +matrix_r3 mb)
		mbc0 (reals (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2) (elem 0 mbr3))
		mbc1 (reals (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2) (elem 1 mbr3))
		mbc2 (reals (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2) (elem 2 mbr3))
		mbc3 (reals (elem 3 mbr0) (elem 3 mbr1) (elem 3 mbr2) (elem 3 mbr3)))
	(list (reals (vec-sum (vec-mul mar0 mbc0 +reals_tmp))
			 (vec-sum (vec-mul mar0 mbc1 +reals_tmp))
			 (vec-sum (vec-mul mar0 mbc2 +reals_tmp))
			 (vec-sum (vec-mul mar0 mbc3 +reals_tmp)))
		(reals (vec-sum (vec-mul mar1 mbc0 +reals_tmp))
			 (vec-sum (vec-mul mar1 mbc1 +reals_tmp))
			 (vec-sum (vec-mul mar1 mbc2 +reals_tmp))
			 (vec-sum (vec-mul mar1 mbc3 +reals_tmp)))
		(reals (vec-sum (vec-mul mar2 mbc0 +reals_tmp))
			 (vec-sum (vec-mul mar2 mbc1 +reals_tmp))
			 (vec-sum (vec-mul mar2 mbc2 +reals_tmp))
			 (vec-sum (vec-mul mar2 mbc3 +reals_tmp)))
		(reals (vec-sum (vec-mul mar3 mbc0 +reals_tmp))
			 (vec-sum (vec-mul mar3 mbc1 +reals_tmp))
			 (vec-sum (vec-mul mar3 mbc2 +reals_tmp))
			 (vec-sum (vec-mul mar3 mbc3 +reals_tmp)))))

(defun vertex-cloud (num)
	;array of random verts
	(defq out (cap num (list)))
	(while (> (setq num (dec num)) -1)
		(push out (vertex-f
			(- (random 2.0) 1.0)
			(- (random 2.0) 1.0)
			(- (random 2.0) 1.0)))) out)

(defun vertex-clip (vs)
	;clip verts
	(reduce (lambda (out v)
		(bind '(x y z w) v)
		(and (> z +real_0) (> x +real_-1) (< x +real_1) (> y +real_-1) (< y +real_1)
			(push out v))
		out) vs (cap (length vs) (list))))
