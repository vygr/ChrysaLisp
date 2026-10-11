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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 3x2 matrix of the plane, six fixeds, a b tx c d ty:
;
;	x' = a*x + b*y + tx
;	y' = c*x + d*y + ty
;
; It is what (path-transform) takes. :nil is a matrix too, that of no
; change, and every function here takes it as one and does no work for it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Mat3x2 (&optional a b tx c d ty)
	; (Mat3x2 [a b tx c d ty]) -> matrix
	(fixeds (ifn a 1.0) (ifn b 0.0) (ifn tx 0.0) (ifn c 0.0) (ifn d 1.0) (ifn ty 0.0)))

(defun Mat3x2-mul (ma mb)
	; (Mat3x2-mul ma mb) -> matrix | :nil
	;the one matrix that does what mb does and then what ma does
	(cond
		((not ma) mb)
		((not mb) ma)
		(:t (bind '(a0 a1 a2 a3 a4 a5) ma)
			(bind '(b0 b1 b2 b3 b4 b5) mb)
			(fixeds (+ (* a0 b0) (* a1 b3)) (+ (* a0 b1) (* a1 b4)) (+ (* a0 b2) (* a1 b5) a2)
				(+ (* a3 b0) (* a4 b3)) (+ (* a3 b1) (* a4 b4)) (+ (* a3 b2) (* a4 b5) a5)))))

(defun Mat3x2-translate (tx ty)
	; (Mat3x2-translate tx ty) -> matrix
	(fixeds 1.0 0.0 (n2f tx) 0.0 1.0 (n2f ty)))

(defun Mat3x2-scale (sx &optional sy cx cy)
	; (Mat3x2-scale sx [sy cx cy]) -> matrix
	;about a point, default the origin
	(setd sy sx cx 0.0 cy 0.0)
	(setq sx (n2f sx) sy (n2f sy) cx (n2f cx) cy (n2f cy))
	(fixeds sx 0.0 (- cx (* sx cx)) 0.0 sy (- cy (* sy cy))))

(defun Mat3x2-rot (angle &optional cx cy)
	; (Mat3x2-rot angle [cx cy]) -> matrix
	;by an angle in radians, the way from the x axis to the y axis, about
	;a point, default the origin
	(setd cx 0.0 cy 0.0)
	(setq cx (n2f cx) cy (n2f cy))
	(defq c (cos (n2f angle)) s (sin (n2f angle)))
	(fixeds c (neg s) (- cx (- (* c cx) (* s cy))) s c (- cy (+ (* s cx) (* c cy)))))

(defun Mat3x2-invert (m)
	; (Mat3x2-invert m) -> matrix | :nil
	;the matrix that undoes it. One that flattens everything to a line
	;has none, and that of no change is given
	(when m
		(bind '(a b tx c d ty) m)
		(defq det (- (* a d) (* b c)))
		(if (= det 0.0) (Mat3x2)
			(progn
				(defq ia (/ d det) ib (/ (neg b) det) ic (/ (neg c) det) id (/ a det))
				(fixeds ia ib (neg (+ (* ia tx) (* ib ty))) ic id (neg (+ (* ic tx) (* id ty))))))))

(defun Mat3x2-point (m x y)
	; (Mat3x2-point m x y) -> (x y)
	(setq x (n2f x) y (n2f y))
	(if m
		(progn
			(bind '(a b tx c d ty) m)
			(list (+ (* a x) (* b y) tx) (+ (* c x) (* d y) ty)))
		(list x y)))

(defun Mat3x2-box (box m)
	; (Mat3x2-box box m) -> (x y x1 y1)
	;the box round a box that a matrix has moved. Each side is the least
	;or the most of what each of its two parts can come to
	(cond
		(m (bind '(x y x1 y1) box)
			(bind '(a b tx c d ty) m)
			(defq ax (* a x) ax1 (* a x1) by (* b y) by1 (* b y1)
				cx (* c x) cx1 (* c x1) dy (* d y) dy1 (* d y1))
			(list (+ (min ax ax1) (min by by1) tx) (+ (min cx cx1) (min dy dy1) ty)
				(+ (max ax ax1) (max by by1) tx) (+ (max cx cx1) (max dy dy1) ty)))
		(:t box)))
