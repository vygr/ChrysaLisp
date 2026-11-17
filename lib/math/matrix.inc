;;;;;;;;;;;;;
; matrix math
;;;;;;;;;;;;;

(import "./vector.inc")
(import "gui/path/lisp.inc")

(defun mat4x4-unity ()
	(doubles +double_1 +double_0 +double_0 +double_0
		+double_0 +double_1 +double_0 +double_0
		+double_0 +double_0 +double_1 +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-rotx (a)
	(defq s (sin a) c (cos a))
	(doubles +double_1 +double_0 +double_0 +double_0
		+double_0 c (neg s) +double_0
		+double_0 s c +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-roty (a)
	(defq s (sin a) c (cos a))
	(doubles c +double_0 s +double_0
		+double_0 +double_1 +double_0 +double_0
		(neg s) +double_0 c +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-rotz (a)
	(defq s (sin a) c (cos a))
	(doubles c (neg s) +double_0 +double_0
		s c +double_0 +double_0
		+double_0 +double_0 +double_1 +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-translate (x y z)
	(doubles +double_1 +double_0 +double_0 x
		+double_0 +double_1 +double_0 y
		+double_0 +double_0 +double_1 z
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-scale (x y z)
	(doubles x +double_0 +double_0 +double_0
		+double_0 y +double_0 +double_0
		+double_0 +double_0 z +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun mat4x4-frustum (left right top bottom near far)
	(defq rl (recip (- right left))
		tb (recip (- top bottom))
		nf (recip (- near far)))
	(doubles (* (* +double_2 near) rl) +double_0 (* (+ left right) rl) +double_0
		+double_0 (* (* +double_2 near) tb) (* (+ top bottom) tb) +double_0
		+double_0 +double_0 (* (+ near far) nf) (* (* (* +double_2 near) far) nf)
		+double_0 +double_0 +double_-1 +double_0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 3x2 matrix of the plane, six fixeds, a b tx c d ty:
;
;	x' = a*x + b*y + tx
;	y' = c*x + d*y + ty
;
; It is what (mat3x2-mul) and (mat3x2-vec2-mul) take. The matrix of no
; change is +mat3x2_unity, held once: one that is it can be told by eql,
; and work for it left out. It is never written into.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mat3x2 (&optional a b tx c d ty)
	; (mat3x2 [a b tx c d ty]) -> matrix
	(fixeds (ifn a 1.0) (ifn b 0.0) (ifn tx 0.0) (ifn c 0.0) (ifn d 1.0) (ifn ty 0.0)))

;the matrix of no change, the one object
(defq +mat3x2_unity (mat3x2))

(defun mat3x2-translate (tx ty)
	; (mat3x2-translate tx ty) -> matrix
	(fixeds 1.0 0.0 (n2q tx) 0.0 1.0 (n2q ty)))

(defun mat3x2-scale (sx &optional sy cx cy)
	; (mat3x2-scale sx [sy cx cy]) -> matrix
	;about a point, default the origin
	(setd sy sx cx 0.0 cy 0.0)
	(setq sx (n2q sx) sy (n2q sy) cx (n2q cx) cy (n2q cy))
	(fixeds sx 0.0 (- cx (* sx cx)) 0.0 sy (- cy (* sy cy))))

(defun mat3x2-rot (angle &optional cx cy)
	; (mat3x2-rot angle [cx cy]) -> matrix
	;by an angle in radians, the way from the x axis to the y axis, about
	;a point, default the origin
	(setd cx 0.0 cy 0.0)
	(setq cx (n2q cx) cy (n2q cy))
	(defq c (cos (n2q angle)) s (sin (n2q angle)))
	(fixeds c (neg s) (- cx (- (* c cx) (* s cy))) s c (- cy (+ (* s cx) (* c cy)))))

(defun mat3x2-skew (ax &optional ay)
	; (mat3x2-skew ax [ay]) -> matrix
	;by angles in radians, x leaning along y by ax and y along x by ay,
	;default 0, as svg's skewX and skewY: x' = x + tan(ax)*y
	(setd ay 0.0)
	(setq ax (n2q ax) ay (n2q ay))
	(fixeds 1.0 (/ (sin ax) (cos ax)) 0.0 (/ (sin ay) (cos ay)) 1.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the 3x4 matrix of a flat thing in space, twelve fixeds, by rows. A point of
; the thing has z 0:
;
;	x' = m0*x + m1*y + m3
;	y' = m4*x + m5*y + m7
;	z' = m8*x + m9*y + m11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mat3x4-mul (ma mb)
	; (mat3x4-mul ma mb) -> matrix
	;the one matrix that does what mb does and then what ma does
	(bind '(a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11) ma)
	(bind '(b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11) mb)
	(fixeds
		(+ (* a0 b0) (* a1 b4) (* a2 b8)) (+ (* a0 b1) (* a1 b5) (* a2 b9))
		(+ (* a0 b2) (* a1 b6) (* a2 b10)) (+ (* a0 b3) (* a1 b7) (* a2 b11) a3)
		(+ (* a4 b0) (* a5 b4) (* a6 b8)) (+ (* a4 b1) (* a5 b5) (* a6 b9))
		(+ (* a4 b2) (* a5 b6) (* a6 b10)) (+ (* a4 b3) (* a5 b7) (* a6 b11) a7)
		(+ (* a8 b0) (* a9 b4) (* a10 b8)) (+ (* a8 b1) (* a9 b5) (* a10 b9))
		(+ (* a8 b2) (* a9 b6) (* a10 b10)) (+ (* a8 b3) (* a9 b7) (* a10 b11) a11)))

(defun mat3x4-place (x y z rx ry rz sx sy px py)
	; (mat3x4-place x y z rx ry rz sx sy px py) -> matrix
	;where a flat thing is: scaled and turned about its pivot, px py, about
	;x then y then z in radians, and its pivot put at x y z
	(defq cx (cos rx) ax (sin rx) cy (cos ry) ay (sin ry) cz (cos rz) az (sin rz)
		;the turn, by z by y by x
		r0 (* cz cy) r1 (- (* cz ay ax) (* az cx)) r2 (+ (* cz ay cx) (* az ax))
		r4 (* az cy) r5 (+ (* az ay ax) (* cz cx)) r6 (- (* az ay cx) (* cz ax))
		r8 (neg ay) r9 (* cy ax) r10 (* cy cx)
		;its columns scaled, and the pivot taken off first
		m0 (* r0 sx) m1 (* r1 sy) m4 (* r4 sx) m5 (* r5 sy) m8 (* r8 sx) m9 (* r9 sy))
	(fixeds m0 m1 r2 (- x (+ (* m0 px) (* m1 py)))
		m4 m5 r6 (- y (+ (* m4 px) (* m5 py)))
		m8 m9 r10 (- z (+ (* m8 px) (* m9 py)))))

(defun mat3x2-invert (m)
	; (mat3x2-invert m) -> matrix
	;the matrix that undoes it. One that flattens everything to a line
	;has none, and that of no change is given
	(bind '(a b tx c d ty) m)
	(defq det (- (* a d) (* b c)))
	(if (= det 0.0) +mat3x2_unity
		(defq ia (/ d det) ib (/ (neg b) det) ic (/ (neg c) det) id (/ a det))
		(fixeds ia ib (neg (+ (* ia tx) (* ib ty))) ic id (neg (+ (* ic tx) (* id ty))))))

(defun mat3x2-point (m x y)
	; (mat3x2-point m x y) -> (x y)
	(setq x (n2q x) y (n2q y))
	(bind '(a b tx c d ty) m)
	(list (+ (* a x) (* b y) tx) (+ (* c x) (* d y) ty)))

(defun mat3x2-box (box m)
	; (mat3x2-box box m) -> (x y x1 y1)
	;the box round a box that a matrix has moved. Each side is the least
	;or the most of what each of its two parts can come to
	(bind '(x y x1 y1) box)
	(bind '(a b tx c d ty) m)
	(defq ax (* a x) ax1 (* a x1) by (* b y) by1 (* b y1)
		cx (* c x) cx1 (* c x1) dy (* d y) dy1 (* d y1))
	(list (+ (min ax ax1) (min by by1) tx) (+ (min cx cx1) (min dy dy1) ty)
		(+ (max ax ax1) (max by by1) tx) (+ (max cx cx1) (max dy dy1) ty)))
