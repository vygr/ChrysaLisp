;generic distance metric stuff
(import 'gui/points/lisp.inc)
(import 'sys/math/lisp.inc)

(defun vec-manhattan-distance (p1 p2)
	(defq _ (points-sub p1 p2))
	(points-sum (points-abs _ _)))

(defun vec-euclidean-distance (p1 p2)
	(defq _ (points-sub p1 p2))
	(fsqrt (points-sum (points-fmul _ _ _))))

(defun vec-squared-euclidean-distance (p1 p2)
	(defq _ (points-sub p1 p2))
	(points-sum (points-fmul _ _ _)))

(defun vec-chebyshev-distance (p1 p2)
	(defq _ (points-sub p1 p2))
	(reduce max (points-abs _ _)))

;generic vector stuff

(defun vec-clamp (p i j)
	(apply points (map (lambda (_) (max i (min j _))) p)))

(defun vec-equal (p1 p2)
	(= (manhattan-distance p1 p2) 0))

(defmacro vec-add (p1 p2 &optional _)
	(if _
		(list 'points-add p1 p2 _)
		(list 'points-add p1 p2)))

(defmacro vec-sub (p1 p2 &optional _)
	(if _
		(list 'points-sub p1 p2 _)
		(list 'points-sub p1 p2)))

(defmacro vec-mul (p1 p2 &optional _)
	(if _
		(list 'points-fmul p1 p2 _)
		(list 'points-fmul p1 p2)))

(defmacro vec-scale (p s &optional _)
	(if _
		(list 'points-fscale p s _)
		(list 'points-fscale p s)))

(defun vec-mod (p s)
	(apply points (map (lambda (_) (fmod _ s)) p)))

(defun vec-floor (p)
	(apply points (map (lambda (_) (floor _)) p)))

(defmacro vec-frac (p &optional _)
	(if _
		(list 'points-frac p _)
		(list 'points-frac p)))

(defmacro vec-dot (p1 p2 &optional _)
	(if _
		(list 'points-sum (list 'vec-mul p1 p2 _))
		(list 'points-sum (list 'vec-mul p1 p2))))

(defun vec-reflect (p n &optional _)
	(if _
		(vec-mul p n _)
		(defq _ (vec-mul p n)))
	(points-fscale n (fmul (points-sum _) 2.0) _)
	(points-sub p _ _))

(defmacro vec-length (p &optional _)
	(if _
		(list 'fsqrt (list 'vec-dot p p _))
		(list 'fsqrt (list 'vec-dot p p))))

(defun-bind vec-norm (p &optional _)
	(if _
		(defq l (fsqrt (points-sum (points-fmul p p _))))
		(defq _ (points-fmul p p) l (fsqrt (points-sum _))))
	(if (= l 0)
		(points-fscale p 0 _)
		(points-fscale p (fdiv 1.0 l) _)))

(defun vec-distance (p1 p2)
	(defq _ (vec-sub p1 p2))
	(fsqrt (points-sum (points-fmul _ _ _))))

(defun vec-distance-squared (p1 p2)
	(defq _ (vec-sub p1 p2))
	(points-sum (points-fmul _ _ _)))

(defun vec-distance-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (<= c1 0)
		(vec-distance p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance p p2)
				(vec-distance p (vec-add p1 (vec-scale lv (fdiv c1 c2))))))))

(defun vec-distance-squared-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (<= c1 0)
		(vec-distance-squared p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance-squared p p2)
				(vec-distance-squared p (vec-add p1 (vec-scale lv (fdiv c1 c2))))))))

;specific vector stuff

(defun vec-add-2d ((x1 y1) (x2 y2))
	(list (+ x1 x2) (+ y1 y2)))

(defun vec-add-3d ((x1 y1 z1) (x2 y2 z2))
	(list (+ x1 x2) (+ y1 y2) (+ z1 z2)))

(defun vec-sub-2d ((x1 y1) (x2 y2))
	(list (- x1 x2) (- y1 y2)))

(defun vec-sub-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- x1 x2) (- y1 y2) (- z1 z2)))

(defun vec-mul-2d ((x1 y1) (x2 y2))
	(list (fmul x1 x2) (fmul y1 y2)))

(defun vec-mul-3d ((x1 y1 z1) (x2 y2 z2))
	(list (fmul x1 x2) (fmul y1 y2) (fmul z1 z2)))

(defun vec-mod-3d ((x y z) s)
	(list (fmod x s) (fmod y s) (fmod z s)))

(defun vec-floor-3d ((x y z))
	(list (floor x) (floor y) (floor z)))

(defun vec-scale-2d ((x y) s)
	(list (fmul x s) (fmul y s)))

(defun vec-scale-3d ((x y z) s)
	(list (fmul x s) (fmul y s) (fmul z s)))

(defun vec-perp-2d ((x y))
	(list y (neg x)))

(defun vec-det-2d ((x1 y1) (x2 y2))
	(- (fmul x1 y2) (fmul y1 x2)))

(defun vec-cross-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- (fmul y1 z2) (fmul z1 y2))
		(- (fmul z1 x2) (fmul x1 z2))
		(- (fmul x1 y2) (fmul y1 x2))))

(defun vec-dot-2d ((x1 y1) (x2 y2))
	(+ (fmul x1 x2) (fmul y1 y2)))

(defun vec-dot-3d ((x1 y1 z1) (x2 y2 z2))
	(+ (fmul x1 x2) (fmul y1 y2) (fmul z1 z2)))

(defun vec-reflect-3d (p n)
	(vec-sub-3d p (vec-scale-3d n (fmul (vec-dot-3d p n) 2.0))))

(defun vec-length-2d (p)
	(fsqrt (vec-dot-2d p p)))

(defun vec-length-3d (p)
	(fsqrt (vec-dot-3d p p)))

(defun vec-length-squared-2d (p)
	(vec-dot-2d p p))

(defun vec-length-squared-3d (p)
	(vec-dot-3d p p))

(defun vec-norm-2d (p)
	(defq l (vec-length-2d p))
	(if (= l 0)
		(list 0 0)
		(list (fmul (elem 0 p) (setq l (fdiv 1.0 l))) (fmul (elem 1 p) l))))

(defun vec-norm-3d (p)
	(defq l (vec-length-3d p))
	(if (= l 0)
		(list 0 0 0)
		(list (fmul (elem 0 p) (setq l (fdiv 1.0 l)))
			(fmul (elem 1 p) l) (fmul (elem 2 p) l))))

(defun vec-distance-2d (p1 p2)
	(vec-length-2d (vec-sub-2d p2 p1)))

(defun vec-distance-3d (p1 p2)
	(vec-length-3d (vec-sub-3d p2 p1)))

(defun vec-distance-squared-2d (p1 p2)
	(vec-length-squared-2d (vec-sub-2d p2 p1)))

(defun vec-distance-squared-3d (p1 p2)
	(vec-length-squared-3d (vec-sub-3d p2 p1)))

(defun vec-distance-to-line-2d (p p1 p2)
	(defq lv (vec-sub-2d p2 p1)
		pv (vec-sub-2d p p1)
		c1 (vec-dot-2d pv lv))
	(if (<= c1 0)
		(vec-distance-2d p p1)
		(progn
			(defq c2 (vec-dot-2d lv lv))
			(if (<= c2 c1)
				(vec-distance-2d p p2)
				(vec-distance-2d p (vec-add-2d p1 (vec-scale-2d lv (fdiv c1 c2))))))))

(defun vec-distance-to-line-3d (p p1 p2)
	(defq lv (vec-sub-3d p2 p1)
		pv (vec-sub-3d p p1)
		c1 (vec-dot-3d pv lv))
	(if (<= c1 0)
		(vec-distance-3d p p1)
		(progn
			(defq c2 (vec-dot-3d lv lv))
			(if (<= c2 c1)
				(vec-distance-3d p p2)
				(vec-distance-3d p (vec-add-3d p1 (vec-scale-3d lv (fdiv c1 c2))))))))

(defun vec-distance-squared-to-line-2d (p p1 p2)
	(defq lv (vec-sub-2d p2 p1)
		pv (vec-sub-2d p p1)
		c1 (vec-dot-2d pv lv))
	(if (<= c1 0)
		(vec-distance-squared-2d p p1)
		(progn
			(defq c2 (vec-dot-2d lv lv))
			(if (<= c2 c1)
				(vec-distance-squared-2d p p2)
				(vec-distance-squared-2d p (vec-add-2d p1 (vec-scale-2d lv (fdiv c1 c2))))))))

(defun vec-distance-squared-to-line-3d (p p1 p2)
	(defq lv (vec-sub-3d p2 p1)
		pv (vec-sub-3d p p1)
		c1 (vec-dot-3d pv lv))
	(if (<= c1 0)
		(vec-distance-squared-3d p p1)
		(progn
			(defq c2 (vec-dot-3d lv lv))
			(if (<= c2 c1)
				(vec-distance-squared-3d p p2)
				(vec-distance-squared-3d p (vec-add-3d p1 (vec-scale-3d lv (fdiv c1 c2))))))))

(defun vec-intersect-2d (l1_p1 av l2_p1 bv)
	(defq axb (vec-det-2d av bv)
		da (vec-det-2d (vec-add-2d l1_p1 av) l1_p1)
		db (vec-det-2d (vec-add-2d l2_p1 bv) l2_p1))
	(if (/= axb 0)
		(list
			(fdiv (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(fdiv (vec-det-2d
				(list da (elem 1 av))
				(list db (elem 1 bv))) axb))))

(defun vec-intersect-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub-2d l1_p2 l1_p1)
		bv (vec-sub-2d l2_p2 l2_p1)
		axb (vec-det-2d av bv)
		da (vec-det-2d l1_p2 l1_p1)
		db (vec-det-2d l2_p2 l2_p1))
	(if (/= axb 0)
		(list
			(fdiv (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(fdiv (vec-det-2d
				(list da (elem 1 av))
				(list db (elem 1 bv))) axb))))

(defun vec-collide-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub-2d l1_p2 l1_p1)
		bv (vec-sub-2d l2_p2 l2_p1)
		cv (vec-sub-2d l2_p2 l1_p1)
		axb (vec-det-2d av bv)
		axc (vec-det-2d av cv)
		cxb (vec-det-2d cv bv))
	(cond
		((= axb 0) nil)
		((> axb 0)
			(cond
				((or (< axc 0) (> axc axb)) nil)
				((or (< cxb 0) (> cxb axb)) nil)
				(t t)))
		(t
			(cond
				((or (> axc 0) (< axc axb)) nil)
				((or (> cxb 0) (< cxb axb)) nil)
				(t t)))))

(defun vec-collide-thick-lines-2d (l1_p1 l1_p2 l2_p1 tl2_p2 r)
	(cond
		((vec-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2))
		((<= (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) (setq r (fmul r r))))
		((<= (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r))
		((<= (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) r))
		((<= (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r))))
