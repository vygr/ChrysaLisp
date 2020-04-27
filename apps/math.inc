;generic distance metric stuff
(import 'class/num/lisp.inc)
(import 'class/nums/lisp.inc)
(import 'class/fixeds/lisp.inc)

(defun-bind vec-manhattan-distance (p1 p2)
	(defq _ (nums-sub p1 p2))
	(nums-sum (nums-abs _ _)))

(defun-bind vec-euclidean-distance (p1 p2)
	(defq _ (nums-sub p1 p2))
	(sqrt (nums-sum (nums-mul _ _ _))))

(defun-bind vec-squared-euclidean-distance (p1 p2)
	(defq _ (nums-sub p1 p2))
	(nums-sum (nums-mul _ _ _)))

(defun-bind vec-chebyshev-distance (p1 p2)
	(defq _ (nums-sub p1 p2))
	(reduce max (nums-abs _ _)))

;generic vector stuff

(defun-bind vec-clamp (p i j)
	(apply (const nums) (map (lambda (_) (max i (min j _))) p)))

(defun-bind vec-equal (p1 p2)
	(= (manhattan-distance p1 p2) 0))

(defmacro-bind vec-add (p1 p2 &optional _)
	(if _
		`(nums-add ,p1 ,p2 ,_)
		`(nums-add ,p1 ,p2)))

(defmacro-bind vec-sub (p1 p2 &optional _)
	(if _
		`(nums-sub ,p1 ,p2 ,_)
		`(nums-sub ,p1 ,p2)))

(defmacro-bind vec-mul (p1 p2 &optional _)
	(if _
		`(nums-mul ,p1 ,p2 ,_)
		`(nums-mul ,p1 ,p2)))

(defmacro-bind vec-div (p1 p2 &optional _)
	(if _
		`(nums-div ,p1 ,p2 ,_)
		`(nums-div ,p1 ,p2)))

(defmacro-bind vec-mod (p1 p2 &optional _)
	(if _
		`(nums-mod ,p1 ,p2 ,_)
		`(nums-mod ,p1 ,p2)))

(defmacro-bind vec-frac (p &optional _)
	(if _
		`(nums-frac ,p ,_)
		`(nums-frac ,p)))

(defmacro-bind vec-floor (p &optional _)
	(if _
		`(nums-floor ,p ,_)
		`(nums-floor ,p)))

(defmacro-bind vec-abs (p &optional _)
	(if _
		`(nums-abs ,p ,_)
		`(nums-abs ,p)))

(defmacro-bind vec-scale (p s &optional _)
	(if _
		`(nums-scale ,p ,s ,_)
		`(nums-scale ,p ,s)))

(defmacro-bind vec-dot (p1 p2 &optional _)
	(if _
		`(nums-sum (vec-mul ,p1 ,p2 ,_))
		`(nums-sum (vec-mul ,p1 ,p2))))

(defun-bind vec-reflect (p n &optional _)
	(if _
		(vec-mul p n _)
		(defq _ (vec-mul p n)))
	(nums-scale n (* (nums-sum _) 2.0) _)
	(nums-sub p _ _))

(defmacro-bind vec-length (p &optional _)
	(if _
		`(sqrt (vec-dot ,p ,p ,_))
		`(sqrt (vec-dot ,p ,p))))

(defmacro-bind vec-length-squared (p &optional _)
	(if _
		`(vec-dot ,p ,p ,_)
		`(vec-dot ,p ,p)))

(defun-bind vec-norm (p &optional _)
	(if _
		(defq l (sqrt (nums-sum (nums-mul p p _))))
		(defq _ (nums-mul p p) l (sqrt (nums-sum _))))
	(if (= l 0)
		(nums-scale p 0 _)
		(nums-scale p (/ 1.0 l) _)))

(defun-bind vec-distance (p1 p2)
	(defq _ (vec-sub p1 p2))
	(sqrt (nums-sum (nums-mul _ _ _))))

(defun-bind vec-distance-squared (p1 p2)
	(defq _ (vec-sub p1 p2))
	(nums-sum (nums-mul _ _ _)))

(defun-bind vec-distance-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (<= c1 0)
		(vec-distance p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance p p2)
				(vec-distance p (vec-add p1 (vec-scale lv (/ c1 c2))))))))

(defun-bind vec-distance-squared-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (<= c1 0)
		(vec-distance-squared p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance-squared p p2)
				(vec-distance-squared p (vec-add p1 (vec-scale lv (/ c1 c2))))))))

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
	(list (* x1 x2) (* y1 y2)))

(defun vec-mul-3d ((x1 y1 z1) (x2 y2 z2))
	(list (* x1 x2) (* y1 y2) (* z1 z2)))

(defun vec-mod-3d ((x y z) s)
	(list (% x s) (% y s) (% z s)))

(defun vec-floor-3d ((x y z))
	(list (floor x) (floor y) (floor z)))

(defun vec-scale-2d ((x y) s)
	(list (* x s) (* y s)))

(defun vec-scale-3d ((x y z) s)
	(list (* x s) (* y s) (* z s)))

(defun vec-perp-2d ((x y))
	(list y (neg x)))

(defun vec-det-2d ((x1 y1) (x2 y2))
	(- (* x1 y2) (* y1 x2)))

(defun vec-cross-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- (* y1 z2) (* z1 y2))
		(- (* z1 x2) (* x1 z2))
		(- (* x1 y2) (* y1 x2))))

(defun vec-dot-2d ((x1 y1) (x2 y2))
	(+ (* x1 x2) (* y1 y2)))

(defun vec-dot-3d ((x1 y1 z1) (x2 y2 z2))
	(+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defun vec-reflect-3d (p n)
	(vec-sub-3d p (vec-scale-3d n (* (vec-dot-3d p n) (<< 2 fp_shift)))))

(defun vec-length-2d (p)
	(sqrt (vec-dot-2d p p)))

(defun vec-length-3d (p)
	(sqrt (vec-dot-3d p p)))

(defun vec-length-squared-2d (p)
	(vec-dot-2d p p))

(defun vec-length-squared-3d (p)
	(vec-dot-3d p p))

(defun vec-norm-2d (p)
	(defq l (vec-length-2d p))
	(if (= l 0)
		(list 0 0)
		(list (* (elem 0 p) (setq l (/ (<< 1 fp_shift) l))) (* (elem 1 p) l))))

(defun vec-norm-3d (p)
	(defq l (vec-length-3d p))
	(if (= l 0)
		(list 0 0 0)
		(list (* (elem 0 p) (setq l (/ (<< 1 fp_shift) l)))
			(* (elem 1 p) l) (* (elem 2 p) l))))

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
				(vec-distance-2d p (vec-add-2d p1 (vec-scale-2d lv (/ c1 c2))))))))

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
				(vec-distance-3d p (vec-add-3d p1 (vec-scale-3d lv (/ c1 c2))))))))

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
				(vec-distance-squared-2d p (vec-add-2d p1 (vec-scale-2d lv (/ c1 c2))))))))

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
				(vec-distance-squared-3d p (vec-add-3d p1 (vec-scale-3d lv (/ c1 c2))))))))

(defun vec-intersect-2d (l1_p1 av l2_p1 bv)
	(defq axb (vec-det-2d av bv)
		da (vec-det-2d (vec-add-2d l1_p1 av) l1_p1)
		db (vec-det-2d (vec-add-2d l2_p1 bv) l2_p1))
	(if (/= axb 0)
		(list
			(/ (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(/ (vec-det-2d
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
			(/ (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(/ (vec-det-2d
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
		((<= (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) (setq r (* r r))))
		((<= (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r))
		((<= (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) r))
		((<= (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r))))
