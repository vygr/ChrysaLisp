;;;;;;;;;;;;;
; vector math
;;;;;;;;;;;;;

(import "class/num/lisp.inc")
(import "class/nums/lisp.inc")
(import "class/fixeds/lisp.inc")

(enums +vec2 0
	(enum x y))

(enums +vec3 0
	(enum x y z))

(enums +vec4 0
	(enum x y z w))

(defmacro Vec3-f (x y z)
	`(fixeds ,x ,y ,z))

(defmacro Vec4-f (x y z w)
	`(fixeds ,x ,y ,z ,w))

(defmacro Vec3-r (x y z)
	`(reals ,x ,y ,z))

(defmacro Vec4-r (x y z w)
	`(reals ,x ,y ,z ,w))

;useful vector constants
(defq +fixeds_zero3 (fixeds 0.0 0.0 0.0)
	+fixeds_zero4 (fixeds 0.0 0.0 0.0 0.0)
	+reals_zero3 (reals (n2r 0) (n2r 0) (n2r 0))
	+reals_zero4 (reals (n2r 0) (n2r 0) (n2r 0) (n2r 0))
	+fixeds_tmp3 (cat +fixeds_zero3) +fixeds_tmp4 (cat +fixeds_zero4)
	+reals_tmp3 (cat +reals_zero3) +reals_tmp4 (cat +reals_zero4))

;macro to define macros that take optional output vector
(defmacro vec-macro (op &rest v)
	`(defmacro ,(sym (cat "vec" (slice (find "-" op) -1 op))) (~v &optional _)
		(if _ `(,,(sym op) ~(list ~v) ,_) `(,,(sym op) ~(list ~v)))))

(vec-macro "nums-add" v0 v1)
(vec-macro "nums-sub" v0 v1)
(vec-macro "nums-min" v0 v1)
(vec-macro "nums-max" v0 v1)
(vec-macro "nums-mul" v0 v1)
(vec-macro "nums-div" v0 v1)
(vec-macro "nums-mod" v0 v1)
(vec-macro "nums-abs" v)
(vec-macro "nums-scale" v s)
(vec-macro "fixeds-frac" v)
(vec-macro "fixeds-floor" v)
;these don't take an optional output but...
(vec-macro "nums-dot" v0 v1)
(vec-macro "nums-sum" v)

(defmacro vec-clamp (p1 p2 p3 &optional _)
	(if _ `(nums-min (nums-max ,p1 ,p2 ,_) ,p3 ,_) `(nums-min (nums-max ,p1 ,p2) ,p3)))

(defun vec-reflect (p n)
	(vec-sub p (vec-scale n (* (nums-sum (vec-mul p n)) 2.0))))

(defun vec-length (p)
	(sqrt (vec-dot p p)))

(defun vec-length-squared (p)
	(vec-dot p p))

(defun vec-norm (p)
	(vec-scale p (recip (vec-length p))))

(defun vec-distance (p1 p2)
	(vec-length (vec-sub p2 p1)))

(defun vec-distance-squared (p1 p2)
	(vec-length-squared (vec-sub p2 p1)))

(defun vec-distance-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1) c1 (vec-dot (vec-sub p p1) lv))
	(if (<= c1 0)
		(vec-distance p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance p p2)
				(vec-distance p (vec-add p1 (vec-scale lv (/ c1 c2))))))))

(defun vec-distance-squared-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1) c1 (vec-dot (vec-sub p p1) lv))
	(if (<= c1 0)
		(vec-distance-squared p p1 lv)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (<= c2 c1)
				(vec-distance-squared p p2 lv)
				(vec-distance-squared p (vec-add p1 (vec-scale lv (/ c1 c2)) lv) lv)))))

(defun vec-manhattan-distance (p1 p2)
	(nums-sum (nums-abs (nums-sub p1 p2))))

(defun vec-euclidean-distance (p1 p2)
	(sqrt (nums-sum (nums-mul (defq _ (nums-sub p1 p2)) _))))

(defun vec-squared-euclidean-distance (p1 p2)
	(nums-sum (nums-mul (defq _ (nums-sub p1 p2)) _)))

(defun vec-chebyshev-distance (p1 p2)
	(reduce max (nums-abs (nums-sub p1 p2))))

;specific vector stuff

(defun vec-perp-2d ((x y))
	(list y (neg x)))

(defun vec-det-2d ((x1 y1) (x2 y2))
	(- (* x1 y2) (* y1 x2)))

(defun vec-cross-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- (* y1 z2) (* z1 y2))
		(- (* z1 x2) (* x1 z2))
		(- (* x1 y2) (* y1 x2))))

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
