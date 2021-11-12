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

(defmacro Vec2-f (x y)
	`(fixeds ,x ,y))

(defmacro Vec3-f (x y z)
	`(fixeds ,x ,y ,z))

(defmacro Vec4-f (x y z w)
	`(fixeds ,x ,y ,z ,w))

(defmacro Vec3-r (x y z)
	`(reals ,x ,y ,z))

(defmacro Vec4-r (x y z w)
	`(reals ,x ,y ,z ,w))

;useful vector constants
(defq +nums_zero2 (nums 0 0)
	+nums_zero3 (nums 0 0 0)
	+fixeds_zero2 (fixeds 0.0 0.0)
	+fixeds_zero3 (fixeds 0.0 0.0 0.0)
	+fixeds_zero4 (fixeds 0.0 0.0 0.0 0.0)
	+reals_zero3 (reals (n2r 0) (n2r 0) (n2r 0))
	+reals_zero4 (reals (n2r 0) (n2r 0) (n2r 0) (n2r 0))
	+nums_tmp3 (cat +nums_zero3) +nums_tmp2 (cat +nums_zero2)
	+fixeds_tmp3 (cat +fixeds_zero3) +fixeds_tmp4 (cat +fixeds_zero4)
	+reals_tmp3 (cat +reals_zero3) +reals_tmp4 (cat +reals_zero4))

;useful real constants
(defmacro def_real (c s e f)
	`(bind (map (# (sym (str ,c %0))) (range ,s ,e)) (map ,f (range ,s ,e))))
;[0:10]
(def_real "+real_" 0 11 (# (n2r %0)))
;[-1:-10]
(def_real "+real_" -10 0 (# (n2r %0)))
;[1/2:1/20]
(def_real "+real_1/" 2 21 (# (/ (const (n2r 1)) (n2r %0))))
;[-1/2:-1/20]
(def_real "+real_-1/" 2 21 (# (/ (const (n2r -1)) (n2r %0))))
;pi and temp vec3, vec4
(defq +real_pi (n2r +fp_pi) +real_hpi (n2r +fp_hpi) +real_2pi (n2r +fp_2pi))

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

(undef (env) 'def_real 'vec-macro)

(defmacro vec-clamp (p1 p2 p3 &optional _)
	(if _ `(nums-min (nums-max ,p1 ,p2 ,_) ,p3 ,_) `(nums-min (nums-max ,p1 ,p2) ,p3)))

(defun vec-reflect (p n)
	(vec-sub p (vec-scale n (* (nums-sum (vec-mul p n)) 2.0))))

(defun vec-length-squared (p)
	(vec-dot p p))

(defun vec-length (p)
	(sqrt (vec-dot p p)))

(defun vec-norm (p)
	(vec-scale p (recip (sqrt (vec-dot p p)))))

(defun vec-sdist (p1 p2)
	(vec-dot (defq p1 (vec-sub p2 p1)) p1))

(defun vec-dist (p1 p2)
	(sqrt (vec-dot (defq p1 (vec-sub p2 p1)) p1)))

(defun vec-dist-to-line (p p1 p2)
	(if (<= (defq lv (vec-sub p2 p1) c1 (vec-dot (vec-sub p p1) lv)) 0)
		(vec-dist p p1)
		(if (<= (defq c2 (vec-dot lv lv)) c1)
			(vec-dist p p2)
			(vec-dist p (vec-add p1 (vec-scale lv (/ c1 c2)))))))

(defun vec-sdist-to-line (p p1 p2)
	(if (<= (defq lv (vec-sub p2 p1) c1 (vec-dot (vec-sub p p1) lv)) 0)
		(vec-sdist p p1)
		(if (<= (defq c2 (vec-dot lv lv)) c1)
			(vec-sdist p p2)
			(vec-sdist p (vec-add p1 (vec-scale lv (/ c1 c2)) lv)))))

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

(defun vec-det ((x1 y1) (x2 y2))
	(- (* x1 y2) (* y1 x2)))

(defun vec-cross-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- (* y1 z2) (* z1 y2))
		(- (* z1 x2) (* x1 z2))
		(- (* x1 y2) (* y1 x2))))

(defun vec-intersect-2d (l1_p1 av l2_p1 bv)
	(defq axb (vec-det av bv)
		da (vec-det (vec-add l1_p1 av) l1_p1)
		db (vec-det (vec-add l2_p1 bv) l2_p1))
	(if (/= axb 0)
		(list
			(/ (vec-det
				(list da (elem-get 0 av))
				(list db (elem-get 0 bv))) axb)
			(/ (vec-det
				(list da (elem-get 1 av))
				(list db (elem-get 1 bv))) axb))))

(defun vec-intersect-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub l1_p2 l1_p1)
		bv (vec-sub l2_p2 l2_p1)
		axb (vec-det av bv)
		da (vec-det l1_p2 l1_p1)
		db (vec-det l2_p2 l2_p1))
	(if (/= axb 0)
		(list
			(/ (vec-det
				(list da (elem-get 0 av))
				(list db (elem-get 0 bv))) axb)
			(/ (vec-det
				(list da (elem-get 1 av))
				(list db (elem-get 1 bv))) axb))))

(defun vec-collide-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub l1_p2 l1_p1)
		bv (vec-sub l2_p2 l2_p1)
		cv (vec-sub l2_p2 l1_p1)
		axb (vec-det av bv)
		axc (vec-det av cv)
		cxb (vec-det cv bv))
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

(defun vec-collide-thick-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2 r)
	(cond
		((vec-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2))
		((<= (vec-sdist-to-line l2_p1 l1_p1 l1_p2) (setq r (* r r))))
		((<= (vec-sdist-to-line l2_p2 l1_p1 l1_p2) r))
		((<= (vec-sdist-to-line l1_p1 l2_p1 l2_p2) r))
		((<= (vec-sdist-to-line l1_p2 l2_p1 l2_p2) r))))

(defun bounding-box (verts fnc)
	; (bounding-box verts vec3-extract-fnc) -> (min_v3 max_v3)
	(defq min_v (cat (fnc (elem-get 0 verts))) max_v (cat min_v))
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
	(prin (join (map (# (pad (n2f %0) 8)) v) " "))
	(print))
