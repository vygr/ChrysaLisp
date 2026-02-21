;;;;;;;;;;;;;
; vector math
;;;;;;;;;;;;;

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
	+reals_one3 (reals (n2r 1) (n2r 1) (n2r 1))
	+reals_one4 (reals (n2r 1) (n2r 1) (n2r 1) (n2r 1))
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
(defmacro vector-macro (op &rest v)
	`(defmacro ,(sym (cat "vector" (slice op (find "-" op) -1))) (~v &optional _)
		(if _ `(,,(sym op) ~(list ~v) ,_) `(,,(sym op) ~(list ~v)))))

(vector-macro "nums-add" v0 v1)
(vector-macro "nums-sub" v0 v1)
(vector-macro "nums-min" v0 v1)
(vector-macro "nums-max" v0 v1)
(vector-macro "nums-mul" v0 v1)
(vector-macro "nums-div" v0 v1)
(vector-macro "nums-mod" v0 v1)
(vector-macro "nums-abs" v)
(vector-macro "nums-scale" v scale)
(vector-macro "fixeds-frac" v)
(vector-macro "fixeds-floor" v)
(vector-macro "fixeds-ceil" v)
(vector-macro "reals-quant" v tol)
;these don't take an optional output but...
(vector-macro "nums-dot" v0 v1)
(vector-macro "nums-sum" v)

(undef (env) 'def_real 'vector-macro)

(defmacro vector-clamp (p1 p2 p3 &optional _)
	(if _ `(nums-min (nums-max ,p1 ,p2 ,_) ,p3 ,_) `(nums-min (nums-max ,p1 ,p2) ,p3)))

(defun vector-reflect (p n)
	(vector-sub p (vector-scale n (* (nums-sum (vector-mul p n)) +real_2))))

(defun vector-length-squared (p)
	(vector-dot p p))

(defun vector-length (p)
	(sqrt (vector-dot p p)))

(defun vector-norm (p)
	(vector-scale p (recip (sqrt (vector-dot p p)))))

(defun vector-sdist (p1 p2)
	(vector-dot (defq p1 (vector-sub p2 p1)) p1))

(defun vector-dist (p1 p2)
	(sqrt (vector-dot (defq p1 (vector-sub p2 p1)) p1)))

(defun vector-dist-to-line (p p1 p2)
	(if (<= (defq lv (vector-sub p2 p1) c1 (vector-dot (vector-sub p p1) lv)) 0)
		(vector-dist p p1)
		(if (<= (defq c2 (vector-dot lv lv)) c1)
			(vector-dist p p2)
			(vector-dist p (vector-add p1 (vector-scale lv (/ c1 c2)))))))

(defun vector-sdist-to-line (p p1 p2)
	(if (<= (defq lv (vector-sub p2 p1) c1 (vector-dot (vector-sub p p1) lv)) 0)
		(vector-sdist p p1)
		(if (<= (defq c2 (vector-dot lv lv)) c1)
			(vector-sdist p p2)
			(vector-sdist p (vector-add p1 (vector-scale lv (/ c1 c2)) lv)))))

(defun vector-manhattan-distance (p1 p2)
	(nums-sum (nums-abs (nums-sub p1 p2))))

(defun vector-euclidean-distance (p1 p2)
	(sqrt (nums-dot (defq _ (nums-sub p1 p2)) _)))

(defun vector-squared-euclidean-distance (p1 p2)
	(nums-dot (defq _ (nums-sub p1 p2)) _))

(defun vector-chebyshev-distance (p1 p2)
	(reduce max (nums-abs (nums-sub p1 p2))))

;specific vector stuff

(defun vector-perp-2d ((x y))
	(list y (neg x)))

(defun vector-det ((x1 y1) (x2 y2))
	(- (* x1 y2) (* y1 x2)))

(defun vector-cross-3d ((x1 y1 z1) (x2 y2 z2))
	(list (- (* y1 z2) (* z1 y2))
		(- (* z1 x2) (* x1 z2))
		(- (* x1 y2) (* y1 x2))))

(defun vector-intersect-2d (l1_p1 av l2_p1 bv)
	(defq axb (vector-det av bv)
		da (vector-det (vector-add l1_p1 av) l1_p1)
		db (vector-det (vector-add l2_p1 bv) l2_p1))
	(if (/= axb 0)
		(list
			(/ (vector-det
				(list da (first av))
				(list db (first bv))) axb)
			(/ (vector-det
				(list da (second av))
				(list db (second bv))) axb))))

(defun vector-intersect-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vector-sub l1_p2 l1_p1)
		bv (vector-sub l2_p2 l2_p1)
		axb (vector-det av bv)
		da (vector-det l1_p2 l1_p1)
		db (vector-det l2_p2 l2_p1))
	(if (/= axb 0)
		(list
			(/ (vector-det
				(list da (first av))
				(list db (first bv))) axb)
			(/ (vector-det
				(list da (second av))
				(list db (second bv))) axb))))

(defun vector-collide-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vector-sub l1_p2 l1_p1)
		bv (vector-sub l2_p2 l2_p1)
		cv (vector-sub l2_p2 l1_p1)
		axb (vector-det av bv)
		axc (vector-det av cv)
		cxb (vector-det cv bv))
	(cond
		((= axb 0) :nil)
		((> axb 0)
			(cond
				((or (< axc 0) (> axc axb)) :nil)
				((or (< cxb 0) (> cxb axb)) :nil)
				(:t :t)))
		(:t
			(cond
				((or (> axc 0) (< axc axb)) :nil)
				((or (> cxb 0) (< cxb axb)) :nil)
				(:t :t)))))

(defun vector-collide-thick-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2 r)
	(cond
		((vector-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2))
		((<= (vector-sdist-to-line l2_p1 l1_p1 l1_p2) (setq r (* r r))))
		((<= (vector-sdist-to-line l2_p2 l1_p1 l1_p2) r))
		((<= (vector-sdist-to-line l1_p1 l2_p1 l2_p2) r))
		((<= (vector-sdist-to-line l1_p2 l2_p1 l2_p2) r))))

(defun vector-bounds-2d (paths)
	; (vector-bounds-2d paths) -> (min_v2 max_v2)
	(defq min_v (slice (first paths) 0 2) max_v (cat min_v))
	(each (# (each (# (setq
		min_v (vector-min %0 min_v)
		max_v (vector-max %0 max_v))) (partition %0 2))) paths)
	(list min_v max_v))

(defun vector-bounds-3d (verts &optional stride)
	; (vector-bounds-3d verts [stride]) -> (min_v3 max_v3)
	(setd stride 3)
	(defq min_v (slice verts 0 3) max_v (cat min_v))
	(each (lambda (v)
		(defq v3 (if (> stride 3) (slice v 0 3) v))
		(vector-min v3 min_v min_v)
		(vector-max v3 max_v max_v)) (partition verts stride))
	(list min_v max_v))

(defun vector-bounds-sphere (verts &optional stride)
	; (vector-bounds-sphere verts [stride]) -> (center_v3 radius)
	(setd stride 3)
	(bind '(min_v max_v) (vector-bounds-3d verts stride))
	(defq center (vector-scale (vector-add min_v max_v) +real_1/2)
		radius (* (reduce max (vector-sub max_v min_v)) +real_1/2))
	(each (lambda (v)
		(defq v3 (if (> stride 3) (slice v 0 3) v)
			  pv (vector-sub v3 center +reals_tmp3)
			  rv (vector-length pv))
		(when (> rv radius)
			(setq radius (* (+ radius rv) +real_1/2))
			(vector-sub v3 (vector-scale (vector-norm pv) radius pv) center)))
		(partition verts stride))
	(list center radius))

(defun vector-point-in-polygon (p paths winding_mode)
	; (vector-point-in-polygon p paths winding_mode) -> :t | :nil
	(defq winding 0)
	(bind '(px py) p)
	(if (some! (lambda (pth)
			(defq pth (partition pth 2) p1 (last pth))
			(some! (lambda (p2)
				(bind '(x1 y1) p1)
				(bind '(x2 y2) p2)
				(defq d (- (* (- x2 x1) (- py y1)) (* (- y2 y1) (- px x1))))
				(cond
					((and (= d 0)
						  (<= (min x1 x2) px (max x1 x2))
						  (<= (min y1 y2) py (max y1 y2)))
						:t)
					(:t
						(cond
							((<= y1 py)
								(if (and (> y2 py) (> d 0)) (++ winding)))
							((and (<= y2 py) (< d 0))
								(-- winding)))
						(setq p1 p2)
						:nil)))
				(list pth)))
			(list paths))
		:t
		(if (= winding_mode +winding_odd_even)
			(/= (logand winding 1) 0)
			(/= winding 0))))