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

(defun bounding-box (verts fnc)
	; (bounding-box verts vec3-extract-fnc) -> (min_v3 max_v3)
	(defq min_v (cat (fnc (first verts))) max_v (cat min_v))
	(each! (#
		(vector-min (defq v (fnc %0)) min_v min_v)
		(vector-max v max_v max_v)) (list verts) 1)
	(list min_v max_v))

(defun bounding-sphere (verts fnc)
	; (bounding-sphere verts vec3-extract-fnc) -> (center_v3 radius)
	(bind '(min_v max_v) (bounding-box verts fnc))
	(defq center (vector-scale (vector-add min_v max_v) +real_1/2)
		radius (* (reduce max (vector-sub max_v min_v)) +real_1/2))
	(each (#
		(defq p (fnc %0) pv (vector-sub p center) rv (vector-length pv))
		(when (> rv radius)
			(setq radius (* (+ radius rv) +real_1/2))
			(vector-sub p (vector-scale (vector-norm pv) radius +reals_tmp3) center))) verts)
	(list center radius))

(defun vector-path-bbox (paths)
	; (vector-path-bbox paths) -> (min_x min_y max_x max_y)
	(defq minx (const (n2f 1000000.0)) miny minx
		maxx (const (n2f -1000000.0)) maxy maxx)
	(each (lambda (p)
		(defq i 0 l (length p))
		(while (< i l)
			(defq x (elem-get p i) y (elem-get p (inc i)))
			(setq minx (min minx x) maxx (max maxx x)
				  miny (min miny y) maxy (max maxy y))
			(setq i (+ i 2)))) paths)
	(if (> minx maxx)
		(list 0.0 0.0 0.0 0.0)
		(list minx miny maxx maxy)))

(defun vector-point-in-polygon (px py paths)
	; (vector-point-in-polygon px py paths) -> :t | :nil
	(defq inside :nil)
	(each (lambda (p)
		(defq i 0 l (length p) j (- l 2))
		(while (< i l)
			(defq xi (elem-get p i) yi (elem-get p (inc i))
				xj (elem-get p j) yj (elem-get p (inc j)))
			(when (and (/= (<= yi py) (<= yj py))
					(< px (+ xi (* (/ (- xj xi) (- yj yi)) (- py yi)))))
				(setq inside (not inside)))
			(setq j i i (+ i 2)))) paths)
	inside)
