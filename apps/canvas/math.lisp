;generic distance metric stuff

(defun vec-manhattan-distance (p1 p2)
	(reduce add (map (lambda (x y) (abs (sub x y))) p1 p2)))

(defun vec-euclidean-distance (p1 p2)
	(sqrt (reduce add (map (lambda (x y) (fp-mul (sub x y) (sub x y))) p1 p2))))

(defun vec-squared-euclidean-distance (p1 p2)
	(reduce add (map (lambda (x y) (fp-mul (sub x y) (sub x y))) p1 p2)))

(defun vec-chebyshev-distance (p1 p2)
	(reduce max (map (lambda (x y) (abs (sub x y))) p1 p2)))

(defun vec-reciprical-distance (p1 p2)
	(defq d (manhattan-distance p1 p2))
	(if (eq d 0) (1) (fp-div 1 d)))

;generic vector stuff

(defun vec-equal (p1 p2)
	(eq (manhattan-distance p1 p2) 0))

(defun vec-add (p1 p2)
	(map add p1 p2))

(defun vec-sub (p1 p2)
	(map sub p1 p2))

(defun vec-scale (p s)
	(map (lambda (_) (fp-mul _ s)) p))

(defun vec-dot (p1 p2)
	(reduce add (map (lambda (x y) (fp-mul x y)) p1 p2)))

(defun vec-length (p)
	(fp-sqrt (vec-dot p p)))

(defun vec-distance (p1 p2)
	(vec-length (vec-sub p1 p2)))

(defun vec-distance-squared (p1 p2)
	(defq p (vec-sub p1 p2))
	(vec-dot p p))

(defun vec-norm (p)
	(defq l (vec-length p))
	(if (eq l 0)
		(vec-scale p 0)
		(vec-scale p (fp-div 1 l))))

(defun vec-distance-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (le c1 0)
		(vec-distance p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (le c2 c1)
				(vec-distance p p2)
				(vec-distance p (vec-add p1 (vec-scale lv (fp-div c1 c2))))))))

(defun vec-distance-squared-to-line (p p1 p2)
	(defq lv (vec-sub p2 p1)
		pv (vec-sub p p1)
		c1 (vec-dot pv lv))
	(if (le c1 0)
		(vec-distance-squared p p1)
		(progn
			(defq c2 (vec-dot lv lv))
			(if (le c2 c1)
				(vec-distance-squared p p2)
				(vec-distance-squared p (vec-add p1 (vec-scale lv (fp-div c1 c2))))))))

;specific vector stuff

(defun vec-add-2d (p1 p2)
	(list (add (elem 0 p1) (elem 0 p2))
		(add (elem 1 p1) (elem 1 p2))))

(defun vec-add-3d (p1 p2)
	(list (add (elem 0 p1) (elem 0 p2))
		(add (elem 1 p1) (elem 1 p2))
		(add (elem 2 p1) (elem 2 p2))))

(defun vec-sub-2d (p1 p2)
	(list (sub (elem 0 p1) (elem 0 p2))
		(sub (elem 1 p1) (elem 1 p2))))

(defun vec-sub-3d (p1 p2)
	(list (sub (elem 0 p1) (elem 0 p2))
		(sub (elem 1 p1) (elem 1 p2))
		(sub (elem 2 p1) (elem 2 p2))))

(defun vec-scale-2d (p s)
	(list (fp-mul (elem 0 p) s)
		(fp-mul (elem 1 p) s)))

(defun vec-scale-3d (p s)
	(list (fp-mul (elem 0 p) s)
		(fp-mul (elem 1 p) s)
		(fp-mul (elem 2 p) s)))

(defun vec-perp-2d (p)
	(list (elem 1 p) (neg (elem 0 p))))

(defun vec-cross-3d (p1 p2)
	(defq x1 (elem 0 p1) y1 (elem 1 p1) z1 (elem 2 p1)
		x2 (elem 0 p2) y2 (elem 1 p2) z2 (elem 2 p2))
	(list (sub (fp-mul y1 z2) (fp-mul z1 y2))
		(sub (fp-mul z1 x2) (fp-mul x1 z2))
		(sub (fp-mul x1 y2) (fp-mul y1 x2))))

(defun vec-dot-2d (p1 p2)
	(add (fp-mul (elem 0 p1) (elem 0 p2))
		(fp-mul (elem 1 p1) (elem 1 p2))))

(defun vec-dot-3d (p1 p2)
	(add (fp-mul (elem 0 p1) (elem 0 p2))
		(fp-mul (elem 1 p1) (elem 1 p2))
		(fp-mul (elem 2 p1) (elem 2 p2))))

(defun vec-length-2d (p)
	(fp-sqrt (vec-dot-2d p p)))

(defun vec-length-3d (p)
	(fp-sqrt (vec-dot-3d p p)))

(defun vec-length-squared-2d (p)
	(vec-dot-2d p p))

(defun vec-length-squared-3d (p)
	(vec-dot-3d p p))

(defun vec-norm-2d (p)
	(defq l (vec-length-2d p))
	(if (eq l 0)
		(list 0 0)
		(list (fp-div (elem 0 p) l) (fp-div (elem 1 p) l))))

(defun vec-norm-3d (p)
	(defq l (vec-length-3d p))
	(if (eq l 0)
		(list 0 0 0)
		(list (fp-div (elem 0 p) l) (fp-div (elem 1 p) l) (fp-div (elem 2 p) l))))

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
	(if (le c1 0)
		(vec-distance-2d p p1)
		(progn
			(defq c2 (vec-dot-2d lv lv))
			(if (le c2 c1)
				(vec-distance-2d p p2)
				(vec-distance-2d p (vec-add-2d p1 (vec-scale-2d lv (fp-div c1 c2))))))))

(defun vec-distance-to-line-3d (p p1 p2)
	(defq lv (vec-sub-3d p2 p1)
		pv (vec-sub-3d p p1)
		c1 (vec-dot-3d pv lv))
	(if (le c1 0)
		(vec-distance-3d p p1)
		(progn
			(defq c2 (vec-dot-3d lv lv))
			(if (le c2 c1)
				(vec-distance-3d p p2)
				(vec-distance-3d p (vec-add-3d p1 (vec-scale-3d lv (fp-div c1 c2))))))))

(defun vec-distance-squared-to-line-2d (p p1 p2)
	(defq lv (vec-sub-2d p2 p1)
		pv (vec-sub-2d p p1)
		c1 (vec-dot-2d pv lv))
	(if (le c1 0)
		(vec-distance-squared-2d p p1)
		(progn
			(defq c2 (vec-dot-2d lv lv))
			(if (le c2 c1)
				(vec-distance-squared-2d p p2)
				(vec-distance-squared-2d p (vec-add-2d p1 (vec-scale-2d lv (fp-div c1 c2))))))))

(defun vec-distance-squared-to-line-3d (p p1 p2)
	(defq lv (vec-sub-3d p2 p1)
		pv (vec-sub-3d p p1)
		c1 (vec-dot-3d pv lv))
	(if (le c1 0)
		(vec-distance-squared-3d p p1)
		(progn
			(defq c2 (vec-dot-3d lv lv))
			(if (le c2 c1)
				(vec-distance-squared-3d p p2)
				(vec-distance-squared-3d p (vec-add-3d p1 (vec-scale-3d lv (fp-div c1 c2))))))))

;generic path stuff

(defun thicken-path-2d (points radius capstyle joinstyle)
	(if (eq radius 0)
		(cat points (slice -2 -1 points))
		(progn
			(defq index 0 step 1 out-points (list) sides 2)
			(while (ge (setq sides (dec sides)) 0)
				(defq p1 (elem index points)
					index (add index step)
					p2 (elem index points)
					index (add index step)
					l2-v (vec-sub-2d p2 p1)
					l2-pv (vec-perp-2d l2-v)
					l2-npv (vec-norm-2d l2-pv)
					rv (vec-scale-2d l2-npv radius))
				(cond
					((eq capstyle 0)
						;butt cap
						(push out-points
							(vec-sub-2d p1 rv)
							(vec-add-2d p1 rv)))
					((eq capstyle 1)
						;square cap
						(defq p0 (vec-add-2d p1 (vec-perp-2d rv)))
						(push out-points
							(vec-sub-2d p0 rv)
							(vec-add-2d p0 rv)))
					((eq capstyle 2)
						;triangle cap
						(push out-points
							(vec-sub-2d p1 rv)
							(vec-add-2d p1 (vec-perp-2d rv))
							(vec-add-2d p1 rv)))
					((eq capstyle 3)
						;round cap
						(defq rvx (elem 0 rv) rvy (elem 1 rv) a 0)
						(while (le a fp-pi)
							(defq s (fp-sin a) c (fp-cos a))
							(push out-points
								(vec-sub-2d p1 (list (sub (fp-mul rvx c) (fp-mul rvy s))
													(add (fp-mul rvx s) (fp-mul rvy c)))))
							(setq a (add a (div fp-pi 32)))))
					(t (throw "Missing capsytle " capstyle)))
				(while (and (ne index -1) (ne index (length points)))
					(defq p1 p2 l1-v l2-v l1-npv l2-npv
						p2 (elem index points)
						index (add index step)
						l2-v (vec-sub-2d p2 p1)
						l2-pv (vec-perp-2d l2-v)
						l2-npv (vec-norm-2d l2-pv)
						nbv (vec-norm-2d (vec-scale-2d (vec-add-2d l1-npv l2-npv) fp-half))
						c (vec-dot-2d nbv (vec-norm-2d l1-v)))
					(cond
						((or (le c 0) (eq joinstyle 0))
							;mitre join
							(defq s (fp-sin (acos c))
								bv (vec-scale-2d nbv (fp-div radius s)))
							(push out-points
								(vec-add-2d p1 bv)))
						((eq joinstyle 1)
							;bevel join
							(push out-points
								(vec-add-2d p1 (vec-scale-2d l1-npv radius))
								(vec-add-2d p1 (vec-scale-2d l2-npv radius))))
						(t (throw "Missing joinstyle " joinstyle))))
				(setq step (neg step) index (add index step)))
				out-points)))

