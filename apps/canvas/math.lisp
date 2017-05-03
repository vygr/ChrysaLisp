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

(defun vec-det-2d (p1 p2)
	(sub (fp-mul (elem 0 p1) (elem 1 p2)) (fp-mul (elem 1 p1) (elem 0 p2))))

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

(defun vec-collide-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub-2d l1_p2 l1_p1)
		bv (vec-sub-2d l2_p2 l2_p1)
		cv (vec-sub-2d l2_p2 l1_p1)
		axb (vec-det-2d av bv)
		axc (vec-det-2d av cv)
		cxb (vec-det-2d cv bv))
	(cond
		((eq axb 0))
		((gt axb 0)
			(cond
				((or (lt axc 0) (gt axc axb)))
				((or (lt cxb 0) (gt cxb axb)))
				(t t)))
		(t
			(cond
				((or (gt axc 0) (lt axc axb)))
				((or (gt cxb 0) (lt cxb axb)))
				(t t)))))

(defun vec-intersect-2d (l1_p1 av l2_p1 bv)
	(defq axb (vec-det-2d av bv)
		da (vec-det-2d (vec-add-2d l1_p1 av) l1_p1)
		db (vec-det-2d (vec-add-2d l2_p1 bv) l2_p1))
	(if (ne axb 0)
		(list
			(fp-div (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(fp-div (vec-det-2d
				(list da (elem 1 av))
				(list db (elem 1 bv))) axb))))

(defun vec-intersect-lines-2d (l1_p1 l1_p2 l2_p1 l2_p2)
	(defq av (vec-sub-2d l1_p2 l1_p1)
		bv (vec-sub-2d l2_p2 l2_p1)
		axb (vec-det-2d av bv)
		da (vec-det-2d l1_p2 l1_p1)
		db (vec-det-2d l2_p2 l2_p1))
	(if (ne axb 0)
		(list
			(fp-div (vec-det-2d
				(list da (elem 0 av))
				(list db (elem 0 bv))) axb)
			(fp-div (vec-det-2d
				(list da (elem 1 av))
				(list db (elem 1 bv))) axb))))

(defun vec-collide-thick-lines-2d (l1_p1 l1_p2 l2_p1 tl2_p2 r)
	(cond
		((vec-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2) t)
		((le (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) (setq r (fp-mul r r))) t)
		((le (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) (setq r (fp-mul r r))) t)
		((le (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) (setq r (fp-mul r r))) t)
		((le (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) (setq r (fp-mul r r))) t)))

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
					l2_v (vec-sub-2d p2 p1)
					l2_pv (vec-perp-2d l2_v)
					l2_npv (vec-norm-2d l2_pv)
					l2_rv (vec-scale-2d l2_npv radius))
				(cond
					((eq capstyle 0)
						;butt cap
						(push out-points
							(vec-sub-2d p1 l2_rv)
							(vec-add-2d p1 l2_rv)))
					((eq capstyle 1)
						;square cap
						(defq p0 (vec-add-2d p1 (vec-perp-2d l2_rv)))
						(push out-points
							(vec-sub-2d p0 l2_rv)
							(vec-add-2d p0 l2_rv)))
					((eq capstyle 2)
						;triangle cap
						(push out-points
							(vec-sub-2d p1 l2_rv)
							(vec-add-2d p1 (vec-perp-2d l2_rv))
							(vec-add-2d p1 l2_rv)))
					((eq capstyle 3)
						;round cap
						(defq rvx (elem 0 l2_rv) rvy (elem 1 l2_rv) a 0)
						(while (le a fp-pi)
							(defq s (fp-sin a) c (fp-cos a))
							(push out-points
								(vec-sub-2d p1 (list (sub (fp-mul rvx c) (fp-mul rvy s))
													(add (fp-mul rvx s) (fp-mul rvy c)))))
							(setq a (add a (div fp-pi 32)))))
					(t (throw "Missing capsytle " capstyle)))
				(while (and (ne index -1) (ne index (length points)))
					(defq p1 p2 l1_v l2_v l1_npv l2_npv l1_rv l2_rv
						p2 (elem index points)
						index (add index step)
						l2_v (vec-sub-2d p2 p1)
						l2_pv (vec-perp-2d l2_v)
						l2_npv (vec-norm-2d l2_pv)
						l2_rv (vec-scale-2d l2_npv radius)
						nbv (vec-norm-2d (vec-scale-2d (vec-add-2d l1_npv l2_npv) fp-half))
						c (vec-dot-2d nbv (vec-norm-2d l1_v)))
					(cond
						((or (le c 0) (eq joinstyle 0))
							;mitre join
							(push out-points (vec-intersect-2d
								(vec-add-2d p1 l1_rv) l1_v
								(vec-add-2d p1 l2_rv) l2_v)))
						((eq joinstyle 1)
							;bevel join
							(push out-points
								(vec-add-2d p1 l1_rv)
								(vec-add-2d p1 l2_rv)))
						(t (throw "Missing joinstyle " joinstyle))))
				(setq step (neg step) index (add index step)))
				out-points)))
