;generic distance metric stuff

(defun vec-manhattan-distance (p1 p2)
	(reduce add (map (lambda (x y) (abs (sub x y))) p1 p2)))

(defun vec-euclidean-distance (p1 p2)
	(sqrt (reduce add (map (lambda (x y) (fp-mul (sub x y) (sub x y))) p1 p2))))

(defun vec-squared-euclidean-distance (p1 p2)
	(reduce add (map (lambda (x y) (fp-mul (sub x y) (sub x y))) p1 p2)))

(defun vec-chebyshev-distance (p1 p2)
	(reduce max (map (lambda (x y) (abs (sub x y))) p1 p2)))

(defun vec-reciprocal-distance (p1 p2)
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
		(vec-scale p (fp-div fp-one l))))

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
		(list (fp-mul (elem 0 p) (setq l (fp-div fp-one l))) (fp-mul (elem 1 p) l))))

(defun vec-norm-3d (p)
	(defq l (vec-length-3d p))
	(if (eq l 0)
		(list 0 0 0)
		(list (fp-mul (elem 0 p) (setq l (fp-div fp-one l)))
			(fp-mul (elem 1 p) l) (fp-mul (elem 2 p) l))))

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

(defun vec-collide-thick-lines-2d (l1_p1 l1_p2 l2_p1 tl2_p2 r)
	(cond
		((vec-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2) t)
		((le (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) (setq r (fp-mul r r))) t)
		((le (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r) t)
		((le (distance-squared-to-line-2d l2_p1 l1_p1 l1_p2) r) t)
		((le (distance-squared-to-line-2d l2_p2 l1_p1 l1_p2) r) t)))

;generic path stuff

(defun remove-coincident-polyline-2d (_)
	(reduce (lambda (l _)
		(cond
			((ne (length l) 0)
				(defq p (elem -2 l))
				(if (or (ne (elem 0 p) (elem 0 _))
						(ne (elem 1 p) (elem 1 _)))
					(push l _)))
			(t (push l _))) l) _ (list)))

(defun remove-coincident-polygon-2d (_)
	(defq _ (remove-coincident-polyline-2d _)
		p1 (elem 0 _) p2 (elem -2 _))
	(if (and (eq (elem 0 p1) (elem 0 p2))
			(eq (elem 1 p1) (elem 1 p2)))
		(pop _)) _)

(defun gen-arc-polyline-2d (out_points p r a1 a2)
	(when (ne r 0)
		(setq a1 (fp-mod a1 fp-2pi) a2 (if (gt a2 fp-2pi) (fp-mod a2 fp-2pi) a2))
		(cond
			((le a2 fp-three)
				(setq a2 (add a1 a2))
				(defq v1 (list (fp-mul r (fp-sin a1)) (fp-mul r (fp-cos a1)))
					v2 (list (fp-mul r (fp-sin a2)) (fp-mul r (fp-cos a2))))
				(gen-clerp-polyline-2d out_points p v1 v2 r))
			(t
				(defq ah (bit-asr a2 1) a2 (add a1 ah))
				(gen-arc-polyline-2d out_points p r a1 ah)
				(gen-arc-polyline-2d out_points p r a2 ah))))
	out_points)

(defun gen-clerp-polyline-2d (out_points p1 v2 v3 r)
	(when (ne r 0)
		(defq stack (list v2 v3))
		(push out_points (vec-add-2d p1 v2))
		(while (defq v4 (pop stack) v2 (pop stack))
			;calculate the mid-point
			(defq bv (vec-scale-2d (vec-norm-2d (vec-add-2d v2 v4)) r)
				x1 (elem 0 v2) y1 (elem 1 v2)
				x2 (elem 0 bv) y2 (elem 1 bv)
				x3 (elem 0 v4) y3 (elem 1 v4))

			;flatness test
			(cond
				((le (add (abs (sub (add x1 x3) x2 x2))
						(abs (sub (add y1 y3) y2 y2))) fp-two)
					(push out_points (vec-add-2d p1 bv)))
				(t
					;continue subdivision
					(push stack bv v4 v2 bv))))
		(push out_points (vec-add-2d p1 v3)))
	out_points)

(defun gen-bezier-polyline-2d (out_points p1 p2 p3 p4)
	(defq stack (cat p1 p2 p3 p4))
	(push out_points p1)
	(while (defq y4 (pop stack) x4 (pop stack)
		y3 (pop stack) x3 (pop stack)
		y2 (pop stack) x2 (pop stack)
		y1 (pop stack) x1 (pop stack))

		;calculate all the mid-points of the line segments
		(defq x12 (bit-asr (add x1 x2) 1) y12 (bit-asr (add y1 y2) 1)
			x23 (bit-asr (add x2 x3) 1) y23 (bit-asr (add y2 y3) 1)
			x34 (bit-asr (add x3 x4) 1) y34 (bit-asr (add y3 y4) 1)
			x123 (bit-asr (add x12 x23) 1) y123 (bit-asr (add y12 y23) 1)
			x234 (bit-asr (add x23 x34) 1) y234 (bit-asr (add y23 y34) 1)
			x1234 (bit-asr (add x123 x234) 1) y1234 (bit-asr (add y123 y234) 1))

		(cond
			((le (add (abs (sub (add x1 x3) x2 x2))
					(abs (sub (add y1 y3) y2 y2))
					(abs (sub (add x2 x4) x3 x3))
					(abs (sub (add y2 y4) y3 y3))) fp-two)
				(push out_points (list x1234 y1234)))
			(t
				;continue subdivision
				(push stack x1234 y1234 x234 y234 x34 y34 x4 y4
							x1 y1 x12 y12 x123 y123 x1234 y1234))))
	(push out_points p4))

(defun gen-polyline-joints-2d (out_points in_points p1 p2 i j)
	(when (ne r 0)
		(defq l2_v (vec-sub-2d p2 p1)
			l2_pv (vec-perp-2d l2_v)
			l2_npv (vec-norm-2d l2_pv)
			l2_rv (vec-scale-2d l2_npv r))
		(each! i j nil (lambda (_)
			(defq l1_v l2_v l1_npv l2_npv l1_rv l2_rv)
			(setq p1 p2 p2 _
				l2_v (vec-sub-2d p2 p1)
				l2_pv (vec-perp-2d l2_v)
				l2_npv (vec-norm-2d l2_pv)
				l2_rv (vec-scale-2d l2_npv r))
			(defq nbv (vec-norm-2d (vec-add-2d l1_npv l2_npv))
				c (vec-dot-2d nbv (vec-norm-2d l1_v)))
			(cond
				((or (le c 0) (eq join_style mitre-join))
					;mitre join
					(push out_points (vec-intersect-2d
						(vec-add-2d p1 l1_rv) l1_v
						(vec-add-2d p1 l2_rv) l2_v)))
				((eq join_style bevel-join)
					;bevel join
					(push out_points
						(vec-add-2d p1 l1_rv)
						(vec-add-2d p1 l2_rv)))
				((eq join_style round-join)
					;rounded join
					(gen-clerp-polyline-2d out_points p1 l1_rv l2_rv r))
				(t (throw "Missing join style " join_style)))) (list in_points)))
	out_points)

(defun stroke-polyline-2d (out_polygons r join_style cap1_style cap2_style in_polylines)
 	(when (ne r 0)
	 	(each (lambda (_)
			(setq _ (remove-coincident-polyline-2d _))
			(defq index 0 step 1 sides 2 out_points (list))
			(while (ge (setq sides (dec sides)) 0)
				(defq p1 (elem index _)
					index (add index step)
					p2 (elem index _)
					index (add index step)
					l2_v (vec-sub-2d p2 p1)
					l2_pv (vec-perp-2d l2_v)
					l2_npv (vec-norm-2d l2_pv)
					l2_rv (vec-scale-2d l2_npv r)
					c (if (eq sides 0) cap2_style cap1_style))
				(cond
					((eq c butt-cap)
						;butt cap
						(push out_points
							(vec-sub-2d p1 l2_rv)
							(vec-add-2d p1 l2_rv)))
					((eq c square-cap)
						;square cap
						(defq p0 (vec-add-2d p1 (vec-perp-2d l2_rv)))
						(push out_points
							(vec-sub-2d p0 l2_rv)
							(vec-add-2d p0 l2_rv)))
					((eq c tri-cap)
						;triangle cap
						(push out_points
							(vec-sub-2d p1 l2_rv)
							(vec-add-2d p1 (vec-perp-2d l2_rv))
							(vec-add-2d p1 l2_rv)))
					((eq c arrow-cap)
						;arrow cap
						(defq rv (vec-scale-2d l2_rv fp-two))
						(push out_points
							(vec-sub-2d p1 l2_rv)
							(vec-sub-2d p1 rv)
							(vec-add-2d p1 (vec-perp-2d rv))
							(vec-add-2d p1 rv)
							(vec-add-2d p1 l2_rv)))
					((eq c round-cap)
						;round cap
						(defq pv (vec-perp-2d l2_rv))
						(gen-clerp-polyline-2d out_points p1 (vec-scale-2d l2_rv (neg fp-one)) pv r)
						(gen-clerp-polyline-2d out_points p1 pv l2_rv r))
					(t (throw "Missing cap sytle " c)))
				(while (and (ne index -1) (ne index (length _)))
					(defq p1 p2 l1_v l2_v l1_npv l2_npv l1_rv l2_rv
						p2 (elem index _)
						index (add index step)
						l2_v (vec-sub-2d p2 p1)
						l2_pv (vec-perp-2d l2_v)
						l2_npv (vec-norm-2d l2_pv)
						l2_rv (vec-scale-2d l2_npv r)
						nbv (vec-norm-2d (vec-add-2d l1_npv l2_npv))
						c (vec-dot-2d nbv (vec-norm-2d l1_v)))
					(cond
						((or (le c 0) (eq join_style mitre-join))
							;mitre join
							(push out_points (vec-intersect-2d
								(vec-add-2d p1 l1_rv) l1_v
								(vec-add-2d p1 l2_rv) l2_v)))
						((eq join_style bevel-join)
							;bevel join
							(push out_points
								(vec-add-2d p1 l1_rv)
								(vec-add-2d p1 l2_rv)))
						((eq join_style round-join)
							;rounded join
							(gen-clerp-polyline-2d out_points p1 l1_rv l2_rv r))
						(t (throw "Missing join style " join_style))))
				(setq step (neg step) index (add index step)))
			(push out_polygons out_points)) in_polylines))
	out_polygons)

(defun stroke-polygon-2d (out_polygons r join_style in_polygons)
	(when (ne r 0)
		(each (lambda (_)
			(setq _ (remove-coincident-polygon-2d _))
			(push out_polygons (gen-polyline-joints-2d (list) _ (elem -3 _) (elem -2 _) 0 (length _)))
			(push out_polygons (gen-polyline-joints-2d (list) _ (elem 1 _) (elem 0 _) (length _) 0)))
			in_polygons))
	out_polygons)
