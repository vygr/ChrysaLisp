(report-header "Polygon: set logic of polygons, in fixed point")

(import "gui/lisp.inc")
(import "lib/math/polygon.inc")

(defq pg_sq (list (path 10.0 10.0 50.0 10.0 50.0 50.0 10.0 50.0))
	pg_sq2 (list (path 30.0 30.0 70.0 30.0 70.0 70.0 30.0 70.0)))
(defun pg-points (paths) (map (# (/ (length %0) 2)) paths))

;two squares of 40 that share a square of 20
(assert-eq "either: the two less what they share" 2800.0 (poly-area (poly-union pg_sq pg_sq2)))
(assert-eq "one ring of 8 corners" "(8)" (str (pg-points (poly-union pg_sq pg_sq2))))
(assert-eq "both: what they share" 400.0 (poly-area (poly-intersect pg_sq pg_sq2)))
(assert-eq "its corners" "((30.00000 30.00000 30.00000 50.00000 50.00000 50.00000 50.00000 30.00000))"
	(str (poly-intersect pg_sq pg_sq2)))
(assert-eq "the first and not the second" 1200.0 (poly-area (poly-difference pg_sq pg_sq2)))
(assert-eq "an L of 6 corners" "(6)" (str (pg-points (poly-difference pg_sq pg_sq2))))
(assert-eq "just one of them" 2400.0 (poly-area (poly-xor pg_sq pg_sq2)))

;edges that lie along each other
(defq pg_a (list (path 0.0 0.0 40.0 0.0 40.0 40.0 0.0 40.0)) pg_b (list (path 40.0 0.0 80.0 0.0 80.0 40.0 40.0 40.0))
	pg_c (list (path 40.0 10.0 80.0 10.0 80.0 30.0 40.0 30.0)) pg_in (list (path 10.0 10.0 30.0 10.0 30.0 30.0 10.0 30.0)))
(assert-eq "two squares side by side are one box, of 4 corners" "((0.00000 0.00000 0.00000 40.00000 80.00000 40.00000 80.00000 0.00000))"
	(str (poly-union pg_a pg_b)))
(assert-eq "and share nothing" 0 (length (poly-intersect pg_a pg_b)))
(assert-eq "a side shared in part: 8 corners" "(8)" (str (pg-points (poly-union pg_a pg_c))))
(assert-eq "and all of both" 2400.0 (poly-area (poly-union pg_a pg_c)))
(assert-eq "a thing less itself is nothing" 0 (length (poly-difference pg_a pg_a)))
(assert-eq "a thing and itself is itself" "(4)" (str (pg-points (poly-intersect pg_a pg_a))))
(assert-eq "a thing or itself is itself" 1600.0 (poly-area (poly-union pg_a pg_a)))
(assert-eq "a thing or nothing is itself" 1600.0 (poly-area (poly-union pg_a (list))))
(assert-eq "nothing and nothing is nothing" 0 (length (poly-union (list) (list))))

;a hole
(defq pg_hole (poly-difference pg_a pg_in))
(assert-eq "a square with a square taken from its middle is two rings" 2 (length pg_hole))
(assert-eq "of the area left" 1200.0 (poly-area pg_hole))
(assert-true "the hole is not in it, by either rule" (and
	(not (vector-point-in-polygon (path 20.0 20.0) pg_hole +winding_odd_even))
	(not (vector-point-in-polygon (path 20.0 20.0) pg_hole +winding_none_zero))))
(assert-true "the rim is" (and
	(vector-point-in-polygon (path 5.0 5.0) pg_hole +winding_odd_even)
	(vector-point-in-polygon (path 5.0 5.0) pg_hole +winding_none_zero)))
(assert-eq "what was taken put back is the square, 4 corners" "(4)" (str (pg-points (poly-union pg_hole pg_in))))

;the winding rule of a set
(defq pg_star (list (path 50.0 0.0 79.4 90.5 2.4 34.5 97.6 34.5 20.6 90.5)))
(assert-true "a star that crosses itself has more in it by non zero than by odd even"
	(> (poly-area (poly-union pg_star (list) 1)) (poly-area (poly-union pg_star (list) 0))))
(assert-true "its middle is in by one rule" (vector-point-in-polygon (path 50.0 45.0) (poly-union pg_star (list) 1) +winding_odd_even))
(assert-true "and out by the other" (not (vector-point-in-polygon (path 50.0 45.0) (poly-union pg_star (list) 0) +winding_odd_even)))

;any two polygons, each tried at many points: what the answer says of a point is what the two say
(defq pg_seed 12345)
(defun pg-rnd (n) (setq pg_seed (logand (+ (* pg_seed 1103515245) 12345) 0x7fffffff)) (% (>> pg_seed 8) n))
(defun pg-any (n size)
	(defq p (path))
	(times n (push p (/ (n2f (pg-rnd (* size 256))) 256.0) (/ (n2f (pg-rnd (* size 256))) 256.0)))
	(list p))
(defun pg-in? (x y paths) (if (vector-point-in-polygon (path x y) paths +winding_odd_even) :t :nil))
(defq pg_bad 0 pg_out 0.0)
(times 40
	;a long loop of Lisp holds its node, and what times a task beside it fails
	(task-slice)
	(defq a (pg-any (+ 3 (pg-rnd 8)) 100) b (pg-any (+ 3 (pg-rnd 8)) 100)
		u (poly-union a b) i (poly-intersect a b) d (poly-difference a b) d2 (poly-difference b a) x (poly-xor a b))
	(times 40
		(defq px (+ (n2f (pg-rnd 100)) 0.3711) py (+ (n2f (pg-rnd 100)) 0.4173)
			ia (pg-in? px py a) ib (pg-in? px py b))
		(unless (and (eql (pg-in? px py u) (if (or ia ib) :t :nil))
				(eql (pg-in? px py i) (if (and ia ib) :t :nil))
				(eql (pg-in? px py d) (if (and ia (not ib)) :t :nil))
				(eql (pg-in? px py x) (if (eql ia ib) :nil :t)))
			(setq pg_bad (inc pg_bad))))
	;and the areas add up
	(defq sa (poly-area (poly-union a (list))) sb (poly-area (poly-union b (list))))
	(setq pg_out (max pg_out (abs (- (+ sa sb) (+ (poly-area u) (poly-area i))))
		(abs (- sa (+ (poly-area d) (poly-area i)))) (abs (- (poly-area x) (+ (poly-area d) (poly-area d2)))))))
(assert-eq "40 pairs of polygons that cross themselves and each other, 1600 points: none wrong" 0 pg_bad)
(assert-true "and their areas add up, to a hundredth of a pixel in ten thousand" (< pg_out 0.01))

;a bite out of a line, as an eraser takes one
(defq pg_line (path-stroke-polylines (list) 6.0 +join_round +cap_round +cap_round
		(list (path 10.0 10.0 200.0 60.0 120.0 200.0 300.0 220.0)))
	pg_rub (path-stroke-polylines (list) 12.0 +join_round +cap_round +cap_round
		(list (path 40.0 80.0 150.0 30.0 260.0 250.0)))
	pg_bit (poly-difference pg_line pg_rub 1 1) pg_took (poly-intersect pg_line pg_rub 1 1))
(assert-true "a line with a line rubbed across it three times is in pieces" (> (length pg_bit) 2))
(assert-true "what is left and what was taken are the line" (< (abs (- (poly-area (poly-union pg_line (list) 1))
	(+ (poly-area pg_bit) (poly-area pg_took)))) 0.01))
(assert-true "a point of it that was rubbed is gone" (not (pg-in? 150.0 47.0 pg_bit)))
(assert-true "one that was not is there" (pg-in? 20.0 12.6 pg_bit))

;the hull, and what a shape covers as it is moved
(assert-eq "the hull of a shape with a dent has none" "(0.00000 0.00000 10.00000 0.00000 10.00000 10.00000 0.00000 10.00000)"
	(str (poly-hull (list (path 0.0 0.0 10.0 0.0 5.0 5.0 10.0 10.0 0.0 10.0 5.0 2.0)))))
(assert-eq "points in a line give its ends" "(0.00000 0.00000 20.00000 0.00000)"
	(str (poly-hull (list (path 0.0 0.0 10.0 0.0) (path 20.0 0.0)))))
(defq pg_cloth (path -5.0 -5.0 5.0 -5.0 5.0 5.0 -5.0 5.0))
(assert-eq "a square moved along and then down covers an L" 2100.0
	(poly-area (poly-sweep pg_cloth (path 0.0 0.0 100.0 0.0 100.0 100.0))))
(assert-eq "of 6 corners" "(6)" (str (pg-points (poly-sweep pg_cloth (path 0.0 0.0 100.0 0.0 100.0 100.0)))))
(assert-eq "moved on a slant, 6 corners too" "(6)" (str (pg-points (poly-sweep pg_cloth (path 0.0 0.0 50.0 30.0)))))
(assert-eq "not moved it is where it is" "((2.00000 2.00000 12.00000 2.00000 12.00000 12.00000 2.00000 12.00000))"
	(str (poly-sweep pg_cloth (path 7.0 7.0))))
(assert-eq "and nowhere is nothing" 0 (length (poly-sweep pg_cloth (path))))

;the native code of a band and the Lisp it was written from, held together:
;bands of edges at random, in any order, some that end at the top of the
;band, some that cross and some that lie along each other, by every op and
;rule. And what is made of the pieces, the lines and the ys of the crossings
(defq pg_seed 777 pg_bands 0 pg_differ 0 pg_cross 0 pg_lines 0)
(times 400
	(task-slice)
	(defq n (+ 1 (pg-rnd 12)) edges (nums) active (nums) y0 (+ 1000 (pg-rnd 5000)) y1 (+ y0 1 (pg-rnd 5000)))
	(times n
		;an edge that is in the band, from at or above its top to at or below
		;its bottom, or now and then one that ends at its top
		(defq yt (- y0 (if (= 0 (pg-rnd 3)) 0 (pg-rnd 3000))) yb (+ y1 (if (= 0 (pg-rnd 3)) 0 (pg-rnd 3000)))
			xt (* 64 (pg-rnd 600)) xb (if (= 0 (pg-rnd 4)) xt (* 64 (pg-rnd 600))))
		(when (= 0 (pg-rnd 8)) (setq yt (- y0 500) yb y0))
		(push edges yt yb xt xb (if (= 0 (pg-rnd 2)) 1 -1) (pg-rnd 2) (/ (length edges) +poly_edge) 0 0 0)
		(push active (length active)))
	;now and then one over another
	(and (> n 2) (= 0 (pg-rnd 3))
		(each (# (elem-set edges (+ +poly_edge %0) (elem-get edges %0))) (range 0 4)))
	(shuffle active)
	(each (lambda (mode)
		(defq e1 (cat edges) a1 (cat active) o1 (nums) e2 (cat edges) a2 (cat active) o2 (nums 7 7 7)
			r1 (poly-band-ref e1 a1 y0 y1 mode o1) r2 (poly-band e2 a2 y0 y1 mode o2))
		(setq pg_bands (inc pg_bands))
		(unless (and (eql (str r1) (str r2)) (eql (str a1) (str a2)) (eql (str e1) (str e2)))
			(setq pg_differ (inc pg_differ)))
		(cond
			(r1 ;the lines of the pieces, on what a band above might have left
				(defq above (nums) segs (nums) openl (nums) openr (nums) x (* 64 (pg-rnd 100)))
				(times (pg-rnd 4)
					(push above x (setq x (+ x 64 (* 64 (pg-rnd 200)))))
					(setq x (+ x (* 64 (pg-rnd 200)))))
				(times n (push openl -1) (push openr -1))
				;a line or two from that band, that a piece here may go on from
				(when (and (>= (length o1) 6) (= 0 (pg-rnd 2)))
					(push segs (elem-get o1 0) (- y0 640) (elem-get o1 0) y0 (elem-get o1 4))
					(elem-set openl (elem-get o1 4) 0)
					(push segs (elem-get o1 2) y0 (elem-get o1 2) (- y0 640) (elem-get o1 5))
					(elem-set openr (elem-get o1 5) 5))
				(defq s1 (cat segs) l1 (cat openl) p1 (cat openr) b1 (nums 1 2)
					s2 (cat segs) l2 (cat openl) p2 (cat openr) b2 (nums))
				(poly-lines-ref o1 y0 y1 above b1 s1 l1 p1)
				(poly-lines o2 y0 y1 above b2 s2 l2 p2)
				(setq pg_lines (+ pg_lines (/ (length s1) 5)))
				(unless (and (eql (str s1) (str s2)) (eql (str b1) (str b2)) (eql (str l1) (str l2)) (eql (str p1) (str p2)))
					(setq pg_differ (inc pg_differ))))
			(:t ;where they cross
				(setq pg_cross (inc pg_cross))
				(unless (eql (str (poly-cuts-ref e1 a1 y0 y1 (nums y0 y1))) (str (poly-cuts e2 a2 y0 y1 (nums y0 y1))))
					(setq pg_differ (inc pg_differ))))))
		(list (pg-rnd 16) (+ 16 (pg-rnd 16)))))
(assert-eq "800 bands, the native code gives what the Lisp gives, every number of it" 0 pg_differ)
(assert-true "and some of them were bands that edges cross in, and some were not" (< 50 pg_cross 350))
(assert-true "and lines were made of them" (> pg_lines 1000))

;do two shapes with no dent touch, (poly-convex-hit?), held to the set logic
(defq pg_h1 (poly-hull (list (path 10.0 10.0 50.0 10.0 50.0 50.0 10.0 50.0)))
	pg_h2 (poly-hull (list (path 30.0 30.0 70.0 30.0 70.0 70.0 30.0 70.0)))
	pg_h3 (poly-hull (list (path 60.0 10.0 80.0 10.0 80.0 15.0 60.0 15.0)))
	pg_line (poly-hull (list (path 0.0 100.0 100.0 0.0))))
(assert-true "two squares that overlap touch" (poly-convex-hit? pg_h1 pg_h2))
(assert-true "two that are apart do not" (not (poly-convex-hit? pg_h1 pg_h3)))
(assert-true "one inside the other" (poly-convex-hit? pg_h1 (poly-hull (list (path 20.0 20.0 30.0 20.0 30.0 30.0 20.0 30.0)))))
(assert-true "a slanted line, and a square in the corner of its box that it does not cross" (not (poly-convex-hit? pg_line pg_h3)))
(assert-true "and one it does cross" (poly-convex-hit? pg_line (poly-hull (list (path 40.0 40.0 60.0 40.0 60.0 60.0 40.0 60.0)))))
(assert-eq "the furthest point of a hull a way, climbed to from anywhere" "(50.00000 50.00000)"
	(str (map (# (elem-get pg_h1 (+ %0 (poly-support pg_h1 4 1.0 1.0)))) '(0 1))))
;600 pairs of hulls of random points: never apart when the set logic has them share something of size
(defq pg_hseed 4321 pg_wrong 0 pg_apart 0 pg_agree 0)
(defun pg-hrnd (n) (setq pg_hseed (logand (+ (* pg_hseed 1103515245) 12345) 0x7fffffff)) (% (>> pg_hseed 8) n))
(defun pg-blob (cx cy r n)
	(defq p (path))
	(times n (push p (n2f (+ cx (- (pg-hrnd (* 2 r)) r))) (n2f (+ cy (- (pg-hrnd (* 2 r)) r)))))
	(poly-hull (list p)))
(times 600
	(defq a (pg-blob (+ 100 (pg-hrnd 200)) (+ 100 (pg-hrnd 200)) (+ 5 (pg-hrnd 60)) (+ 3 (pg-hrnd 12)))
		b (pg-blob (+ 100 (pg-hrnd 200)) (+ 100 (pg-hrnd 200)) (+ 5 (pg-hrnd 60)) (+ 3 (pg-hrnd 12))))
	(when (and (>= (length a) 6) (>= (length b) 6))
		(defq shared (poly-area (poly-intersect (list a) (list b))) hit (poly-convex-hit? a b))
		(cond
			((and (> shared 0.01) (not hit)) (setq pg_wrong (inc pg_wrong)))
			((not hit) (setq pg_apart (inc pg_apart)))
			((> shared 0.0) (setq pg_agree (inc pg_agree)))))
	(task-slice))
(assert-eq "of 600 pairs, none that share anything is said to be apart" 0 pg_wrong)
(assert-true "many are apart, and many touch" (and (> pg_apart 100) (> pg_agree 100)))
;the lines joined into rings by the native code are the rings the Lisp gives, of 300 pairs of random shapes
(defq pg_rwrong 0 pg_rings 0)
(defun pg-shape (cx cy r n)
	(defq p (path))
	(times n (push p (n2f (+ cx (- (pg-hrnd (* 2 r)) r))) (n2f (+ cy (- (pg-hrnd (* 2 r)) r)))))
	p)
(times 300
	(defq a (list (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 14))) (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 9))))
		b (list (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 14))))
		segs (poly-segs (elem-get '(:union :intersect :difference :xor) (pg-hrnd 4)) a b (pg-hrnd 2) (pg-hrnd 2))
		want (poly-fixed (poly-rings-ref segs)) got (poly-rings segs))
	(setq pg_rings (+ pg_rings (length got)))
	(unless (eql (str want) (str got)) (setq pg_rwrong (inc pg_rwrong)))
	(task-slice))
(assert-eq "of 300, none joined another way" 0 pg_rwrong)
(assert-true "and they were rings" (> pg_rings 300))
(assert-eq "no lines is no rings" "()" (str (poly-rings (nums))))
;every band done by the native code gives the lines the Lisp gives, of 300 pairs, many with edges that cross
(defq pg_swrong 0 pg_slines 0)
(times 300
	(defq a (list (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 14))) (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 9))))
		b (list (pg-shape 150 150 (+ 10 (pg-hrnd 90)) (+ 3 (pg-hrnd 14))))
		mode (+ (pg-hrnd 4) (* 4 (pg-hrnd 4)))
		e1 (nums) y1 (nums) l1 (nums) r1 (nums) e2 (nums) y2 (nums) l2 (nums) r2 (nums))
	(poly-setup a b e1 y1 l1 r1) (poly-setup a b e2 y2 l2 r2)
	(defq want (poly-scan-ref e1 y1 l1 r1 mode (nums)) got (poly-scan e2 y2 l2 r2 mode (nums)))
	(setq pg_slines (+ pg_slines (/ (length got) 5)))
	(unless (eql (str want) (str got)) (setq pg_swrong (inc pg_swrong)))
	(task-slice))
(assert-eq "of 300, none with other lines" 0 pg_swrong)
(assert-true "and there were lines" (> pg_slines 3000))
(assert-eq "nothing is no lines" "()" (str (poly-scan (nums) (nums) (nums) (nums) 0 (nums))))
;do two sets touch, of any shape
(defun pg-box (x y w h) (path x y (+ x w) y (+ x w) (+ y h) x (+ y h)))
(defq pg_c (list (path 0.0 0.0 100.0 0.0 100.0 20.0 20.0 20.0 20.0 80.0 100.0 80.0 100.0 100.0 0.0 100.0))
	pg_ring (poly-difference (list (pg-box 0.0 0.0 100.0 100.0)) (list (pg-box 20.0 20.0 60.0 60.0))))
(assert-true "two apart do not touch" (not (poly-hit? (list (pg-box 0.0 0.0 10.0 10.0)) (list (pg-box 20.0 0.0 10.0 10.0)))))
(assert-true "two that cross do" (poly-hit? (list (pg-box 0.0 0.0 10.0 10.0)) (list (pg-box 5.0 5.0 10.0 10.0))))
(assert-true "one inside the other, no edge on an edge" (poly-hit? (list (pg-box 0.0 0.0 100.0 100.0)) (list (pg-box 40.0 40.0 5.0 5.0))))
(assert-true "and the other way round" (poly-hit? (list (pg-box 40.0 40.0 5.0 5.0)) (list (pg-box 0.0 0.0 100.0 100.0))))
(assert-true "a side on a side" (poly-hit? (list (pg-box 0.0 0.0 10.0 10.0)) (list (pg-box 10.0 2.0 10.0 5.0))))
(assert-true "a corner on a corner" (poly-hit? (list (pg-box 0.0 0.0 10.0 10.0)) (list (pg-box 10.0 10.0 10.0 10.0))))
(assert-true "in the mouth of a C, inside its hull, and not touching it" (not (poly-hit? pg_c (list (pg-box 40.0 40.0 30.0 20.0)))))
(assert-true "through the side of the C" (poly-hit? pg_c (list (pg-box 10.0 40.0 30.0 20.0))))
(assert-true "in the hole of a ring" (not (poly-hit? pg_ring (list (pg-box 40.0 40.0 10.0 10.0)))))
(assert-true "in the ring itself" (poly-hit? pg_ring (list (pg-box 5.0 5.0 5.0 5.0))))
(assert-true "nothing touches nothing" (not (poly-hit? (list) pg_c)))
;a star by its two rules: its middle is of it by the one and not by the other
(defq pg_star5 (list (path 50.0 0.0 79.0 90.0 2.0 35.0 98.0 35.0 21.0 90.0)) pg_dot (list (pg-box 49.0 44.0 2.0 2.0)))
(assert-true "the middle of a star, wound round" (poly-hit? pg_star5 pg_dot 1 0))
(assert-true "and not, odd and even" (not (poly-hit? pg_star5 pg_dot 0 0)))
;400 pairs of random shapes, at a fraction of a unit: they touch if and only if the set logic has them share something
(defq pg_hwrong 0 pg_hyes 0 pg_hno 0)
(defun pg-frac-shape (cx cy r n)
	(defq p (path))
	(times n (push p (+ (n2f (+ cx (- (pg-hrnd (* 2 r)) r))) (/ (n2f (pg-hrnd 997)) 997.0))
		(+ (n2f (+ cy (- (pg-hrnd (* 2 r)) r))) (/ (n2f (pg-hrnd 991)) 991.0))))
	p)
(times 400
	(defq a (list (pg-frac-shape (+ 100 (pg-hrnd 120)) (+ 100 (pg-hrnd 120)) (+ 8 (pg-hrnd 50)) (+ 3 (pg-hrnd 10))))
		b (list (pg-frac-shape (+ 100 (pg-hrnd 120)) (+ 100 (pg-hrnd 120)) (+ 8 (pg-hrnd 50)) (+ 3 (pg-hrnd 10))))
		shared (> (poly-area (poly-intersect a b)) 0.0) hit (poly-hit? a b))
	(cond
		;the Lisp it was written from tells the same
		((not (eql hit (poly-hit-ref? a b))) (setq pg_hwrong (inc pg_hwrong)))
		((and shared (not hit)) (setq pg_hwrong (inc pg_hwrong)))
		((and hit (not shared)) (setq pg_hwrong (inc pg_hwrong)))
		(hit (setq pg_hyes (inc pg_hyes)))
		(:t (setq pg_hno (inc pg_hno))))
	(task-slice))
(assert-eq "of 400 pairs, none told other than the set logic tells it, or than the Lisp does" 0 pg_hwrong)
(assert-true "many touch and many do not" (and (> pg_hyes 60) (> pg_hno 60)))
(assert-true "the Lisp tells the hand made ones the same" (and
	(not (poly-hit-ref? pg_c (list (pg-box 40.0 40.0 30.0 20.0)))) (poly-hit-ref? pg_c (list (pg-box 10.0 40.0 30.0 20.0)))
	(not (poly-hit-ref? pg_ring (list (pg-box 40.0 40.0 10.0 10.0)))) (poly-hit-ref? pg_ring (list (pg-box 5.0 5.0 5.0 5.0)))
	(poly-hit-ref? (list (pg-box 0.0 0.0 10.0 10.0)) (list (pg-box 10.0 10.0 10.0 10.0)))
	(poly-hit-ref? pg_star5 pg_dot 1 0) (not (poly-hit-ref? pg_star5 pg_dot 0 0))))
