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
(assert-eq "its corners" "((50.00000 30.00000 30.00000 30.00000 30.00000 50.00000 50.00000 50.00000))"
	(str (poly-intersect pg_sq pg_sq2)))
(assert-eq "the first and not the second" 1200.0 (poly-area (poly-difference pg_sq pg_sq2)))
(assert-eq "an L of 6 corners" "(6)" (str (pg-points (poly-difference pg_sq pg_sq2))))
(assert-eq "just one of them" 2400.0 (poly-area (poly-xor pg_sq pg_sq2)))

;edges that lie along each other
(defq pg_a (list (path 0.0 0.0 40.0 0.0 40.0 40.0 0.0 40.0)) pg_b (list (path 40.0 0.0 80.0 0.0 80.0 40.0 40.0 40.0))
	pg_c (list (path 40.0 10.0 80.0 10.0 80.0 30.0 40.0 30.0)) pg_in (list (path 10.0 10.0 30.0 10.0 30.0 30.0 10.0 30.0)))
(assert-eq "two squares side by side are one box, of 4 corners" "((80.00000 0.00000 0.00000 0.00000 0.00000 40.00000 80.00000 40.00000))"
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
