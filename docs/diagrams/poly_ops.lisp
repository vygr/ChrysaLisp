;docs/ai_digest/polygon_sets.md: two shapes and the four things (poly-op) makes of them, each as it gave it
(import "lib/math/polygon.inc")
(defq s (dia-scene) blue 0xffd7e6f7 gold 0xffffe8a8 green 0xffbfe3b4 line 0xff3c4650
	a (list (path-gen-rect 10.0 20.0 110.0 100.0 14.0 14.0 (path)))
	b (list (path-gen-ellipse 110.0 70.0 60.0 46.0 (path)))
	draw (lambda (paths fill)
		;the rings of an answer as one shape, so a ring in a ring is a hole
		(list (cwb-shape (join (map (# (cwb-d-points %0 :t)) paths) " ") :fill fill :stroke line :width 1.4 :rule :evenodd) 170 120)))
(dia-put s :nil (draw a blue) 0 0)
(dia-put s :nil (draw b gold) 0 0)
(dia-say s "a, and b over it" 30 138 11 :t 0xff202428)
(each (lambda (op title)
	(defq x (* (inc (!)) 200))
	;the two as they were, faint, and the answer on them
	(dia-put s :nil (list (cwb-shape (cwb-d-points (first a) :t) :stroke 0xffc8ced4 :width 1.0) 170 120) x 0)
	(dia-put s :nil (list (cwb-shape (cwb-d-points (first b) :t) :stroke 0xffc8ced4 :width 1.0) 170 120) x 0)
	(defq r (poly-op op a b))
	(dia-put s :nil (draw r green) x 0)
	(dia-say s title (+ x 30) 138 11 :t 0xff202428)
	(dia-say s (cat (str (length r)) (if (= (length r) 1) " ring, " " rings, ") (str (n2i (poly-area r))) " square pixels") (+ x 30) 154 11))
	'(:union :intersect :difference :xor) '(":union, in either" ":intersect, in both" ":difference, in a, not b" ":xor, in just one"))
(diagram "poly_ops" (dia-scene-doc s))
