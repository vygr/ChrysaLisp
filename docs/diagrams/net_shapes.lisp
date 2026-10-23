;docs/intro/intro.md: the shapes of a network that run.sh -t makes, small ones of each
(defq s (dia-scene) blue 0xffd7e6f7 dark 0xff202428
	ring '((100 55) (88 83) (60 95) (32 83) (20 55) (32 27) (60 15) (88 27)))
(defun net-shape (at name under nodes links)
	;a shape at a place across: its links, then its nodes over them, then
	;its name under it. nodes (x y), links pairs of their numbers
	(defq ox (* at 125))
	(each (lambda ((a b))
			(bind '(x y) (elem-get nodes a))
			(bind '(x1 y1) (elem-get nodes b))
			(push (first s) (cwb-shape (cwb-d "M" (+ ox x) (+ 20 y) "L" (+ ox x1) (+ 20 y1))
				:stroke 0xff3c4650 :width 1.2)))
		links)
	(each (lambda ((x y))
			(push (second s) (cwb-shape (cwb-d-ellipse (+ ox x) (+ 20 y) 6.5) :fill blue :stroke 0xff3c4650 :width 1.2)))
		nodes)
	(dia-say s name (+ (n2f ox) 60.0 (* -0.5 (dia-width name 12 :t))) 140.0 12 :t dark)
	(dia-say s under (+ (n2f ox) 60.0 (* -0.5 (dia-width under 11))) 156.0 11))
(net-shape 0 "full" "each to each, the default"
	'((60 15) (98 43) (84 88) (36 88) (22 43))
	'((0 1) (0 2) (0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
(net-shape 1 "ring" "two neighbours"
	ring '((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 0)))
(net-shape 2 "star" "all to the first"
	(cat '((60 55)) (slice ring 0 7)) '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7)))
(net-shape 3 "tree" "one above, two below"
	'((60 15) (32 50) (88 50) (17 88) (47 88) (73 88) (103 88))
	'((0 1) (0 2) (1 3) (1 4) (2 5) (2 6)))
(net-shape 4 "mesh" "a grid, four each"
	'((25 20) (60 20) (95 20) (25 55) (60 55) (95 55) (25 90) (60 90) (95 90))
	'((0 1) (1 2) (3 4) (4 5) (6 7) (7 8) (0 3) (3 6) (1 4) (4 7) (2 5) (5 8)))
(net-shape 5 "cube" "in three, six each"
	'((20 45) (70 45) (70 95) (20 95) (45 20) (95 20) (95 70) (45 70))
	'((0 1) (1 2) (2 3) (3 0) (4 5) (5 6) (6 7) (7 4) (0 4) (1 5) (2 6) (3 7)))
(dia-say s "./run.sh -t <shape> -n <count>: a mesh or a cube -n wide, the edges of each wrapping round to the other side." 0 184 11)
(diagram "net_shapes" (dia-scene-doc s))
