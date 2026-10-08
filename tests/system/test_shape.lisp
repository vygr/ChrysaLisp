(report-header "Shapes: the links of a network, a ring, a star, a tree, a mesh, a cube")

(defun sh-count (shape num)
	;how many nodes, how many links, and the most and fewest links a node has
	(bind '(total pairs) (node-shape shape num))
	(defq deg (map (lambda (_) 0) (range 0 total)))
	(each (lambda ((i j))
		(elem-set deg i (inc (elem-get deg i)))
		(elem-set deg j (inc (elem-get deg j)))) pairs)
	(list total (length pairs) (reduce (const max) deg 0) (reduce (const min) deg (first deg))))

(defun sh-joined? (shape num)
	;can every node be reached from node 0
	(bind '(total pairs) (node-shape shape num))
	(defq seen (list 0) grew :t)
	(while grew
		(setq grew :nil)
		(each (lambda ((i j))
			(defq a (find i seen) b (find j seen))
			(cond
				((and a (not b)) (push seen j) (setq grew :t))
				((and b (not a)) (push seen i) (setq grew :t)))) pairs))
	(= (length seen) total))

(assert-list-eq "full, every node to every other" '(8 28 7 7) (sh-count :full 8))
(assert-list-eq "a ring, two links a node" '(8 8 2 2) (sh-count :ring 8))
(assert-list-eq "a ring of two is one link" '(2 1 1 1) (sh-count :ring 2))
(assert-list-eq "a star, all to node 0" '(8 7 7 1) (sh-count :star 8))
(assert-list-eq "a tree, one above and two below" '(15 14 3 1) (sh-count :tree 15))
(assert-list-eq "a mesh, a square that wraps, four links a node" '(16 32 4 4) (sh-count :mesh 4))
(assert-list-eq "a mesh two wide, a link is not made twice" '(4 4 2 2) (sh-count :mesh 2))
(assert-list-eq "a cube, six links a node" '(27 81 6 6) (sh-count :cube 3))
(assert-list-eq "one node has no links" '(1 0 0 0) (sh-count :ring 1))
(assert-list-eq "no more than 64 nodes" '(64 64 2 2) (sh-count :ring 500))
(assert-list-eq "32 for full" '(32 496 31 31) (sh-count :full 500))
(assert-list-eq "a mesh no wider than 8" '(64 128 4 4) (sh-count :mesh 20))
(assert-list-eq "a cube no wider than 4" '(64 192 6 6) (sh-count :cube 20))
(each (lambda (shape)
	(assert-true (cat "every node of a " (rest (str shape)) " can be reached") (sh-joined? shape 4))
	;with no number it is sized to the machine
	(defq total (first (node-shape shape)))
	(assert-true (cat "a " (rest (str shape)) " sized to the machine")
		(<= 1 total (min (pii-cpus) (if (eql shape :full) 32 64)))))
	'(:full :ring :star :tree :mesh :cube))
(assert-true "a link is from the lower node to the higher"
	(every (lambda ((i j)) (< i j)) (second (node-shape :cube 4))))
(assert-eq "node-spawn's links, this node and the new ones, each to each" 6
	(length (second (node-shape :full 4))))
