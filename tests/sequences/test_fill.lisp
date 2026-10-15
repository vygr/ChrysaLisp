(report-header "Sequences: fill, an array made so long, each element the one given")
(assert-list-eq "nums" '(7 7 7 7 7) (fill (nums) 5 7))
(assert-list-eq "made shorter, the same" '(0 0) (fill (nums 1 2 3 4) 2 0))
(assert-eq "none" 0 (length (fill (nums 1 2 3) 0 9)))
(defq tf_big (fill (nums) 100000 -3))
(assert-eq "long" 100000 (length tf_big))
(assert-eq "and all of it" -300000 (nums-sum tf_big))
(assert-eq "the array it is given" :t (let ((a (nums 1))) (eql (fill a 3 2) a)))
(assert-list-eq "fixeds" '(1.5 1.5 1.5) (map (const identity) (fill (fixeds) 3 1.5)))
(assert-list-eq "reals" (list (n2r 2) (n2r 2)) (map (const identity) (fill (reals) 2 (n2r 2))))
(assert-eq "a path" 4 (length (fill (path) 4 1.0)))

;a list, a reference for each, and those it had let go of
(defq tf_s (cat "a" "b") tf_l (fill (list 1 2 3) 4 tf_s))
(assert-list-eq "a list" '("ab" "ab" "ab" "ab") tf_l)
(assert-eq "each the one object" :t (every (# (eql %0 tf_s)) tf_l))
(elem-set tf_l 0 "x")
(assert-list-eq "and one changed is the one" '("x" "ab" "ab" "ab") tf_l)
(assert-list-eq "a list filled with what it holds" '((1) (1) (1))
	(let ((l (list (list 1)))) (fill l 3 (first l))))
(assert-list-eq "with :nil" '(:nil :nil) (fill (list) 2 :nil))

;not a map or a set, their elements are their own
(assert-error "not a map" (fill (env 1) 2 1))
