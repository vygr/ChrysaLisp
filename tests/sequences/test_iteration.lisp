(report-header "Iteration: !, each!, map!, every, some, etc.")

; Every / Some / Notany
(defq l (list 2 4 6))
(assert-true "Every even" (every (const even?) l))
(assert-true "Some > 5"   (some (# (> %0 5)) l))
(assert-true "Notany odd" (notany (const odd?) l))
(assert-true "Notevery > 3" (notevery (# (> %0 3)) l))

; --- Functional ---
(defq acc_m 0)
(each-mergeable (lambda (x) (setq acc_m (+ acc_m x))) '(1 2 3))
(assert-eq "each-mergeable" 6 acc_m)

; --- ! (pling) ---
(defq idxs (map (lambda (x) (!)) '(a b c)))
(assert-list-eq "pling !" '(0 1 2) idxs)

; --- each! ---
(defq acc_each 0)
(each! (lambda (x) (setq acc_each (+ acc_each x))) (list (list 1 2 3)))
(assert-eq "each!" 6 acc_each)

; --- map! ---
(assert-list-eq "map!" '(2 4 6) (map! (lambda (x) (* x 2)) (list (list 1 2 3))))

; --- High Level Iterators (map, each, reduce, filter) ---
(defq t_seq '(1 2 3 4))
(assert-list-eq "map" '(2 4 6 8) (map (# (* %0 2)) t_seq))
(assert-eq "reduce" 10 (reduce (const +) t_seq 0))
(assert-list-eq "filter" '(2 4) (filter (const even?) t_seq))

(defq acc_each_hl 0)
(each (# (setq acc_each_hl (+ acc_each_hl %0))) t_seq)
(assert-eq "each" 10 acc_each_hl)

; --- Reverse Iterators (reverse, rmap, reach, rreduce, rsome) ---
(assert-list-eq "reverse" '(4 3 2 1) (reverse t_seq))
(assert-list-eq "rmap" '(8 6 4 2) (rmap (# (* %0 2)) t_seq))
(assert-eq "rreduce" 10 (rreduce (const +) t_seq 0))

(defq acc_reach "")
(reach (# (setq acc_reach (cat acc_reach (str %0)))) t_seq)
(assert-eq "reach" "4321" acc_reach)

(assert-eq "rsome" 3 (rsome (# (if (odd? %0) %0 :nil)) t_seq))
