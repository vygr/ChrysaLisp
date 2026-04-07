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
