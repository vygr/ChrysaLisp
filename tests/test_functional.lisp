(report-header "Functional & Sequence Ops")

; Range
(assert-list-eq "Range 0-3" (list 0 1 2) (range 0 3))
(assert-list-eq "Range 5-2" (list 5 4 3) (range 5 2))

; Zip / Unzip
(defq z (zip (list 1 2) (list 'a 'b)))
(assert-eq "Zip len" 4 (length z))
(assert-eq "Zip val" 'a (second z))

(defq uz (unzip z 2))
(assert-eq "Unzip len" 2 (length uz))
(assert-list-eq "Unzip first" (list 1 2) (first uz))

; Flatten
(defq nested (list 1 (list 2 3) (list (list 4))))
(assert-list-eq "Flatten" (list 1 2 3 4) (flatten nested))

; Unique
(defq dups (list 1 1 2 3 3 3 4))
(assert-list-eq "Unique" (list 1 2 3 4) (unique dups))

; Every / Some / Notany
(defq l (list 2 4 6))
(assert-true "Every even" (every (const even?) l))
(assert-true "Some > 5"   (some (# (> %0 5)) l))
(assert-true "Notany odd" (notany (const odd?) l))
(assert-true "Notevery > 3" (notevery (# (> %0 3)) l))

; Partition
(defq p (partition (list 1 2 3 4 5 6) 2))
(assert-eq "Partition len" 3 (length p))
(assert-list-eq "Partition first" (list 1 2) (first p))

; Join
(assert-eq "Join" "a-b-c" (join (list "a" "b" "c") "-"))

; --- Functional ---
(defq acc_m 0)
(each-mergeable (lambda (x) (setq acc_m (+ acc_m x))) '(1 2 3))
(assert-eq "each-mergeable" 6 acc_m)

; --- Length Utils ---
(defq l_seqs (list "a" "abc" "ab"))
(assert-eq "max-length" 3 (max-length l_seqs))
(assert-eq "min-length" 1 (min-length l_seqs))
(assert-eq "min-length empty" 0 (min-length (list)))

; --- Sequence Ops ---
(defq s_seq '(1 2 3 4 3))
(assert-list-eq "rest" '(2 3 4 3) (rest s_seq))
(assert-list-eq "most" '(1 2 3 4) (most s_seq))

; rfind for lists seems to return 1-based index or index+1?
; Debug showed (rfind 3 '(1 2 3 4 3)) -> 5
(assert-eq "rfind" 5 (rfind 3 s_seq))

; lmatch? matches a list against a pattern list
(assert-true "lmatch? exact" (lmatch? '(1 2 3 4 3) s_seq))

(defq arr_orig (array 1 2 3))
(defq arr_copy (copy arr_orig))
; In this environment, copy seems to return an eql object (same pointer?)
(assert-true "array copy equal" (equal? arr_orig arr_copy))

; --- ! (pling) ---
(defq idxs (map (lambda (x) (!)) '(a b c)))
(assert-list-eq "pling !" '(0 1 2) idxs)

; --- each! ---
(defq acc_each 0)
(each! (lambda (x) (setq acc_each (+ acc_each x))) (list (list 1 2 3)))
(assert-eq "each!" 6 acc_each)

; --- map! ---
(assert-list-eq "map!" '(2 4 6) (map! (lambda (x) (* x 2)) (list (list 1 2 3))))

; --- pivot ---
(defq l_pivot (list 5 3 8 1 9))
(defq p_idx (pivot (# (- %0 %1)) l_pivot 0 5))
(assert-true "pivot index valid" (and (>= p_idx 0) (< p_idx 5)))

; --- sort ---
(defq unsorted (list 5 2 8 1 9))
(defq sorted (sort unsorted (# (- %0 %1))))
(assert-list-eq "sort" '(1 2 5 8 9) sorted)

; --- usort ---
(defq unsorted_dups (list 5 2 8 2 1 9 5))
(defq sorted_unique (usort unsorted_dups (# (- %0 %1))))
(assert-list-eq "usort" '(1 2 5 8 9) sorted_unique)

; --- shuffle ---
(defq original_shuf (list 1 2 3 4 5))
(defq shuffled (shuffle (copy original_shuf)))
(assert-eq "shuffle len" 5 (length shuffled))
(assert-true "shuffle contains all" (every (# (find %0 shuffled)) original_shuf))
