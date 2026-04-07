(report-header "Sorting: pivot, sort, usort, shuffle")

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
