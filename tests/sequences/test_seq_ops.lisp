(report-header "Sequence Ops: lengths, rest and most, lmatch?, copy of maps and sets, slices")

; --- Length Utils ---
(defq l_seqs (list "a" "abc" "ab"))
(assert-eq "max-length" 3 (max-length l_seqs))
(assert-eq "min-length" 1 (min-length l_seqs))
(assert-eq "min-length empty" 0 (min-length (list)))

; --- Sequence Ops ---
(defq s_seq '(1 2 3 4 3))
(assert-list-eq "rest" '(2 3 4 3) (rest s_seq))
(assert-list-eq "most" '(1 2 3 4) (most s_seq))

; lmatch? matches a list against a pattern list
(assert-true "lmatch? exact" (lmatch? '(1 2 3 4 3) s_seq))

; --- copy, of what test_traps does not: a pmap and a pset in a list ---
; Lists are deep-copied for nested :list types, but reference :pmap, :pset, :array and other types
(defq inner_list (list 1 2))
(defq inner_pmap (pmap 'a 10))
(defq inner_pset (pset 'x 'y))
(defq inner_arr (array 100 200))
(defq list_orig (list inner_list inner_pmap inner_pset inner_arr))
(defq list_copy (copy list_orig))

(assert-true "list copy equal" (equal? list_orig list_copy))
(assert-true "nested pmap in copy is referenced" (eql (weak-ref inner_pmap) (weak-ref (second list_copy))))
(assert-true "nested pset in copy is referenced" (eql (weak-ref inner_pset) (weak-ref (third list_copy))))

; --- slices ---
(assert-list-eq "slices empty" (list) (slices '()))
(assert-list-eq "slices contiguous" (list (list 1 4)) (slices '(1 2 3)))
(assert-list-eq "slices non-contiguous" (list (list 1 4) (list 5 7) (list 9 10)) (slices '(1 2 3 5 6 9)))
(assert-list-eq "slices unsorted input" (list (list 1 4) (list 6 8)) (slices '(3 2 1 6 7)))
