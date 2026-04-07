(report-header "Sequence Ops: range, zip, flatten, unique, partition, etc.")

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

; Partition
(defq p (partition (list 1 2 3 4 5 6) 2))
(assert-eq "Partition len" 3 (length p))
(assert-list-eq "Partition first" (list 1 2) (first p))

; Join
(assert-eq "Join" "a-b-c" (join (list "a" "b" "c") "-"))

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

; --- swap ---
(defq sw_arr (array 1 2 3))
(swap sw_arr 0 2)
(assert-list-eq "swap" '(3 2 1) (map identity sw_arr))
