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

; --- copy (Array & List) ---
; Non-list/pmap/pset types (like Array) are referenced (same pointer)
(defq arr_orig (array 1 2 3))
(defq arr_copy (copy arr_orig))
(assert-true "array copy is same reference" (eql (weak-ref arr_orig) (weak-ref arr_copy)))

; Lists are deep-copied for nested :list types, but reference :pmap, :pset, :array and other types
(defq inner_list (list 1 2))
(defq inner_pmap (pmap 'a 10))
(defq inner_pset (pset 'x 'y))
(defq inner_arr (array 100 200))
(defq list_orig (list inner_list inner_pmap inner_pset inner_arr))
(defq list_copy (copy list_orig))

(assert-true "list copy equal" (equal? list_orig list_copy))
(assert-true "list copy independent instance" (not (eql (weak-ref list_orig) (weak-ref list_copy))))
(assert-true "nested list in copy is deep-copied" (not (eql (weak-ref inner_list) (weak-ref (first list_copy)))))
(assert-true "nested pmap in copy is referenced" (eql (weak-ref inner_pmap) (weak-ref (second list_copy))))
(assert-true "nested pset in copy is referenced" (eql (weak-ref inner_pset) (weak-ref (third list_copy))))
(assert-true "nested array in copy is referenced" (eql (weak-ref inner_arr) (weak-ref (elem-get list_copy 3))))

(push (first list_copy) 3)
(assert-eq "nested list orig length unchanged" 2 (length inner_list))
(assert-eq "nested list copy length updated" 3 (length (first list_copy)))

; --- swap ---
(defq sw_arr (array 1 2 3))
(swap sw_arr 0 2)
(assert-list-eq "swap" '(3 2 1) (map identity sw_arr))

; --- slices ---
(assert-list-eq "slices empty" (list) (slices '()))
(assert-list-eq "slices contiguous" (list (list 1 4)) (slices '(1 2 3)))
(assert-list-eq "slices non-contiguous" (list (list 1 4) (list 5 7) (list 9 10)) (slices '(1 2 3 5 6 9)))
(assert-list-eq "slices unsorted input" (list (list 1 4) (list 6 8)) (slices '(3 2 1 6 7)))
