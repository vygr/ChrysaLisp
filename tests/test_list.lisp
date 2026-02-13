(report-header "Lists & Sequences")

(defq l (list 10 20 30 40))

; Accessors
(assert-eq "Length" 4 (length l))
(assert-eq "First" 10 (first l))
(assert-eq "Second" 20 (second l))
(assert-eq "Last" 40 (last l))
(assert-eq "Elem-get" 30 (elem-get l 2))

; Mutation
(push l 50)
(assert-eq "Push/Length" 5 (length l))
(assert-eq "Pop" 50 (pop l))

; Higher Order
; CORRECTED: Wrapped macro 'inc' in lambda shortcut syntax
(defq m (map (# (inc %0)) (list 1 2 3)))
(assert-eq "Map first" 2 (first m))
(assert-eq "Map last" 4 (last m))

(defq f (filter (lambda (x) (> x 5)) (list 3 6 2 8)))
(assert-eq "Filter len" 2 (length f))
(assert-eq "Filter val" 6 (first f))

; Note: + is a function (via FFI), so (const +) works
(defq r (reduce (const +) (list 1 2 3 4) 0))
(assert-eq "Reduce" 10 r)

(assert-eq "Reverse" 3 (first (reverse (list 1 2 3))))

(defq c_arr (array 1 2 3))
(cap 10 c_arr)
(assert-eq "cap" 3 (length c_arr))

(clear c_arr)
(assert-eq "clear" 0 (length c_arr))

(defq es_arr (array 10 20 30))
(elem-set es_arr 1 99)
(assert-eq "elem-set" 99 (elem-get es_arr 1))

(defq l1_m (list 1 2) l2_m (list 3 4))
(merge l1_m l2_m)
(assert-list-eq "merge" '(1 2 3 4) l1_m)
