;;;;;;;;;;;;;;;;;;;;;;
; tests/test_sets.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "Sets: union, difference, intersect")

(defq s1 (Fset 5))
(. s1 :insert "A")
(. s1 :insert "B")
(. s1 :insert "C")

(defq s2 (Fset 5))
(. s2 :insert "B")
(. s2 :insert "C")
(. s2 :insert "D")

; --- Union ---
(defq u (. (. s1 :copy) :union s2))
(assert-true "union has A" (. u :find "A"))
(assert-true "union has B" (. u :find "B"))
(assert-true "union has C" (. u :find "C"))
(assert-true "union has D" (. u :find "D"))

; --- Difference ---
(defq d (. (. s1 :copy) :difference s2))
(assert-true "diff has A" (. d :find "A"))
(assert-true "diff no B"  (not (. d :find "B")))
(assert-true "diff no C"  (not (. d :find "C")))

; --- Intersect ---
(defq i (. (. s1 :copy) :intersect s2))
(assert-true "intersect no A" (not (. i :find "A")))
(assert-true "intersect has B" (. i :find "B"))
(assert-true "intersect has C" (. i :find "C"))
(assert-true "intersect no D" (not (. i :find "D")))

; --- Not Intersect (Symmetric Difference) ---
(defq ni (. (. s1 :copy) :not_intersect s2))
(assert-true "not_intersect has A" (. ni :find "A"))
(assert-true "not_intersect no B"  (not (. ni :find "B")))
(assert-true "not_intersect no C"  (not (. ni :find "C")))
(assert-true "not_intersect has D" (. ni :find "D"))

; --- Empty? ---
(assert-true "not empty" (not (. s1 :empty?)))
(assert-true "is empty"  (. (Fset 5) :empty?))

(report-header "Collections: Single Bucket (High Collision)")

; Fset with 1 bucket
(defq fs1 (Fset 1))
(. fs1 :insert "X")
(. fs1 :insert "Y")
(. fs1 :insert "Z")
(assert-eq "Fset 1-bucket find X" "X" (. fs1 :find "X"))
(assert-eq "Fset 1-bucket find Y" "Y" (. fs1 :find "Y"))
(assert-eq "Fset 1-bucket find Z" "Z" (. fs1 :find "Z"))
(. fs1 :erase "Y")
(assert-eq "Fset 1-bucket erase Y" :nil (. fs1 :find "Y"))
(assert-eq "Fset 1-bucket still X" "X" (. fs1 :find "X"))

; Fmap with 1 bucket
(defq fm1 (Fmap 1))
(. fm1 :insert 'a 1)
(. fm1 :insert 'b 2)
(. fm1 :insert 'c 3)
(assert-eq "Fmap 1-bucket find a" 1 (. fm1 :find 'a))
(assert-eq "Fmap 1-bucket find b" 2 (. fm1 :find 'b))
(assert-eq "Fmap 1-bucket find c" 3 (. fm1 :find 'c))
(. fm1 :erase 'b)
(assert-eq "Fmap 1-bucket erase b" :nil (. fm1 :find 'b))
(assert-eq "Fmap 1-bucket still a" 1 (. fm1 :find 'a))