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

(defmacro test-set-variety (name constructor)
	`(progn
		(report-header (cat "Set: " ,name))
		(defq s (,constructor))

		; insert / find
		(assert-eq (cat ,name " size 0") 0 (. s :size))
		(. s :insert "A")
		(assert-eq (cat ,name " size 1") 1 (. s :size))
		(assert-eq (cat ,name " find existing") "A" (. s :find "A"))
		(assert-eq (cat ,name " find missing") :nil (. s :find "B"))

		; inserted
		(assert-true (cat ,name " inserted new") (. s :inserted "B"))
		(assert-true (cat ,name " inserted existing") (not (. s :inserted "A")))

		; intern
		(assert-eq (cat ,name " intern existing") "A" (. s :intern "A"))
		(assert-eq (cat ,name " size still 2") 2 (. s :size))
		(assert-eq (cat ,name " intern new") "C" (. s :intern "C"))
		(assert-eq (cat ,name " size 3 after intern") 3 (. s :size))

		; each
		(defq items (list))
		(. s :each (lambda (i) (push items i)))
		(assert-eq (cat ,name " each count") 3 (length items))

		; copy / isolation
		(defq s2 (. s :copy))
		(. s2 :insert "D")
		(assert-eq (cat ,name " copy isolation") :nil (. s :find "D"))

		; deep_copy
		(defq complex_item (list 1 2))
		(. s :insert complex_item)
		(defq s_dc (. s :deep_copy))
		(defq found_item (. s_dc :find complex_item))
		(assert-true (cat ,name " deep_copy equal") (equal? complex_item found_item))
		(assert-true (cat ,name " deep_copy not eql") (not (eql (weak-ref complex_item) (weak-ref found_item))))

		; set ops
		(defq sa (,constructor) sb (,constructor))
		(. sa :insert "1") (. sa :insert "2")
		(. sb :insert "2") (. sb :insert "3")

		(defq u (. (. sa :copy) :union sb))
		(assert-true (cat ,name " union 1") (. u :find "1"))
		(assert-true (cat ,name " union 3") (. u :find "3"))

		(defq i (. (. sa :copy) :intersect sb))
		(assert-eq (cat ,name " intersect") "2" (. i :find "2"))
		(assert-eq (cat ,name " intersect miss") :nil (. i :find "1"))

		(defq d (. (. sa :copy) :difference sb))
		(assert-eq (cat ,name " diff") "1" (. d :find "1"))
		(assert-eq (cat ,name " diff miss") :nil (. d :find "2"))

		(defq ni (. (. sa :copy) :not_intersect sb))
		(assert-true (cat ,name " ni 1") (. ni :find "1"))
		(assert-true (cat ,name " ni 3") (. ni :find "3"))
		(assert-eq (cat ,name " ni miss") :nil (. ni :find "2"))

		; empty? / empty
		(assert-true (cat ,name " not empty?") (not (. s :empty?)))
		(assert-true (cat ,name " size before empty") (> (. s :size) 0))
		(. s :empty)
		(assert-true (cat ,name " is empty?") (. s :empty?))
		(assert-eq (cat ,name " size after empty") 0 (. s :size))

		; move
		(. s :insert "X")
		(defq s3 (. s :move))
		(assert-eq (cat ,name " move find") "X" (. s3 :find "X"))
		(assert-true (cat ,name " move empty") (. s :empty?))

		; erase
		(. s3 :erase "X")
		(assert-eq (cat ,name " erase") :nil (. s3 :find "X"))

		; resize
		(. s3 :insert "Y")
		(. s3 :resize 23)
		(assert-eq (cat ,name " resize find") "Y" (. s3 :find "Y"))
	))

(import "lib/collections/fset.inc")
(import "lib/collections/lset.inc")

(test-set-variety "Fset" (# (Fset 11)))
(test-set-variety "Xset" (# (Xset 11)))
(test-set-variety "Lset" (# (Lset)))

(report-header "Custom Xset")
(defun my-hash (s) (hash (to-lower s)))
(defun my-cmp (a b) (eql (to-lower a) (to-lower b)))

(defq cxs (Xset 11 my-cmp my-hash))
(. cxs :insert "World")
(assert-eq "custom Xset find" "World" (. cxs :find "WORLD"))

(report-header "Lset tree support")
(import "lib/collections/tree.inc")

(defq ls (Lset))
(. ls :insert "A")
(. ls :insert "B")

(defq ms (memory-stream))
(tree-save ms ls)
(stream-seek ms 0 0)
(defq ls2 (tree-load ms))

(assert-true "Lset tree load A" (. ls2 :find "A"))
(assert-true "Lset tree load B" (. ls2 :find "B"))
(assert-eq "Lset tree size" 2 (. ls2 :size))

