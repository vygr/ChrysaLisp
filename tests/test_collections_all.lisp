(report-header "Comprehensive Collections Test")

(defmacro test-map-variety (name constructor)
	`(progn
		(report-header (cat "Map: " ,name))
		(defq m (,constructor))

		; insert / find
		(. m :insert 'a 1)
		(assert-eq (cat ,name " find existing") 1 (. m :find 'a))
		(assert-eq (cat ,name " find missing") :nil (. m :find 'b))

		; update
		(. m :update 'a (lambda (v) (+ v 10)))
		(assert-eq (cat ,name " update existing") 11 (. m :find 'a))
		(. m :update 'b (lambda (v) (if v v 20)))
		(assert-eq (cat ,name " update missing") 20 (. m :find 'b))

		; memoize
		(defq call_count 0)
		(defq slow_gen (lambda () (++ call_count) 100))
		(assert-eq (cat ,name " memoize 1") 100 (. m :memoize 'c slow_gen))
		(assert-eq (cat ,name " memoize 2") 100 (. m :memoize 'c slow_gen))
		(assert-eq (cat ,name " memoize count") 1 call_count)

		; each
		(defq keys (list) vals (list))
		(. m :each (lambda (k v) (push keys k) (push vals v)))
		(assert-eq (cat ,name " each count") 3 (length keys))

		; copy
		(defq m2 (. m :copy))
		(assert-eq (cat ,name " copy find") 11 (. m2 :find 'a))
		(. m2 :insert 'a 99)
		(assert-eq (cat ,name " copy isolation") 11 (. m :find 'a))

		; deep_copy
		(defq complex_val (list 1 2))
		(. m :insert 'd complex_val)
		(defq m3 (. m :deep_copy))
		(defq found_val (. m3 :find 'd))
		(assert-true (cat ,name " deep_copy equal") (equal? complex_val found_val))
		(assert-true (cat ,name " deep_copy not eql") (not (eql (weak-ref complex_val) (weak-ref found_val))))

		; empty? / empty
		(assert-true (cat ,name " not empty?") (not (. m :empty?)))
		(. m :empty)
		(assert-true (cat ,name " is empty?") (. m :empty?))

		; move
		(. m :insert 'x 500)
		(defq m4 (. m :move))
		(assert-eq (cat ,name " move find") 500 (. m4 :find 'x))
		(assert-true (cat ,name " move empty") (. m :empty?))

		; erase
		(. m4 :erase 'x)
		(assert-eq (cat ,name " erase") :nil (. m4 :find 'x))

		; resize
		(. m4 :insert 'y 600)
		(. m4 :resize 23)
		(assert-eq (cat ,name " resize find") 600 (. m4 :find 'y))
	))

(test-map-variety "Fmap" (# (Fmap 11)))
(test-map-variety "Emap" (# (Emap 11)))
(test-map-variety "Lmap" (# (Lmap)))
(test-map-variety "Xmap" (# (Xmap 11)))

(defmacro test-set-variety (name constructor)
	`(progn
		(report-header (cat "Set: " ,name))
		(defq s (,constructor))

		; insert / find
		(. s :insert "A")
		(assert-eq (cat ,name " find existing") "A" (. s :find "A"))
		(assert-eq (cat ,name " find missing") :nil (. s :find "B"))

		; inserted
		(if (eql ,name "Fset")
			(progn
				(assert-true (cat ,name " inserted new") (. s :inserted "B"))
				(assert-true (cat ,name " inserted existing") (not (. s :inserted "A"))))
			(progn
				(assert-true (cat ,name " inserted new") (not (. s :inserted "B")))
				(assert-true (cat ,name " inserted existing") (. s :inserted "A"))))

		; intern
		(assert-eq (cat ,name " intern existing") "A" (. s :intern "A"))
		(assert-eq (cat ,name " intern new") "C" (. s :intern "C"))

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
		(. s :empty)
		(assert-true (cat ,name " is empty?") (. s :empty?))

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

(test-set-variety "Fset" (# (Fset 11)))
(test-set-variety "Xset" (# (Xset 11)))

; --- GATHER / SCATTER ---
(report-header "Gather / Scatter")
(defq ms (Fmap 1))
(scatter ms 'k1 10 'k2 20 'k3 30)
(assert-eq "scatter find" 20 (. ms :find 'k2))
(defq gathered (gather ms 'k3 'k1))
(assert-list-eq "gather" '(30 10) gathered)

; --- MEMOIZE MACRO ---
(report-header "Memoize Macro")
(defq mem_count 0)
(defun test-memo (x)
	(memoize x (progn (++ mem_count) (* x 2)) 11))

(assert-eq "memoize 1st" 20 (test-memo 10))
(assert-eq "memoize 2nd" 20 (test-memo 10))
(assert-eq "memoize count" 1 mem_count)
(assert-eq "memoize other" 40 (test-memo 20))
(assert-eq "memoize count 2" 2 mem_count)

; --- CUSTOM XMAP / XSET ---
(report-header "Custom Xmap / Xset")
; Custom case-insensitive string hash/cmp
(defun my-hash (s) (hash (to-lower s)))
(defun my-cmp (a b) (eql (to-lower a) (to-lower b)))

(defq cxm (Xmap 11 my-cmp my-hash))
(. cxm :insert "Hello" 123)
(assert-eq "custom Xmap find" 123 (. cxm :find "HELLO"))

(defq cxs (Xset 11 my-cmp my-hash))
(. cxs :insert "World")
(assert-eq "custom Xset find" "World" (. cxs :find "WORLD"))

; --- TREE SAVE/LOAD ---
(report-header "Tree Save/Load")

(defq tree_data (Fmap 1))
(. tree_data :insert "name" "test")
(. tree_data :insert "values" (array 1 2 3))

(defq ms_tree (memory-stream))
(tree-save ms_tree tree_data)
(stream-seek ms_tree 0 0)
(defq loaded_tree (tree-load ms_tree))

(assert-eq "Tree-load name" "test" (. loaded_tree :find "name"))
(defq loaded_vals (. loaded_tree :find "values"))
(assert-true "Tree-load array" (array? loaded_vals))
(assert-eq "Tree-load array len" 3 (length loaded_vals))
(assert-eq "Tree-load array val" 2 (elem-get loaded_vals 1))
