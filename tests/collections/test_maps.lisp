(report-header "Maps: Fmap, Emap, Lmap, Xmap")

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

(report-header "Custom Xmap")
; Custom case-insensitive string hash/cmp
(defun my-hash (s) (hash (to-lower s)))
(defun my-cmp (a b) (eql (to-lower a) (to-lower b)))

(defq cxm (Xmap 11 my-cmp my-hash))
(. cxm :insert "Hello" 123)
(assert-eq "custom Xmap find" 123 (. cxm :find "HELLO"))
