(report-header "Maps: Fmap, Emap, Xmap, Lmap")

(defmacro test-map-variety (name constructor)
	`(progn
		(report-header (cat "Map: " ,name))
		(defq m (,constructor))

		; insert / find
		(. m :insert 'a 1)
		(assert-eq (cat ,name " find existing") 1 (. m :find 'a))
		(assert-eq (cat ,name " find missing") :nil (. m :find 'b))

		; update
		(assert-eq (cat ,name " update existing return") 11 (. m :update 'a (lambda (v) (+ v 10))))
		(assert-eq (cat ,name " update existing find") 11 (. m :find 'a))
		(assert-eq (cat ,name " update missing return") 20 (. m :update 'b (lambda (v) (if v v 20))))
		(assert-eq (cat ,name " update missing find") 20 (. m :find 'b))

		; memoize
		(defq call_count 0)
		(defq slow_gen (lambda () (++ call_count) 100))
		(assert-eq (cat ,name " memoize 1 return") 100 (. m :memoize 'c slow_gen))
		(assert-eq (cat ,name " memoize 1 find") 100 (. m :find 'c))
		(assert-eq (cat ,name " memoize 2 return") 100 (. m :memoize 'c slow_gen))
		(assert-eq (cat ,name " memoize count") 1 call_count)

		; each
		(defq keys (list) vals (list))
		(. m :each (lambda (k v) (push keys k) (push vals v)))
		(assert-eq (cat ,name " each count") 3 (length keys))

		; copy (shallow copy verification)
		(defq shallow_list (list 1 2))
		(defq shallow_arr (array 10 20))
		(. m :insert 's_list shallow_list)
		(. m :insert 's_arr shallow_arr)
		(defq m2 (. m :copy))
		(assert-eq (cat ,name " copy find") 11 (. m2 :find 'a))
		(. m2 :insert 'a 99)
		(assert-eq (cat ,name " copy isolation") 11 (. m :find 'a))
		(assert-true (cat ,name " copy shallow list ref") (eql (weak-ref shallow_list) (weak-ref (. m2 :find 's_list))))
		(assert-true (cat ,name " copy shallow arr ref") (eql (weak-ref shallow_arr) (weak-ref (. m2 :find 's_arr))))

		; deep_copy verification
		(defq complex_val (list 1 2))
		(defq arr_val (array 3 4))
		(. m :insert 'd complex_val)
		(. m :insert 'e arr_val)
		(defq m3 (. m :deep_copy))
		(defq found_val (. m3 :find 'd))
		(defq found_arr (. m3 :find 'e))
		(assert-true (cat ,name " deep_copy equal list") (equal? complex_val found_val))
		(assert-true (cat ,name " deep_copy list is independent") (not (eql (weak-ref complex_val) (weak-ref found_val))))
		(assert-true (cat ,name " deep_copy array is referenced") (eql (weak-ref arr_val) (weak-ref found_arr)))

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
(test-map-variety "Xmap" (# (Xmap 11)))
(test-map-variety "Lmap" (# (Lmap)))

(report-header "Custom Xmap")
; Custom case-insensitive string hash/cmp
(redefun my-hash (s) (hash (to-lower s)))
(redefun my-cmp (a b) (eql (to-lower a) (to-lower b)))

(defq cxm (Xmap 11 my-cmp my-hash))
(. cxm :insert "Hello" 123)
(assert-eq "custom Xmap find" 123 (. cxm :find "HELLO"))

(report-header "Map: Lmap & Fmap nil memoization")
(each (lambda (name constructor)
		(defq m (constructor))
		(defq nil_count 0)
		(defq nil_gen (lambda () (++ nil_count) :nil))
		(assert-eq (cat name " memoize nil return") :nil (. m :memoize 'd nil_gen))
		(assert-eq (cat name " memoize nil 2nd return") :nil (. m :memoize 'd nil_gen))
		(assert-eq (cat name " memoize nil count") 1 nil_count))
	'("Lmap" "Fmap") (list (# (Lmap)) (# (Fmap 11))))
