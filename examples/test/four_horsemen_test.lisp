;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Four Horsemen Test Examples
; Demonstrates testing the core primitives: map, filter, reduce, some
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; The First Horseman: map
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "The First Horseman: map"
	(it "should transform all elements"
		(defq nums (list 1 2 3 4 5))
		(defq doubled (map (lambda (x) (* x 2)) nums))
		(should-equal (length doubled) 5)
		(should-equal (elem-get doubled 0) 2)
		(should-equal (elem-get doubled 4) 10))

	(it "should work with strings"
		(defq words (list "hello" "world"))
		(defq lengths (map (lambda (s) (length s)) words))
		(should-equal (elem-get lengths 0) 5)
		(should-equal (elem-get lengths 1) 5))

	(it "should handle empty sequences"
		(defq empty (list))
		(defq result (map (lambda (x) x) empty))
		(should-be-empty result))

	(it "should compose transformations"
		(defq nums (list 1 2 3))
		(defq result (map (lambda (x) (+ (* x 2) 1)) nums))
		(should-equal (elem-get result 0) 3)   ; (1*2)+1 = 3
		(should-equal (elem-get result 1) 5)   ; (2*2)+1 = 5
		(should-equal (elem-get result 2) 7))) ; (3*2)+1 = 7

;;;;;;;;;;;;;;;;;;;;;;;;
; The Second Horseman: filter
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "The Second Horseman: filter"
	(it "should keep only matching elements"
		(defq nums (list 1 2 3 4 5 6))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-equal (length evens) 3)
		(should-contain evens 2)
		(should-not-contain evens 1))

	(it "should work with predicates"
		(defq nums (list -2 -1 0 1 2))
		(defq positive (filter (lambda (x) (> x 0)) nums))
		(should-equal (length positive) 2)
		(should-contain positive 1)
		(should-contain positive 2))

	(it "should return empty when nothing matches"
		(defq nums (list 1 3 5 7 9))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-be-empty evens))

	(it "should return all when everything matches"
		(defq nums (list 2 4 6 8))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-equal (length evens) (length nums))))

;;;;;;;;;;;;;;;;;;;;;;;;
; The Third Horseman: reduce
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "The Third Horseman: reduce"
	(it "should sum numbers"
		(defq nums (list 1 2 3 4 5))
		(defq sum (reduce (lambda (acc x) (+ acc x)) nums 0))
		(should-equal sum 15))

	(it "should multiply numbers"
		(defq nums (list 1 2 3 4 5))
		(defq product (reduce (lambda (acc x) (* acc x)) nums 1))
		(should-equal product 120))

	(it "should concatenate strings"
		(defq words (list "Chrysa" "Lisp"))
		(defq result (reduce (lambda (acc s) (cat acc s)) words ""))
		(should-equal result "ChrysaLisp"))

	(it "should count elements"
		(defq nums (list 1 2 3 4 5))
		(defq count (reduce (lambda (acc x) (inc acc)) nums 0))
		(should-equal count 5))

	(it "should find maximum"
		(defq nums (list 3 7 2 9 4))
		(defq max-val (reduce (lambda (acc x) (if (> x acc) x acc)) nums 0))
		(should-equal max-val 9))

	(it "should find minimum"
		(defq nums (list 3 7 2 9 4))
		(defq min-val (reduce (lambda (acc x) (if (< x acc) x acc)) nums 999))
		(should-equal min-val 2)))

;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Horseman: some
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "The Fourth Horseman: some"
	(it "should find first matching element"
		(defq nums (list 1 2 3 4 5))
		(defq found (some (lambda (x) (if (> x 3) x)) nums))
		(should-equal found 4))

	(it "should return nil when nothing matches"
		(defq nums (list 1 2 3))
		(defq found (some (lambda (x) (if (> x 10) x)) nums))
		(should-be-nil found))

	(it "should short-circuit on first match"
		(defq nums (list 1 2 3 4 5))
		; Find first even number
		(defq found (some (lambda (x) (if (= 0 (% x 2)) x)) nums))
		(should-equal found 2))

	(it "should work as existence check"
		(defq nums (list 1 3 5 7 9))
		(defq has-even (some (lambda (x) (= 0 (% x 2))) nums))
		(should-be-nil has-even)

		(defq nums2 (list 1 3 5 8 9))
		(defq has-even2 (some (lambda (x) (= 0 (% x 2))) nums2))
		(should-be-true has-even2)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Combining The Horsemen
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Combining the Four Horsemen"
	(it "should chain map and filter"
		; Double all numbers, then keep only those > 5
		(defq nums (list 1 2 3 4 5))
		(defq doubled (map (lambda (x) (* x 2)) nums))
		(defq filtered (filter (lambda (x) (> x 5)) doubled))
		(should-equal (length filtered) 3)
		(should-contain filtered 6)
		(should-contain filtered 8)
		(should-contain filtered 10))

	(it "should use filter then reduce"
		; Get evens, then sum them
		(defq nums (list 1 2 3 4 5 6))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(defq sum (reduce (lambda (acc x) (+ acc x)) evens 0))
		(should-equal sum 12)) ; 2+4+6 = 12

	(it "should map then reduce"
		; Square all numbers, then sum
		(defq nums (list 1 2 3 4))
		(defq squared (map (lambda (x) (* x x)) nums))
		(defq sum (reduce (lambda (acc x) (+ acc x)) squared 0))
		(should-equal sum 30)) ; 1+4+9+16 = 30

	(it "should use some to check if any satisfy condition"
		(defq nums (list 1 2 3 4 5))
		; Check if any number is greater than 3
		(defq has-large (some (lambda (x) (> x 3)) nums))
		(should-be-true has-large)

		; Check if any number is greater than 10
		(defq has-huge (some (lambda (x) (> x 10)) nums))
		(should-be-nil has-huge)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Performance and Efficiency
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Efficiency of the Horsemen"
	(it "should handle large sequences with map"
		(defq large (map (lambda (x) x) (range 0 1000 1)))
		(should-equal (length large) 1000))

	(it "should handle large sequences with filter"
		(defq nums (range 0 100 1))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-equal (length evens) 50))

	(it "should handle large sequences with reduce"
		(defq nums (range 1 101 1))
		(defq sum (reduce (lambda (acc x) (+ acc x)) nums 0))
		(should-equal sum 5050))) ; Sum of 1 to 100
