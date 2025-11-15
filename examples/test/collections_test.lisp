;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collections and Sequences Test Examples
; Demonstrates testing with lists and sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; List Operations
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "List Operations"
	(it "should create lists correctly"
		(defq mylist (list 1 2 3 4 5))
		(should-not-be-nil mylist)
		(should-equal (length mylist) 5))

	(it "should access list elements"
		(defq mylist (list 10 20 30))
		(should-equal (first mylist) 10)
		(should-equal (elem-get mylist 1) 20)
		(should-equal (elem-get mylist 2) 30))

	(it "should push elements to lists"
		(defq mylist (list 1 2))
		(push mylist 3)
		(should-equal (length mylist) 3)
		(should-equal (elem-get mylist 2) 3))

	(it "should pop elements from lists"
		(defq mylist (list 1 2 3))
		(defq popped (pop mylist))
		(should-equal popped 3)
		(should-equal (length mylist) 2))

	(it "should handle empty lists"
		(defq empty_list (list))
		(should-be-empty empty_list)
		(should-equal (length empty_list) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Sequence Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Sequence Functions"
	(it "should map over sequences"
		(defq nums (list 1 2 3 4 5))
		(defq doubled (map (lambda (x) (* x 2)) nums))
		(should-equal (elem-get doubled 0) 2)
		(should-equal (elem-get doubled 1) 4)
		(should-equal (elem-get doubled 4) 10))

	(it "should filter sequences"
		(defq nums (list 1 2 3 4 5 6))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-equal (length evens) 3)
		(should-contain evens 2)
		(should-contain evens 4)
		(should-contain evens 6))

	(it "should reduce sequences"
		(defq nums (list 1 2 3 4 5))
		(defq sum (reduce (lambda (acc x) (+ acc x)) nums 0))
		(should-equal sum 15))

	(it "should find elements with some"
		(defq nums (list 1 2 3 4 5))
		(defq found (some (lambda (x) (if (> x 3) x)) nums))
		(should-not-be-nil found)
		(should-equal found 4)))

;;;;;;;;;;;;;;;;;;;;;;;;
; String Operations
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "String Operations"
	(it "should concatenate strings"
		(defq result (cat "Hello" " " "World"))
		(should-equal result "Hello World"))

	(it "should find substrings"
		(should-not-be-nil (find "world" "hello world"))
		(should-be-nil (find "xyz" "hello world")))

	(it "should check string prefixes"
		(should-be-true (starts-with "hello" "hello world"))
		(should-be-false (starts-with "world" "hello world")))

	(it "should check string suffixes"
		(should-be-true (ends-with "world" "hello world"))
		(should-be-false (ends-with "hello" "hello world")))

	(it "should get string length"
		(should-equal (length "hello") 5)
		(should-equal (length "") 0)
		(should-equal (length "ChrysaLisp") 10)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Collection Assertions
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Collection Assertions"
	(it "should check if collection contains element"
		(defq mylist (list 1 2 3 4 5))
		(should-contain mylist 3)
		(should-not-contain mylist 10))

	(it "should check if collection is empty"
		(defq empty_list (list))
		(defq non-empty_list (list 1 2 3))
		(should-be-empty empty_list)
		(should-not-be-empty non-empty_list))

	(it "should handle string collections"
		(defq mystring "hello world")
		(should-contain mystring "world")
		(should-not-contain mystring "xyz")))
