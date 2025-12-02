;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Testing Framework - Example Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file demonstrates how to use the FiveAM testing framework in ChrysaLisp
;;
;; Copyright (c) 2025
;; Licensed under BSD 3-Clause License
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "../test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test basic-arithmetic-test
	:description "Test basic arithmetic operations"
	(is-eq 4 (+ 2 2) "Addition works")
	(is-eq 6 (* 2 3) "Multiplication works")
	(is-eq 1 (- 3 2) "Subtraction works")
	(is-eq 2 (/ 6 3) "Division works"))

(def-test comparison-test
	:description "Test comparison operations"
	(is-true (< 1 2) "Less than comparison")
	(is-true (> 5 3) "Greater than comparison")
	(is-true (<= 2 2) "Less than or equal")
	(is-true (>= 5 5) "Greater than or equal")
	(is-false (< 5 2) "False less than"))

(def-test equality-test
	:description "Test equality operations"
	(is-eq 5 5 "Numbers are equal")
	(is-eq :hello :hello "Symbols are equal")
	(is-equal (list 1 2 3) (list 1 2 3) "Lists are equal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :string-tests :description "String manipulation tests")

(in-suite :string-tests)

(def-test string-concatenation
	:description "Test string concatenation"
	(is-equal "Hello World" (cat "Hello" " " "World") "Cat concatenates strings")
	(is-equal "123" (cat "1" "2" "3") "Cat works with multiple strings"))

(def-test string-length
	:description "Test string length"
	(is-eq 5 (length "Hello") "String length is correct")
	(is-eq 0 (length "") "Empty string has zero length"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :list-tests :description "List operation tests")

(in-suite :list-tests)

(def-test list-creation
	:description "Test list creation and basic operations"
	(is-equal (list 1 2 3) '(1 2 3) "List literal works")
	(is-eq 3 (length (list 1 2 3)) "List length is correct"))

(def-test list-manipulation
	:description "Test list manipulation functions"
	(is-eq 1 (first (list 1 2 3)) "First element")
	(is-equal (list 2 3) (rest (list 1 2 3)) "Rest of list")
	(is-equal (list 0 1 2) (push (list 1 2) 0) "Push to list"))

(def-test list-mapping
	:description "Test list mapping operations"
	(is-equal (list 2 4 6) (map (lambda (x) (* x 2)) (list 1 2 3))
		"Map doubles each element")
	(is-equal (list 2 4) (filter (lambda (x) (= 0 (% x 2))) (list 1 2 3 4))
		"Filter selects even numbers"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Handling Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :error-tests :description "Error handling tests")

(in-suite :error-tests)

(def-test finishes-successfully
	:description "Test code that finishes normally"
	(finishes
		(+ 1 1)
		(* 2 2)))

(def-test explicit-pass-fail
	:description "Test explicit pass and fail"
	(pass "Explicitly passing this check")
	;; Note: Don't use (fail) in real tests unless you want the test to fail!
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :dependency-tests :description "Test dependency examples")

(in-suite :dependency-tests)

(def-test prerequisite-test
	:description "This test must pass for dependent tests to run"
	(is-true :t "Prerequisite always passes"))

(def-test dependent-on-prerequisite
	:description "This test depends on prerequisite-test"
	:depends-on 'prerequisite-test
	(is-true :t "Dependent test runs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run All Example Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-examples ()
	; (run-examples)
	; Run all example tests and display results
	(print "FiveAM Example Tests" (ascii-char 10))
	(print "====================" (ascii-char 10))

	;; Run basic tests
	(print (ascii-char 10) "Running basic tests...")
	(run! 'basic-arithmetic-test)
	(run! 'comparison-test)
	(run! 'equality-test)

	;; Run string suite
	(print (ascii-char 10) "Running string tests...")
	(run! :string-tests)

	;; Run list suite
	(print (ascii-char 10) "Running list tests...")
	(run! :list-tests)

	;; Run error tests
	(print (ascii-char 10) "Running error handling tests...")
	(run! :error-tests)

	;; Run dependency tests
	(print (ascii-char 10) "Running dependency tests...")
	(run! :dependency-tests)

	;; Final summary
	(print (ascii-char 10) "====================" (ascii-char 10))
	(print "All example tests completed!" (ascii-char 10)))

;; Uncomment to run all examples when loading this file:
;; (run-examples)
