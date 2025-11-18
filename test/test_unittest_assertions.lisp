#!/usr/bin/env lsp

;; Unit tests demonstrating all assertion types

(import "lib/test/unittest.inc")

(deftest-suite "Assertion Framework Tests")

(defun main ()
	(run-test-suite
		; Basic assertions
		(deftest "Basic True/False"
			(assert-true :t)
			(assert-false :nil))

		(deftest "Equality"
			(assert-eq 5 5)
			(assert-neq 5 10))

		(deftest "Nil checks"
			(assert-nil :nil)
			(assert-not-nil "not nil"))

		; Numeric comparisons
		(deftest "Numeric Comparisons"
			(assert-greater 10 5)
			(assert-less 5 10)
			(assert-greater-eq 10 10)
			(assert-greater-eq 10 5)
			(assert-less-eq 5 5)
			(assert-less-eq 5 10))

		(deftest "Approximate Equality"
			(assert-close 3.14159 3.14 0.01)
			(assert-close 100 99 2))

		; Collection assertions
		(deftest "Collection Contains"
			(assert-contains '(1 2 3 4 5) 3)
			(assert-not-contains '(1 2 3 4 5) 10))

		(deftest "Collection Empty"
			(assert-empty '())
			(assert-empty :nil)
			(assert-not-empty '(1 2 3)))

		(deftest "Collection Length"
			(assert-length '(1 2 3 4 5) 5)
			(assert-length "hello" 5))

		; String assertions
		(deftest "String Prefix/Suffix"
			(assert-starts-with "hello world" "hello")
			(assert-ends-with "hello world" "world"))

		(deftest "String Contains"
			(assert-contains-str "hello world" "lo wo"))

		; Type assertions
		(deftest "Type Checking"
			(assert-type "string" 'str)
			(assert-type 42 'num)
			(assert-type '(1 2 3) 'list)
			(assert-type (env) 'env))

		; Edge cases
		(deftest "Edge Cases"
			(assert-eq 0 0)
			(assert-neq 0 1)
			(assert-length "" 0)
			(assert-empty ""))

		; Complex assertions
		(deftest "Complex Values"
			(defq list1 '(1 2 3 4 5))
			(assert-contains list1 3)
			(assert-length list1 5)
			(assert-not-empty list1))
	))
