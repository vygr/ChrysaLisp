;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Test Examples
; Demonstrates fundamental testing patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; Arithmetic Operations
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Basic Arithmetic"
	(it "should add two numbers correctly"
		(should-equal (+ 2 3) 5)
		(should-equal (+ 10 20) 30)
		(should-equal (+ -5 5) 0))

	(it "should subtract two numbers correctly"
		(should-equal (- 10 5) 5)
		(should-equal (- 0 5) -5)
		(should-equal (- -5 -10) 5))

	(it "should multiply two numbers correctly"
		(should-equal (* 3 4) 12)
		(should-equal (* 0 100) 0)
		(should-equal (* -2 5) -10))

	(it "should divide two numbers correctly"
		(should-equal (/ 10 2) 5)
		(should-equal (/ 9 3) 3)
		(should-equal (/ 100 10) 10)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Comparison Operations
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Comparisons"
	(it "should compare numbers correctly"
		(should-be-true (< 5 10))
		(should-be-true (> 10 5))
		(should-be-true (<= 5 5))
		(should-be-true (>= 10 10)))

	(it "should test equality correctly"
		(should-equal 5 5)
		(should-not-equal 5 10)
		(should-equal "hello" "hello")
		(should-not-equal "hello" "world"))

	(it "should use comparison assertions"
		(should-be-less-than 5 10)
		(should-be-greater-than 10 5)
		(should-be-less-than -5 0)
		(should-be-greater-than 0 -5)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean Logic
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Boolean Operations"
	(it "should handle boolean AND correctly"
		(should-be-true (and :t :t))
		(should-be-false (and :t :nil))
		(should-be-false (and :nil :t))
		(should-be-false (and :nil :nil)))

	(it "should handle boolean OR correctly"
		(should-be-true (or :t :t))
		(should-be-true (or :t :nil))
		(should-be-true (or :nil :t))
		(should-be-false (or :nil :nil)))

	(it "should handle boolean NOT correctly"
		(should-be-true (not :nil))
		(should-be-false (not :t))
		(should-be-false (not 1))
		(should-be-true (not :nil))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Nil Checks
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Nil Handling"
	(it "should detect nil values"
		(should-be-nil :nil)
		(should-not-be-nil :t)
		(should-not-be-nil 0)
		(should-not-be-nil ""))

	(it "should distinguish nil from false"
		(should-be-nil :nil)
		(should-not-be-nil :nil)  ; This will fail - demonstrates failure output
		))
