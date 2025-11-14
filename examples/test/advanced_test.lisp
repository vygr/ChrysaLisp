;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Advanced Test Examples
; Demonstrates advanced testing features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; Functions Under Test
;;;;;;;;;;;;;;;;;;;;;;;;

(defun divide-safe (a b)
	; Safe division that throws on divide by zero
	(if (= b 0)
		(throw "Division by zero" divide-safe)
		(/ a b)))

(defun factorial (n)
	; Calculate factorial
	(if (<= n 1)
		1
		(* n (factorial (dec n)))))

(defun fibonacci (n)
	; Calculate nth fibonacci number
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(:t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Error Handling Tests
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Error Handling"
	(it "should throw on division by zero"
		(should-throw (lambda () (divide-safe 10 0))))

	(it "should not throw on valid division"
		(should-not-throw (lambda () (divide-safe 10 2))))

	(it "should handle caught errors gracefully"
		(defq result :nil)
		(catch
			(divide-safe 10 0)
			(setq result :error))
		(should-equal result :error)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Recursive Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Factorial Function"
	(it "should calculate factorial of 0"
		(should-equal (factorial 0) 1))

	(it "should calculate factorial of 1"
		(should-equal (factorial 1) 1))

	(it "should calculate factorial of 5"
		(should-equal (factorial 5) 120))

	(it "should calculate factorial of 10"
		(should-equal (factorial 10) 3628800)))

(describe "Fibonacci Sequence"
	(it "should calculate fibonacci(0)"
		(should-equal (fibonacci 0) 0))

	(it "should calculate fibonacci(1)"
		(should-equal (fibonacci 1) 1))

	(it "should calculate fibonacci(5)"
		(should-equal (fibonacci 5) 5))

	(it "should calculate fibonacci(10)"
		(should-equal (fibonacci 10) 55)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Cases
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Edge Cases"
	(it "should handle negative numbers"
		(should-equal (+ -5 -3) -8)
		(should-equal (* -2 -3) 6)
		(should-be-true (< -10 -5)))

	(it "should handle zero"
		(should-equal (* 0 100) 0)
		(should-equal (+ 0 0) 0)
		(should-equal (- 0 0) 0))

	(it "should handle large numbers"
		(should-equal (+ 1000000 1000000) 2000000)
		(should-be-true (> 1000000 999999))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Skipped Tests
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Skipped Tests"
	(it "should run this test"
		(should-be-true :t))

	(xit "should skip this test"
		(should-equal 1 2))  ; This won't fail because it's skipped

	(xit "should also skip this test"
		(should-be-false :t)))  ; This won't fail either

;;;;;;;;;;;;;;;;;;;;;;;;
; Custom Messages
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Custom Error Messages"
	(it "should show custom message on failure"
		(should-equal 10 10 "Ten should equal ten"))

	(it "should show helpful context in custom messages"
		(defq expected_value 42)
		(defq actual_value 42)
		(should-equal actual_value expected_value
			(cat "Expected the answer to life, universe, and everything: "
				(str expected_value)))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Nested Describes
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Math Library"
	(describe "Basic Operations"
		(it "should add"
			(should-equal (+ 2 2) 4))

		(it "should multiply"
			(should-equal (* 3 3) 9)))

	(describe "Advanced Operations"
		(it "should calculate power"
			(should-equal (* 2 2 2) 8))

		(it "should calculate modulo"
			(should-equal (% 10 3) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Testing with State
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "Stateful Operations"
	(it "should maintain state within a test"
		(defq counter 0)
		(setq counter (inc counter))
		(should-equal counter 1)
		(setq counter (inc counter))
		(should-equal counter 2)
		(setq counter (inc counter))
		(should-equal counter 3))

	(it "should have fresh state in each test"
		; This counter is independent of the previous test
		(defq counter 0)
		(should-equal counter 0)
		(setq counter 10)
		(should-equal counter 10)))
