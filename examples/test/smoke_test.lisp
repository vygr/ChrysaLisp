;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Smoke Test
; Quick sanity check that the test framework works
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

(describe "Test Framework Smoke Test"
	(it "should pass a simple equality test"
		(should-equal 1 1))

	(it "should handle basic assertions"
		(should-be-true :t)
		(should-be-false :nil)
		(should-be-nil :nil)
		(should-not-be-nil :t))

	(it "should work with lists"
		(defq mylist (list 1 2 3))
		(should-equal (length mylist) 3)
		(should-contain mylist 2))

	(it "should handle comparisons"
		(should-be-less-than 1 2)
		(should-be-greater-than 2 1)))
