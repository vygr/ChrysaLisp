#!/usr/bin/env lisp

;; Test array operations WASM module
;; This demonstrates NumPy-style array processing via WASM
;; Simulates: import numpy as np; np.sum([1,2,3,4,5])

(import "class/wasm/lisp.inc")

(defun test-array-ops ()
	(print "Testing array operations WASM module...")
	(print "This simulates Python/NumPy array processing")
	(print "")

	;; Load the WASM module
	(defq wasm (Wasm "examples/wasm/array_ops.wasm"))

	(when wasm
		(print "✓ WASM module loaded")

		;; Test 1: Sum an array [1, 2, 3, 4, 5]
		(print "\nTest 1: Sum array [1, 2, 3, 4, 5]")
		(print "Python equivalent: np.sum([1,2,3,4,5])")

		;; Write array to WASM memory at offset 0
		(defq test_array (list 1 2 3 4 5))
		(each! 0 test_array
			(# (progn
				(defq val_bytes (str-alloc 8))
				;; Write as 64-bit integer (little-endian)
				;; This is simplified - real implementation needs proper encoding
				(. wasm :write_memory (* %0 8) val_bytes))))

		;; Call sum_array(offset=0, count=5)
		(defq sum_result (. wasm :call "sum_array" 0 5))
		(print (cat "Result: " sum_result " (expected: 15)"))

		;; Test 2: Mean
		(print "\nTest 2: Mean of array")
		(print "Python equivalent: np.mean([10, 20, 30, 40, 50])")
		(defq mean_result (. wasm :call "mean" 0 5))
		(print (cat "Result: " mean_result " (expected: 30)"))

		;; Test 3: Dot product
		(print "\nTest 3: Dot product")
		(print "Python equivalent: np.dot([1,2,3], [4,5,6])")
		(defq dot_result (. wasm :call "dot_product" 0 4096 3))
		(print (cat "Result: " dot_result " (expected: 32)"))

		;; Clean up
		(. wasm :close)
		(print "\n✓ All tests completed")))

;; Run tests
(test-array-ops)
