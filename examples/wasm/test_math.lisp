; Test script for math WASM module
; This demonstrates basic WASM function calls with arguments and return values

(import "class/wasm/lisp.inc")

(defun test-math-wasm ()
	(print "Testing math WASM module...")
	(print "")

	; Load the WASM module
	(defq wasm (Wasm "examples/wasm/math.wasm"))
	(defq filepath (get :filepath wasm))

	(cond
		(filepath
			(print "✓ WASM module loaded successfully")
			(print "")

			; Test 1: Addition
			(print "Test 1: add(5, 10)")
			(defq result (. wasm :call "add" 5 10))
			(print (cat "  Result: " result " (expected: 15)"))
			(print "")

			; Test 2: Multiplication
			(print "Test 2: multiply(6, 7)")
			(setq result (. wasm :call "multiply" 6 7))
			(print (cat "  Result: " result " (expected: 42)"))
			(print "")

			; Test 3: Fibonacci
			(print "Test 3: fibonacci(10)")
			(setq result (. wasm :call "fibonacci" 10))
			(print (cat "  Result: " result " (expected: 55)"))
			(print "")

			; Test 4: Factorial
			(print "Test 4: factorial(5)")
			(setq result (. wasm :call "factorial" 5))
			(print (cat "  Result: " result " (expected: 120)"))
			(print "")

			; Clean up
			(. wasm :close)
			(print "✓ All tests completed successfully"))
		(:t
			(print "✗ Failed to load WASM module"))))

; Run test
(test-math-wasm)
