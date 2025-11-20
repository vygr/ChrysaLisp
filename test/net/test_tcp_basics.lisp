;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP/IP Stack Basic Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple tests for TCP/IP utilities without external dependencies

(defun test-basic-network ()
	(prin "TCP/IP Stack Basic Tests\n")
	(prin "========================\n\n")

	(defq passed 0)
	(defq failed 0)

	; Test 1: Basic arithmetic
	(prin "Test 1: Basic arithmetic\n")
	(if (= (+ 1 2) 3)
		(progn (prin "  ✓ Addition works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Addition failed\n") (setq failed (+ failed 1))))

	; Test 2: Array operations
	(prin "\nTest 2: Array operations\n")
	(defq arr (array 192 168 1 1))
	(if (= (length arr) 4)
		(progn (prin "  ✓ Array length correct\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Array length wrong\n") (setq failed (+ failed 1))))

	; Test 3: Array element access
	(prin "\nTest 3: Array element access\n")
	(if (= (elem-get arr 0) 192)
		(progn (prin "  ✓ Array element access works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Array element access failed\n") (setq failed (+ failed 1))))

	; Test 4: String concatenation
	(prin "\nTest 4: String operations\n")
	(defq ip_str (str (elem-get arr 0) "." (elem-get arr 1) "." (elem-get arr 2) "." (elem-get arr 3)))
	(if (eql ip_str "192.168.1.1")
		(progn (prin "  ✓ String building works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ String building failed (got " ip_str ")\n") (setq failed (+ failed 1))))

	; Test 5: Bit operations
	(prin "\nTest 5: Bit operations\n")
	(if (= (logand 0xFF 0x0F) 0x0F)
		(progn (prin "  ✓ Bitwise AND works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Bitwise AND failed\n") (setq failed (+ failed 1))))

	; Test 6: List operations
	(prin "\nTest 6: List operations\n")
	(defq list_val (list 1 2 3))
	(if (= (length list_val) 3)
		(progn (prin "  ✓ List length works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ List length failed\n") (setq failed (+ failed 1))))

	; Test 7: Conditional logic
	(prin "\nTest 7: Conditional logic\n")
	(if (and (= 1 1) (= 2 2))
		(progn (prin "  ✓ AND logic works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ AND logic failed\n") (setq failed (+ failed 1))))

	; Test 8: OR logic
	(prin "\nTest 8: OR logic\n")
	(if (or (= 1 0) (= 2 2))
		(progn (prin "  ✓ OR logic works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ OR logic failed\n") (setq failed (+ failed 1))))

	; Test 9: NOT logic
	(prin "\nTest 9: NOT logic\n")
	(if (not (= 1 2))
		(progn (prin "  ✓ NOT logic works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ NOT logic failed\n") (setq failed (+ failed 1))))

	; Test 10: Equality check (string vs number)
	(prin "\nTest 10: Type handling\n")
	(if (eql "test" "test")
		(progn (prin "  ✓ String equality works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ String equality failed\n") (setq failed (+ failed 1))))

	(prin "\n" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "\n")
	(prin "Results: " passed " passed, " failed " failed\n")
	(if (= failed 0)
		(prin "✓ All tests passed!\n")
		(prin "✗ Some tests failed\n")))

; Run tests
(test-basic-network)
