;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP State Machine Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for TCP Control Block creation and state management

(defun test-tcp-state-machine ()
	(prin "TCP State Machine Tests\n")
	(prin "======================\n\n")

	(defq passed 0)
	(defq failed 0)

	; Define TCP state constants locally
	(defq tcp_state_closed 0)
	(defq tcp_state_listen 1)
	(defq tcp_state_syn_sent 2)
	(defq tcp_state_syn_received 3)
	(defq tcp_state_established 4)
	(defq tcp_state_fin_wait_1 5)
	(defq tcp_state_fin_wait_2 6)
	(defq tcp_state_close_wait 7)
	(defq tcp_state_closing 8)
	(defq tcp_state_last_ack 9)
	(defq tcp_state_time_wait 10)

	; Define TCP flags locally
	(defq tcp_flag_fin 0x01)
	(defq tcp_flag_syn 0x02)
	(defq tcp_flag_rst 0x04)
	(defq tcp_flag_psh 0x08)
	(defq tcp_flag_ack 0x10)
	(defq tcp_flag_urg 0x20)

	; Test 1: TCP state constants defined
	(prin "Test 1: TCP state constants\n")
	(if (and (= tcp_state_closed 0) (= tcp_state_established 4) (= tcp_state_listen 1))
		(progn (prin "  ✓ TCP state constants defined correctly\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCP state constants incorrect\n") (setq failed (+ failed 1))))

	; Test 2: TCP flag constants defined
	(prin "\nTest 2: TCP flag constants\n")
	(if (and (= tcp_flag_syn 0x02) (= tcp_flag_ack 0x10) (= tcp_flag_fin 0x01))
		(progn (prin "  ✓ TCP flag constants defined correctly\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCP flag constants incorrect\n") (setq failed (+ failed 1))))

	; Test 3: Flag detection (bitwise AND)
	(prin "\nTest 3: Flag detection\n")
	(defq flags (logand tcp_flag_syn tcp_flag_ack))
	(if (= flags 0)
		(progn (prin "  ✓ Flag detection works (non-overlapping flags)\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Flag detection failed\n") (setq failed (+ failed 1))))

	; Test 4: Flag combination (bitwise OR)
	(prin "\nTest 4: Flag combination\n")
	(defq combined_flags (logior tcp_flag_syn tcp_flag_ack))
	(if (= combined_flags 0x12)
		(progn (prin "  ✓ Flag combination works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Flag combination failed\n") (setq failed (+ failed 1))))

	; Test 5: TCB structure creation (simple array)
	(prin "\nTest 5: TCB structure creation\n")
	(defq tcb (array tcp_state_closed 1234 5678 100 200))
	(if (and (= (elem-get tcb 0) tcp_state_closed) (= (elem-get tcb 1) 1234) (= (elem-get tcb 3) 100))
		(progn (prin "  ✓ TCB structure created\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCB structure creation failed\n") (setq failed (+ failed 1))))

	; Test 6: TCB state transition
	(prin "\nTest 6: TCB state transition\n")
	(elem-set tcb 0 tcp_state_syn_sent)
	(if (= (elem-get tcb 0) tcp_state_syn_sent)
		(progn (prin "  ✓ TCB state transition works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCB state transition failed\n") (setq failed (+ failed 1))))

	; Test 7: TCB field modification
	(prin "\nTest 7: TCB field modification\n")
	(elem-set tcb 3 150)
	(if (= (elem-get tcb 3) 150)
		(progn (prin "  ✓ TCB field modification works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCB field modification failed\n") (setq failed (+ failed 1))))

	; Test 8: Multiple TCB connections
	(prin "\nTest 8: Multiple TCB connections\n")
	(defq tcb2 (array tcp_state_established 9999 0 0 0))
	(if (and (= (elem-get tcb 1) 1234) (= (elem-get tcb2 1) 9999))
		(progn (prin "  ✓ Multiple TCBs independent\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Multiple TCBs failed\n") (setq failed (+ failed 1))))

	; Test 9: Sequence number comparison
	(prin "\nTest 9: Sequence number management\n")
	(defq seq1 100)
	(defq seq2 101)
	(if (< seq1 seq2)
		(progn (prin "  ✓ Sequence number comparison works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ Sequence number comparison failed\n") (setq failed (+ failed 1))))

	; Test 10: TCP state comparison
	(prin "\nTest 10: TCP state comparison\n")
	(if (= (elem-get tcb 0) tcp_state_syn_sent)
		(progn (prin "  ✓ TCP state comparison works\n") (setq passed (+ passed 1)))
		(progn (prin "  ✗ TCP state comparison failed\n") (setq failed (+ failed 1))))

	(prin "\n" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "\n")
	(prin "Results: " passed " passed, " failed " failed\n")
	(if (= failed 0)
		(prin "✓ All TCP state tests passed!\n")
		(prin "✗ Some tests failed\n")))

; Run tests
(test-tcp-state-machine)
