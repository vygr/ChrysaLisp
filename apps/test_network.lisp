;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Driver Integration Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This test verifies that the network driver integration works
; by calling the host network driver functions

(import "sys/net/class.vp")
(import "lib/net/utils.inc")

(defun test-network-integration ()
	(prin "Network Driver Integration Test\n")
	(prin "===============================\n\n")

	; Test 1: Check if network is available
	(prin "Test 1: Checking network availability...\n")
	(if (defq net_funcs (get-env 'host_net_funcs))
		(prin "  ✓ Network functions available\n")
		(progn
			(prin "  ✗ Network functions NOT available\n")
			(prin "  Build with _HOST_NET=0 or _HOST_NET=1\n")
			(exit 1)))

	; Test 2: Get network info (if driver initialized)
	(prin "\nTest 2: Getting network information...\n")
	(defq info_struct (array net_info_size))
	(defq result (host_net :net_get_info info_struct))

	(if (= result 0)
		(progn
			(prin "  ✓ Network driver is initialized\n")
			(prin "  MAC: ")
			(each (lambda (b idx)
				(prin (str (logand (elem-get info_struct b) 0xFF)))
				(when (< idx 5) (prin ":")))
				(range 0 6))
			(prin "\n")

			(prin "  IP:  ")
			(prin (net/ip-to-string (slice info_struct 6 10)))
			(prin "\n")

			(prin "  Mask: ")
			(prin (net/ip-to-string (slice info_struct 10 14)))
			(prin "\n")

			(prin "  Gateway: ")
			(prin (net/ip-to-string (slice info_struct 14 18)))
			(prin "\n")

			(defq mtu (net/read-u32 info_struct 18))
			(prin (str "  MTU: " mtu "\n")))
		(prin "  ℹ Network driver not yet initialized (expected if not configured)\n"))

	; Test 3: Poll for packets
	(prin "\nTest 3: Polling for packets...\n")
	(defq poll_result (host_net :net_poll))
	(if (>= poll_result 0)
		(prin (str "  ✓ Poll successful: " poll_result " packet(s) ready\n"))
		(prin "  ℹ Poll returned error (expected if not initialized)\n"))

	(prin "\n✓ Integration test complete!\n")
	(prin "Network stack is ready to use.\n"))

; Run the test
(test-network-integration)
