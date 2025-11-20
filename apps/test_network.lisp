;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Driver Integration Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prin "Network Driver Integration Test\n")
(prin "===============================\n\n")

(prin "Test 1: Checking host_net class availability...\n")

; Try to call host_net :poll
(prin "  Attempting to call host_net :poll...\n")
(defq result (host_net :poll))
(prin (str "  ✓ host_net class is available! Poll result: " result "\n"))
(prin "\n✓ Network driver is integrated into boot image!\n")
