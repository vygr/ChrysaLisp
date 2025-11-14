(include "lib/asm/class.inc")
(include "./abi.inc")

; Host network driver class
(def-class host_net :nil
	; Virtual methods - called via ABI to C driver
	(dec-method :init :nil :virtual (abi-args 6) (:r0))
	(dec-method :deinit :nil :virtual () (:r0))
	(dec-method :send :nil :virtual (abi-args 2) (:r0))
	(dec-method :recv :nil :virtual (abi-args 2) (:r0))
	(dec-method :poll :nil :virtual () (:r0))
	(dec-method :get_info :nil :virtual (abi-args 1) (:r0))
	(dec-method :get_stats :nil :virtual (abi-args 1) (:r0))

	; Static wrapper methods - call virtual methods
	(dec-method :net_init sys/net/init :static (:r0 :r1 :r2 :r3 :r4 :r5) (:r0))
	(dec-method :net_deinit sys/net/deinit :static () (:r0))
	(dec-method :net_send sys/net/send :static (:r0 :r1) (:r0))
	(dec-method :net_recv sys/net/recv :static (:r0 :r1) (:r0))
	(dec-method :net_poll sys/net/poll :static () (:r0))
	(dec-method :net_get_info sys/net/get_info :static (:r0) (:r0))
	(dec-method :net_get_stats sys/net/get_stats :static (:r0) (:r0)))

; Helper macro to call host network functions
; Similar to host-os-call but uses r4 (host_net_funcs register)
(defun host-net-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name
	;_2 = member name
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg (default r4 for network)
	;_6 = temp reg
	(setd _5 :r4 _6 :r13)
	(abi-call-table _1 _2 _5 _3 _4 _6))
