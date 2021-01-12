(include "sys/mail/class.inc")

(def-class 'sys_kernel)
(dec-method :id 'sys/kernel/id :static nil '(r0 r1))
(dec-method :kernel 'sys/kernel/kernel :static '(r0))
(dec-method :ping 'sys/kernel/ping)

;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(def-enum 'kn_call)
	(enum 'callback 'open 'child 'ping)
(def-enum-end)

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct 'kn_msg)
	(ulong 'user)
	(struct 'reply_id 'net_id)
	(offset 'reply_size)
	(ulong 'func)
(def-struct-end)

(def-struct 'kn_msg_callback 'kn_msg)
	(ptr 'addr)
(def-struct-end)

(def-struct 'kn_msg_open 'kn_msg)
	(offset 'pathname)
(def-struct-end)

(def-struct 'kn_msg_ping 'kn_msg)
	(struct 'via 'node_id)
	(struct 'origin 'node_id)
	(uint 'session 'hops)
	(offset 'services)
(def-struct-end)

(defcvar 'ping_period 20000000)
