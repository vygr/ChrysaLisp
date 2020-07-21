(include 'sys/mail/class.inc)

(def-class 'sys_kernel)
(dec-method :id 'sys/kernel/id :static nil '(r0))
(dec-method :total 'sys/kernel/total :static nil '(r0))
(dec-method :opts 'sys/kernel/opts :static '(r0))
(dec-method :kernel 'sys/kernel/kernel :static '(r0))

(dec-method :lisp_total 'sys/kernel/lisp_total :static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(def-enum 'kn_call)
	(enum 'callback 'open 'child 'route 'ping)
(def-enum-end)

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct 'kn_msg)
	(ulong 'user)
	(struct 'reply_id 'id)
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
	(uint 'via 'session 'origin 'hops)
	(offset 'services)
(def-struct-end)
