(import 'sys/class.inc)
(import 'sys/mail/mail.inc)

;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(def-enum 'kn_call)
	(enum 'callback 'open 'child 'route)
(def-enum-end)

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct 'oldkn_msg 'msg_header)
	(ulong 'user)
	(struct 'reply_id 'id)
	(offset 'reply_size)
	(ulong 'function)
(def-struct-end)

(def-struct 'oldkn_msg_callback 'oldkn_msg)
	(ptr 'addr)
(def-struct-end)

(def-struct 'oldkn_msg_open 'oldkn_msg)
	(offset 'pathname)
(def-struct-end)

(def-struct 'oldkn_msg_link_route 'oldkn_msg)
	(uint 'origin)
	(uint 'via)
	(uint 'hops)
(def-struct-end)

(def-struct 'kn_msg)
	(ulong 'user)
	(struct 'reply_id 'id)
	(ulong 'function)
(def-struct-end)

(def-struct 'kn_msg_callback 'kn_msg)
	(ptr 'addr)
(def-struct-end)

(def-struct 'kn_msg_open 'kn_msg)
	(offset 'pathname)
(def-struct-end)

(def-struct 'kn_msg_link_route 'kn_msg)
	(uint 'origin)
	(uint 'via)
	(uint 'hops)
(def-struct-end)

(def-class 'kernel)
(def-method 'id 'sys/kernel/id 'static nil '(r0))
(def-method 'total 'sys/kernel/total 'static nil '(r0))
(def-method 'opts 'sys/kernel/opts 'static '(r0))
(def-method 'kernel 'sys/kernel/kernel 'static '(r0 r1))
