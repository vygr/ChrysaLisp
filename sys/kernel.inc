(import 'sys/class.inc)
(import 'sys/mail.inc)

;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(def-enum 'kn_call)
	(enum 'callback 'task_open 'task_child 'task_route)
(def-enum-end)

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(def-struct 'kn_msg 'msg_header)
	(ulong 'user)
	(struct 'reply_id 'id)
	(offset 'reply_size)
	(ulong 'function)
(def-struct-end)

(def-struct 'kn_msg_callback 'kn_msg)
	(ptr 'addr)
(def-struct-end)

(def-struct 'kn_msg_open 'kn_msg)
	(offset 'pathname)
(def-struct-end)

(def-struct 'kn_msg_child 'kn_msg)
	(offset 'pathname)
(def-struct-end)

(def-struct 'kn_msg_link_route 'kn_msg)
	(ulong 'origin)
	(ulong 'via)
	(ulong 'hops)
(def-struct-end)

(def-class 'kernel)
(def-method 'id 'sys/kernel_id 'static nil '(r0))
(def-method 'total 'sys/kernel_total 'static nil '(r0))
(def-method 'opts 'sys/kernel_opts 'static '(r0))
(def-method 'kernel 'sys/kernel 'static '(r0 r1))
