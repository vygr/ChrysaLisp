;lisp bindings
(ffi msg-in-stream "class/stream/lisp_msginstream" 0)
(ffi msg-in-next-msg "class/msg_in/lisp_next_msg" 0)

(defun-bind msg-in-mbox (in)
	;(msg-in-mbox msg-in-stream) -> mbox
	(+ (logand (task-mailbox) (const (lognot 0xffffffff)))
		(get-field in msg_in_mbox_id (const (neg int_size)))))

(defun-bind msg-in-get-state (in)
	;(msg-in-get-state msg-in-stream) -> num
	(get-field in msg_in_state (const (neg int_size))))
