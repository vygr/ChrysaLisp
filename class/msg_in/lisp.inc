;lisp bindings
(ffi msg-in-stream "class/stream/lisp_msginstream" 0)

(defun msg-in-mbox (in)
	(+ (logand (task-mailbox) (const (lognot 0xffffffff)))
		(get-field in msg_in_mbox_id (const (neg int_size)))))
