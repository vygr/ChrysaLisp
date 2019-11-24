;lisp bindings
(ffi create-msg-in "class/msg_in/lisp_create" 0)
(ffi msg-in-next-msg "class/msg_in/lisp_next_msg" 0)

(defun-bind msg-in-mbox (in)
	;(msg-in-mbox msg-in) -> mbox
	(+ (logand (task-mailbox) (const (lognot 0xffffffff)))
		(get-field in (const msg_in_mbox_id) (const (neg int_size)))))

(defun-bind msg-in-get-state (in)
	;(msg-in-get-state msg-in) -> num
	(get-field in (const msg_in_state) (const (neg int_size))))

(defun-bind msg-in-set-state (in state)
	;(msg-in-set-state msg-in num) -> msg-in
	(set-field in (const msg_in_state) (const (neg int_size)) state))
