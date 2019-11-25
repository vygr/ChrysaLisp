;lisp bindings
(ffi in-stream "class/in/lisp_create" 0)
(ffi in-next-msg "class/in/lisp_next_msg" 0)

(defun-bind in-mbox (in)
	;(in-mbox in) -> mbox
	(+ (logand (task-mailbox) (const (lognot 0xffffffff)))
		(get-field in (const in_mbox_id) (const (neg int_size)))))

(defun-bind in-get-state (in)
	;(in-get-state in) -> num
	(get-field in (const in_state) (const (neg int_size))))

(defun-bind in-set-state (in state)
	;(in-set-state in num) -> in
	(set-field in (const in_state) (const (neg int_size)) state))
