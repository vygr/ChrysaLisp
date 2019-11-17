;lisp bindings
(ffi msg-out-stream "class/stream/lisp_msgoutstream" 0)

(defun msg-out-set-state (out state)
	(set-field out (const msg_out_state) (const (neg int_size)) state))
