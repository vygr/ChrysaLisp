;lisp bindings
(ffi msg-out-stream "class/stream/lisp_msgoutstream" 0)

(defun-bind msg-out-set-state (out state)
	;(msg-out-set-state msg-in-stream num) -> msg-in-stream
	(set-field out (const msg_out_state) (const (neg int_size)) state))
