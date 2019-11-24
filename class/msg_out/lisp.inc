;lisp bindings
(ffi create-msg-out "class/msg_out/lisp_create" 0)

(defun-bind msg-out-set-state (out state)
	;(msg-out-set-state msg-out num) -> msg-out
	(set-field out (const msg_out_state) (const (neg int_size)) state))
