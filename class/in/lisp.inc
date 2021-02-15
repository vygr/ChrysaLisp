(import "sys/lisp.inc")

;lisp bindings
(ffi in-stream "class/in/lisp_create" 0)
(ffi in-next-msg "class/in/lisp_next_msg" 0)

(defun in-mbox (in)
	; (in-mbox in) -> mbox
	(cat (char (get-field in (const in_mbox_id) (const (neg +int_size+))) +long_size+)
		(slice +long_size+ -1 (task-mailbox))))

(defun in-get-state (in)
	; (in-get-state in) -> num
	(get-field in (const in_state) (const (neg +int_size+))))

(defun in-set-state (in state)
	; (in-set-state in num) -> in
	(set-field in (const in_state) (const (neg +int_size+)) state))
