(import "class/stream/lisp.inc")
(import "sys/list/struct.inc")
(import "./struct.inc")

(defun in-mbox (in)
	; (in-mbox in) -> mbox
	(cat (char (getf in +in_mbox_id 0) +long_size)
		(slice (task-mbox) +long_size -1)))

(defun in-get-state (in)
	; (in-get-state in) -> num
	(getf in +in_state 0))

(defun in-set-state (in state)
	; (in-set-state in num) -> in
	(setf in +in_state state 0))
