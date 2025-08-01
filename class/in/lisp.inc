(import "class/stream/lisp.inc")
(import "sys/list/lisp.inc")

;lisp bindings
(ffi "class/in/lisp_create" in-stream)
; (in-stream) -> in_stream
(ffi "class/in/lisp_next_msg" in-next-msg)
; (in-next-msg in_stream) -> msg

(structure +in +stream_size
	(struct list +lh_list_size)
	(ptr mbox_addr)
	(uint mbox_oid mbox_id ack_seqnum seqnum state))

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
