(import "class/stream/lisp.inc")

(enums +stream_mail_state 0
	(enum started stopping stopped aborted))

(structure +out +stream_size
	(struct id +net_id_size)
	(ptr ack_mbox_addr)
	(uint ack_mbox_id ack_seqnum seqnum state))

(defun out-set-state (out state)
	; (out-set-state out num) -> out
	(setf out +out_state state 0))