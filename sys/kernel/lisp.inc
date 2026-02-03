; Lisp (getf ...)

(enums +kn_call 0
	(enum exit callback ping open child signal))

(structure +kn_msg 0
	(long func key)
	(netid reply_id))

(structure +kn_msg_signal 0
	(long func mbox flags))
