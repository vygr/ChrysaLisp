; Lisp (getf ...)

(enums +kn_call 0
	(enum callback open child ping))

(structure +kn_msg 0
	(ulong key)
	(netid reply_id)
	(offset reply_size)
	(ulong func))

(structure +kn_msg_callback +kn_msg_size
	(ptr addr))

(structure +kn_msg_open +kn_msg_size
	(offset pathname))

(structure +kn_msg_ping +kn_msg_size
	(nodeid via)
	(nodeid origin)
	(uint session hops)
	(offset services))
