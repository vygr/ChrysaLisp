; Lisp (getf ...)

(enums +kn_call 0
	(enum exit callback ping open child))

(structure +kn_msg 0
	(ulong func key)
	(netid reply_id))
