;;;;;;;;;;;;;;;;;;;;;;;
; kernel mail functions
;;;;;;;;;;;;;;;;;;;;;;;

(enums +kn_call 0
	(enum exit callback signal pin run want ping ping_sys))

;;;;;;;;;;;;;;;;;;
; kernel mail data
;;;;;;;;;;;;;;;;;;

(structure +kn_msg 0
	(long func key)
	(netid reply_id))

(structure +kn_msg_signal 0
	(long func mbox flags))

(structure +kn_msg_callback +kn_msg_size
	(ptr addr))

(structure +kn_msg_open +kn_msg_size
	(offset pathname))

(structure +kn_msg_ping 0
	(long func)
	(nodeid via origin)
	(uint session hops)
	;a hash of the origin's services, 0 for none, and if the services follow
	(uint hash full)
	(offset services))
