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

;what comes back from an open: the id of the task in reply_id, 0 if none
;was started, and the node of the kernel that answers. An id of 0 does
;not say who could not, so this does, the task started or not. It is
;after all that there was before it, so what reads only the id still can
(structure +kn_msg_reply +kn_msg_size
	(nodeid node))

(structure +kn_msg_ping 0
	(long func)
	(nodeid via origin)
	;the machine the origin is on
	(nodeid system)
	(uint session hops)
	;a hash of the origin's services, 0 for none, and if the services follow
	(uint hash full)
	;the longest it will be till the origin's next ping, and if this is a kick
	(uint interval kick)
	(offset services))
