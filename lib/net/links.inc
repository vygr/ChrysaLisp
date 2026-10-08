;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/net/links.inc - the links of this node
;
; (net-links) gives what the node knows of each link it has, shared memory
; to a node of this machine or TCP to one of another, a str each with
; these fields. It is the kernel's own record, lk_node in
; sys/link/class.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "service/net/lisp_links" net-links)
; (net-links) -> (link ...)

;the node at the other end, the machine it is on, how many tasks it has,
;its process if it is on this machine, and the bytes of mail this node
;has sent on the link, which only grows
(structure +link 0
	(struct peer_node +node_id_size)
	(struct peer_system +node_id_size)
	(uint peer_tasks peer_pid)
	(long sent))
