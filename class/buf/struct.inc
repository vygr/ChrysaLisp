(import "././obj/struct.inc")

(structure +buf +obj_size
	;a block of memory the nodes of this machine share, under a key. data is
	;where it is mapped on this node, bytes its length, handle the host's,
	;and flags what this one is to it
	(pubyte data)
	(long bytes key handle)
	(uint flags))

;flags, owner if this one made it, and so lets go of the key
(bits +buf_flag 0
	(bit owner))
