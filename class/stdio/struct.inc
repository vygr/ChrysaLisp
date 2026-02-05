(structure +stdio +obj_size
	(ptr stdin stdout stderr args))

(structure +stdio_msg_init 0
	(netid stdout)
	(offset reply)
	(netid stderr ack)
	(offset args))
