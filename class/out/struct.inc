(import "class/stream/struct.inc")

(enums +stream_mail_state 0
	(enum started stopping stopped aborted))

(structure +out +stream_size
	(netid id)
	(ptr ack_mbox_addr)
	(uint ack_mbox_id ack_seqnum seqnum state))

(structure +stream_msg 0
	(netid ack_id)
	(uint seqnum state)
	(offset data))
