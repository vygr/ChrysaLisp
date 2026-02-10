(import "class/stream/struct.inc")
(import "sys/list/struct.inc")

(structure +in +stream_size
	(struct list +lh_list_size)
	(ptr mbox_addr)
	(uint mbox_oid mbox_id ack_seqnum seqnum state))
