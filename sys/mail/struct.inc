(structure +mail_timeout 0
	(long time id))

(structure +node_id 0
	(long node1 node2))

(structure +net_id 0
	(long mbox_id)
	(nodeid node_id))

(structure +mbox +ln_node_size
	(struct list +lh_list_size)
	(ptr tcb owner_tcb)
	(uint id))

(structure +msg +ln_node_size
	(long timestamp)
	(netid dest src)
	(uint frag_length frag_offset total_length)
	(ptr obj frag_data))
