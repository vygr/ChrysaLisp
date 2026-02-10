(import "sys/list/struct.inc")

(structure +hp_heap +lh_list_size
	(ptr free_flist)
	(uint cellsize blocksize))

(structure +hp_block +ln_node_size
	(ptr free_flist free_flist_tail)
	(uint cellcnt freecnt))

(structure +hp_cell 0
	(ptr block next))