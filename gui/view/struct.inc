(structure +view +hmap_size
	(offset start)
	(struct node +ln_node_size)
	(struct list +lh_list_size)
	(netid owner_id)
	(ptr dirty_region opaque_region ctx_node)
	(long id)
	(uint flags)
	(int ctx_x ctx_y x y w h cw ch))

(bits +view_flag 0
	(bit solid opaque dirty_all hidden at_back at_front subtree))
