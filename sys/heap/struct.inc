(import "././list/struct.inc")

(structure +hp_heap +lh_list_size
	(ptr free_flist)
	(uint cellsize blocksize))

(structure +hp_block +ln_node_size
	(ptr free_flist free_flist_tail)
	(uint cellcnt freecnt))

(structure +hp_cell 0
	(ptr block next))

;what a validate build fills a newly allocated cell with, and a freed one,
;and the guard word it keeps after each cell, outside the cell size
(defq +hp_cell_fill 0xa5a5a5a5a5a5a5a5 +hp_cell_free_fill 0x5a5a5a5a5a5a5a5a
	+hp_cell_guard 0x0bad0bad0bad0bad
	+hp_cell_guard_size (if (>= *build_mode* 2) +long_size 0))
