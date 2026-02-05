(structure +sys_mem_header 0
	(ptr heap))

(defq +mem_cell_min_size 24 +mem_cell_max_size 0x4000000
	+max_cell_count (/ (* 32 1024) +mem_cell_min_size)
	mem_heap_cell_sizes (list))

;fill in heap size info
(defq mem_cs +mem_cell_min_size)
(while (<= mem_cs +mem_cell_max_size)
	(push mem_heap_cell_sizes mem_cs)
	(setq mem_cs (* mem_cs 2)))
