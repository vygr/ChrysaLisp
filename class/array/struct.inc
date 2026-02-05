(structure +array +seq_size
	(plong begin)
	(uint length capacity)
	;use all remaining cells for short array storage
	(struct elems (some! (lambda (%0)
		(if (< base %0) (- (* %0 2) base))) (list mem_heap_cell_sizes))))
