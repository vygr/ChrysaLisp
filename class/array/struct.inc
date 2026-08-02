(import "././seq/struct.inc")
(import "sys/mem/struct.inc")

(structure +array +seq_size
	(uint length capacity num_buckets)
	(plong begin)
	;use all remaining cells for short array storage
	(struct elems (some! (lambda (%0)
		(if (< base %0) (- (* %0 2) base))) (list (last +mem_heap_cell_sizes)))))
