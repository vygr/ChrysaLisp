(import "././list/struct.inc")

;:hmap inherits from :list, BUT we want to massage the layout
;of the underlying :array/:list structure...

(structure +hmap +array_base_size
	(plong parent)
	;use all remaining cells for short array storage
	;scale up to next block size so we get more [key value] pairs
	(struct elems (some! (lambda (%0)
		(if (< base %0) (- (* %0 4) base))) (list (last +mem_heap_cell_sizes)))))
