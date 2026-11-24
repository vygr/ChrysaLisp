(import "././hset/struct.inc")

;:hmap inherits from :hset, its buckets an hset's of [key value] pairs where
;the hset's are of keys, as a pmap's are a pset's, BUT we want to massage
;the layout of the underlying :array/:list structure: the hset adds no
;fields, so this is laid over the array's...

(structure +hmap +array_elems
	(plong parent)
	;use all remaining cells for short array storage
	;scale up to next block size so we get more [key value] pairs
	(struct elems (some! (lambda (%0)
				(if (< base %0) (- (* %0 4) base))) (list (last +mem_heap_cell_sizes)))))
