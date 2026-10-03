(structure +obj 0
	(pptr vtable)
	(uint count))

;a validate build takes a count above this to be a freed object
(defq +obj_count_max 0x40000000)
