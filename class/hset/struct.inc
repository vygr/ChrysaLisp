(import "class/obj/struct.inc")

(structure +hset +obj_size
	(uint num_buckets)
	(ptr buckets key_callback))
