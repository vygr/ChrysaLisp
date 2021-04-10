; Lisp (getf ...)

(import "class/obj/lisp.inc")

(structure +pixmap +obj_size
	(ptr texture stream bufp)
	(uint stream_type width height stride type)
	(align)
	(offset data))
