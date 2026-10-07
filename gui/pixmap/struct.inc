(import "class/obj/struct.inc")

(structure +pixmap +obj_size
	(int type)
	(ptr texture stream)
	(pubyte bufp)
	(uint stream_type width height stride)
	;the pixels. They follow the object, or they are in shared memory,
	;under the name key, that other nodes of this machine can draw on.
	;owner is the pixmap that made the shared memory.
	(pubyte data)
	(ptr key)
	(long handle owner)
	(align)
	(offset pixels))

;upload modes
(enums +pixmap_mode 0
	(enum normal glyph greyscale))
