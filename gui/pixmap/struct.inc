(import "class/obj/struct.inc")

(structure +pixmap +obj_size
	(int type)
	(ptr texture stream)
	(pubyte bufp)
	(uint stream_type width height stride)
	(align)
	(offset data))

;upload modes
(enums +pixmap_mode 0
	(enum normal glyph greyscale))
