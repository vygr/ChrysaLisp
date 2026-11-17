(import "class/obj/struct.inc")

(structure +pixmap +obj_size
	(int type)
	(ptr texture)
	(uint width height stride)
	;the pixels. They follow the object, or they are the block of a buf,
	;shared memory that other nodes of this machine can draw on, and buf
	;is 0 if they are not
	(pubyte data)
	(ptr buf)
	(align)
	(offset pixels))

;upload modes
(enums +pixmap_mode 0
	(enum normal glyph greyscale))
