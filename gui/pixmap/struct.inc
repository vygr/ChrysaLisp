(import "class/obj/struct.inc")

(structure +pixmap +obj_size
	(int type)
	(ptr texture stream bufp)
	(uint stream_type width height stride)
	(align)
	(offset data))

(structure +cpm 0
	(uint ident)        ;ident .CPM or .FLM
	(uint bytes)        ;0 if not known, else total size
	(uint version)      ;version number
	(int type)          ;image type
	(uint width)        ;image w
	(uint height)       ;image h
	(uint trans))       ;image transparent colour
