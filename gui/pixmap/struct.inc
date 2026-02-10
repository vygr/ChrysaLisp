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
	(uint type)         ;image type
	(uint width)        ;image w
	(uint height)       ;image h
	(uint trans))       ;image transparent colour

(structure +tga 0
	(ubyte imagedata)   ;id field offset
	(ubyte colmap)      ;color map
	(ubyte type)        ;targa type
	(ubyte a b)         ;nint16 colmap_fei  ;color map origin (non aligned !)
	(ubyte c d)         ;nint16 colmap_num  ;color map number (non aligned !)
	(ubyte colmap_size) ;color map size
	(struct pad 4)      ;padding ?
	(ushort width)
	(ushort height)
	(ubyte depth)
	(ubyte origin))
