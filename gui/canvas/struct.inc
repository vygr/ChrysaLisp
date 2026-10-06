(import "././view/struct.inc")
(import "sys/list/struct.inc")

(structure +canvas +view_size
	(ptr pixmap edges texture)
	(pptr edges_start)
	(pubyte coverage)
	(uint scale cx cy cx1 cy1 color flags))

(structure +edge +ln_fnode_size
	(int x ys ye w dda)
	(align))

;which way a swap goes. Write is the pixmap to the texture, to which a
;+pixmap_mode and any +swap_flag can be added. Read, or any negative number,
;goes the other way, the texture is read back into the pixmap, and a pixmap
;that was freed is made again.
(enums +swap -1
	(enum read write))

;swap flags, with the +pixmap_mode to upload in. Free lets go of the pixmap
;once it is a texture. The canvas still draws, but can not be drawn on.
;Keep leaves the texture as it is, there is no upload.
(bits +swap_flag 8
	(bit free keep))

;load flags, free is for an image that is only ever shown
(bits +load_flag 0
	(bit shared film noswap free))

;flags
(bits +canvas_flag 0
	(bit antialias shader))

;winding
(enums +winding 0
	(enum odd_even none_zero))
