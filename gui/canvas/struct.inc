(import "././view/struct.inc")
(import "sys/list/struct.inc")

(structure +canvas +view_size
	(ptr pixmap edges texture)
	(pptr edges_start)
	(pubyte coverage)
	(uint scale cx cy cx1 cy1 color flags)
	;span, the function every run of a fill ends in, :span_noclip unless
	;something else is put there; span_data is its own, what it wants of the
	;canvas's, which :fpoly and :fbox never look at. At the end, so no field
	;before them moves
	(ptr span span_data))

;a gradient paint, the str a canvas's span_data is when its span is
;:span_linear or :span_radial: two rows of a matrix from a pixel to the
;gradient's own space, u a0 + ax x + ay y and v b0 + bx x + by y, 32 bits
;of fraction, the pixel's centre in them. A linear's t is u, a radial's
;the length of u and v; the top 8 bits of t's fraction are where in the
;lut it is, 256 colors not premultiplied, and spread says what is past 0
;and 1: pad, reflect or repeat. A radial's focus, where its t is 0, is
;its own space's making, (:set_radial), and one number here: so much of u
;added to that length, 16 bits of fraction, 0 for one about its centre.
;After the lut is a row, the width of the canvas's pixmap, a run's colors
;made there and then blended, :span_row
(structure +paint 0
	(long spread a0 ax ay b0 bx by focus)
	(uint lut))
(defq +paint_lut_size 256 +paint_row (+ +paint_lut (* 256 +int_size)))
(enums +spread 0
	(enum pad reflect repeat))

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
	(bit shared noswap free))

;flags
(bits +canvas_flag 0
	(bit antialias shader))

;winding
(enums +winding 0
	(enum odd_even none_zero))
