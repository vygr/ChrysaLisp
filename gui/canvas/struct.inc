(structure +canvas +view_size
	(ptr pixmap edges texture)
	(pptr edges_start)
	(pubyte coverage)
	(uint scale cx cy cx1 cy1 color flags))

(structure +edge +ln_fnode_size
	(int x ys ye w dda)
	(align))

;load flags
(bits +load_flag 0
	(bit shared film noswap))

;flags
(bits +canvas_flag 0
	(bit antialias))

;winding
(enums +winding 0
	(enum odd_even none_zero))
