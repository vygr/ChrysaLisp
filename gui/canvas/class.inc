(include "gui/view/class.inc")

(def-class canvas view
	(dec-method :vtable gui/canvas/vtable)
	(dec-method :create gui/canvas/create :static (r0 r1 r2) (r0))
	(dec-method :create_with_pixmap gui/canvas/create_with_pixmap :static (r0) (r0))
	(dec-method :init gui/canvas/init :static (r0 r1 r2 r3 r4) (r0 r1))
	(dec-method :init_with_pixmap gui/canvas/init_with_pixmap :static (r0 r1 r2) (r0 r1))
	(dec-method :swap gui/canvas/swap :static (r0) (r0))
	(dec-method :set_clip gui/canvas/set_clip :static (r0 r7 r8 r9 r10) (r0))
	(dec-method :set_edges gui/canvas/set_edges :static (r0 r1 r2 r3 r4) (r0 r11 r12 r13 r14))
	(dec-method :span_noclip gui/canvas/span_noclip :static (r0 r1 r7 r8 r9) (r0))
	(dec-method :span gui/canvas/span :static (r0 r1 r7 r8 r9) (r0))
	(dec-method :pick gui/canvas/pick :static (r0 r7 r8) (r0 r1))
	(dec-method :plot gui/canvas/plot :static (r0 r7 r8) (r0))
	(dec-method :fbox gui/canvas/fbox :static (r0 r7 r8 r9 r10) (r0))
	(dec-method :fpoly gui/canvas/fpoly :static (r0 r1 r2 r3 r4) (r0))
	(dec-method :ftri gui/canvas/ftri :static (r0 r1 r2 r3 r4 r5 r6) (r0))
	(dec-method :deinit gui/canvas/deinit :override)
	(dec-method :lisp_create gui/canvas/lisp_create :static (r0 r1) (r0 r1))
	(dec-method :lisp_load gui/canvas/lisp_load :static (r0 r1) (r0 r1))
	(dec-method :lisp_save gui/canvas/lisp_save :static (r0 r1) (r0 r1))
	(dec-method :lisp_next_frame gui/canvas/lisp_next_frame :static (r0 r1) (r0 r1))
	(dec-method :lisp_swap gui/canvas/lisp_swap :static (r0 r1) (r0 r1))
	(dec-method :lisp_fill gui/canvas/lisp_fill :static (r0 r1) (r0 r1))
	(dec-method :lisp_plot gui/canvas/lisp_plot :static (r0 r1) (r0 r1))
	(dec-method :lisp_fbox gui/canvas/lisp_fbox :static (r0 r1) (r0 r1))
	(dec-method :lisp_fpoly gui/canvas/lisp_fpoly :static (r0 r1) (r0 r1))
	(dec-method :lisp_ftri gui/canvas/lisp_ftri :static (r0 r1) (r0 r1))
	(dec-method :lisp_resize gui/canvas/lisp_resize :static (r0 r1) (r0 r1))
	(dec-method :lisp_to_argb32 gui/canvas/lisp_to_argb32 :static (r0 r1) (r0 r1))
	(dec-method :lisp_from_argb32 gui/canvas/lisp_from_argb32 :static (r0 r1) (r0 r1)))

(def-struct canvas view_size
	(ptr pixmap edges texture)
	(pptr edges_start)
	(pubyte coverage)
	(uint scale cx cy cx1 cy1 color flags))

(def-struct edge ln_fnode_size
	(int x ys ye w dda)
	(align))

;load flags
(def-bit load_flag 0
	(bit shared film noswap))

;flags
(def-bit canvas_flag 0
	(bit antialias))

;winding
(def-enum winding 0
	(enum odd_even none_zero))
