(include 'gui/view/class.inc)

(def-class 'canvas 'view)
(dec-method 'vtable 'gui/class_canvas)
(dec-method 'create 'gui/canvas/create 'static '(r0 r1 r2) '(r0))
(dec-method 'create_shared 'gui/canvas/create_shared 'static '(r0) '(r0))
(dec-method 'new 'gui/canvas/new 'static nil '(r0))
(dec-method 'init 'gui/canvas/init 'static '(r0 r1 r2 r3 r4) '(r0 r1))
(dec-method 'init1 'gui/canvas/init1 'static '(r0 r1 r2) '(r0 r1))
(dec-method 'swap 'gui/canvas/swap 'static '(r0) '(r0))
(dec-method 'resize 'gui/canvas/resize 'static '(r0 r1) '(r0))
(dec-method 'resize_2 'gui/canvas/resize_2 'static '(r0 r1) '(r0))
(dec-method 'resize_3 'gui/canvas/resize_3 'static '(r0 r1) '(r0))
(dec-method 'fill 'gui/canvas/fill 'static '(r0 r1) '(r0))
(dec-method 'to_premul 'gui/canvas/to_premul 'static '(r0 r1) '(r0 r1))
(dec-method 'to_argb 'gui/canvas/to_argb 'static '(r0 r1) '(r0 r1))
(dec-method 'as_argb 'gui/canvas/as_argb 'static '(r0 r1) '(r0))
(dec-method 'as_premul 'gui/canvas/as_premul 'static '(r0 r1) '(r0))
(dec-method 'pick 'gui/canvas/pick 'static '(r0 r1 r7 r8) '(r0 r1))
(dec-method 'plot 'gui/canvas/plot 'static '(r0 r1 r2 r7 r8) '(r0))
(dec-method 'fbox 'gui/canvas/fbox 'static '(r0 r1 r2 r7 r8 r9 r10) '(r0))
(dec-method 'fpoly 'gui/canvas/fpoly 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'set_clip 'gui/canvas/set_clip 'static '(r0 r7 r8 r9 r10) '(r0))

(dec-method 'get_pick_noclip 'gui/canvas/get_pick_noclip 'static '(r0 r7 r8) '(r0 r1))
(dec-method 'get_pick 'gui/canvas/get_pick 'static '(r0 r7 r8) '(r0 r1))

(dec-method 'set_plot_noclip 'gui/canvas/set_plot_noclip 'static '(r0 r1 r7 r8) '(r0))
(dec-method 'set_span_noclip 'gui/canvas/set_span_noclip 'static '(r0 r1 r7 r8 r9) '(r0))
(dec-method 'set_span 'gui/canvas/set_span 'static '(r0 r1 r7 r8 r9) '(r0))
(dec-method 'set_plot 'gui/canvas/set_plot 'static '(r0 r1 r7 r8) '(r0))
(dec-method 'set_fbox 'gui/canvas/set_fbox 'static '(r0 r1 r7 r8 r9 r10) '(r0))
(dec-method 'set_fpoly 'gui/canvas/set_fpoly 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))

(dec-method 'blend_plot_noclip 'gui/canvas/blend_plot_noclip 'static '(r0 r1 r7 r8) '(r0))
(dec-method 'blend_span_noclip 'gui/canvas/blend_span_noclip 'static '(r0 r1 r7 r8 r9) '(r0))
(dec-method 'blend_span 'gui/canvas/blend_span 'static '(r0 r1 r7 r8 r9) '(r0))
(dec-method 'blend_plot 'gui/canvas/blend_plot 'static '(r0 r1 r7 r8) '(r0))
(dec-method 'blend_fbox 'gui/canvas/blend_fbox 'static '(r0 r1 r7 r8 r9 r10) '(r0))
(dec-method 'blend_fpoly 'gui/canvas/blend_fpoly 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))

(dec-method 'load 'gui/canvas/load 'static '(r0 r1) '(r0))
(dec-method 'load_cpm 'gui/canvas/load_cpm 'static '(r0) '(r0))

(dec-method 'deinit 'gui/canvas/deinit 'override)
(dec-method 'pref_size 'gui/canvas/pref_size 'override)
(dec-method 'draw 'gui/canvas/draw 'override)

(dec-method 'lisp_create 'gui/canvas/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_load 'gui/canvas/lisp_load 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_swap 'gui/canvas/lisp_swap 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fill 'gui/canvas/lisp_fill 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set_plot 'gui/canvas/lisp_set_plot 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set_fbox 'gui/canvas/lisp_set_fbox 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set_fpoly 'gui/canvas/lisp_set_fpoly 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_blend_plot 'gui/canvas/lisp_blend_plot 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_blend_fbox 'gui/canvas/lisp_blend_fbox 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_blend_fpoly 'gui/canvas/lisp_blend_fpoly 'static '(r0 r1) '(r0 r1))

(def-struct 'canvas 'view)
	(ulong 'texture)
	(ptr 'array)
	(ptr 'edges)
	(ptr 'edge_tracker)
	(puint 'resize_buf)
	(uint 'resize_buf_size)
	(int 'aa_scale)
	(uint 'width)
	(uint 'height)
	(uint 'stride)
	(uint 'cx)
	(uint 'cy)
	(uint 'cx1)
	(uint 'cy1)
(def-struct-end)

(def-struct 'edge)
	(int 'x)
	(int 'ys)
	(int 'ye)
	(int 'w)
	(int 'dda)
	(local-align)
(def-struct-end)

(def-bit 'load_flag)
	(bit 'shared)
(def-bit-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun gui/canvas/get_pick ()
	;inputs
	;r0 = canvas object (ptr)
	;r7 = x (pixels)
	;r8 = y (pixels)
	;outputs
	;r0 = canvas object (ptr)
	;r1 = color (argb)
	;trashes
	;all but r0
	(call 'canvas 'pick `(r0 (@ ,(f-path 'canvas 'get_pick_noclip)) r7 r8) '(r0 r1)))

(defcfun gui/canvas/set_plot ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = color (argb)
	;r7 = x (pixels)
	;r8 = y (pixels)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'plot `(r0 r1 (@ ,(f-path 'canvas 'set_plot_noclip)) r7 r8)))

(defcfun gui/canvas/set_fbox ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = color (argb)
	;r7 = x (pixels)
	;r8 = y (pixels)
	;r9 = w (pixels)
	;r10 = h (pixels)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'fbox `(r0 r1 (@ ,(f-path 'canvas 'set_span_noclip)) r7 r8 r9 r10)))

(defcfun gui/canvas/set_fpoly ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = stack array object (ptr)
	;r2 = color (argb)
	;r3 = x (16.16)
	;r4 = y (16.16)
	;r5 = winding mode (0/1)
	;r6 = vector of points objects (ptr)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'fpoly `(r0 r1 r2 r3 r4 r5 r6 (@ ,(f-path 'canvas 'set_span)))))

(defcfun gui/canvas/blend_plot ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = color (argb)
	;r7 = x (pixels)
	;r8 = y (pixels)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'plot `(r0 r1 (@ ,(f-path 'canvas 'blend_plot_noclip)) r7 r8)))

(defcfun gui/canvas/blend_fbox ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = color (argb)
	;r7 = x (pixels)
	;r8 = y (pixels)
	;r9 = w (pixels)
	;r10 = h (pixels)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'fbox `(r0 r1 (@ ,(f-path 'canvas 'blend_span_noclip)) r7 r8 r9 r10)))

(defcfun gui/canvas/blend_fpoly ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = stack array object (ptr)
	;r2 = color (argb)
	;r3 = x (16.16)
	;r4 = y (16.16)
	;r5 = winding mode (0/1)
	;r6 = vector of points objects (ptr)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(call 'canvas 'fpoly `(r0 r1 r2 r3 r4 r5 r6 (@ ,(f-path 'canvas 'blend_span)))))
