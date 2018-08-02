(import 'class/view/view.inc)

(def-class 'canvas 'view)
(def-method 'create 'class/canvas/create 'static '(r0 r1 r2) '(r0))
(def-method 'create_shared 'class/canvas/create_shared 'static '(r0) '(r0))
(def-method 'new 'class/canvas/new 'static nil '(r0))
(def-method 'init 'class/canvas/init 'static '(r0 r1 r2 r3 r4) '(r0 r1))
(def-method 'init1 'class/canvas/init1 'static '(r0 r1 r2) '(r0 r1))
(def-method 'swap 'class/canvas/swap 'static '(r0) '(r0))
(def-method 'resize 'class/canvas/resize 'static '(r0 r1) '(r0))
(def-method 'fill 'class/canvas/fill 'static '(r0 r1) '(r0))
(def-method 'to_premul 'class/canvas/to_premul 'static '(r0 r1) '(r0 r1))
(def-method 'to_argb 'class/canvas/to_argb 'static '(r0 r1) '(r0 r1))
(def-method 'as_argb 'class/canvas/as_argb 'static '(r0 r1) '(r0))
(def-method 'as_premul 'class/canvas/as_premul 'static '(r0 r1) '(r0))
(def-method 'fbox 'class/canvas/fbox 'static '(r0 r1 r2 r7 r8 r9 r10) '(r0))
(def-method 'fpoly 'class/canvas/fpoly 'static '(r0 r1 r2 r3 r4) '(r0))
(def-method 'set_clip 'class/canvas/set_clip 'static '(r0 r7 r8 r9 r10) '(r0))

(def-method 'set_span_noclip 'class/canvas/set_span_noclip 'static '(r0 r1 r7 r8 r9) '(r0))
(def-method 'set_span 'class/canvas/set_span 'static '(r0 r1 r7 r8 r9) '(r0))
(def-method 'set_fbox 'class/canvas/set_fbox 'static '(r0 r1 r7 r8 r9 r10) '(r0))
(def-method 'set_fpoly 'class/canvas/set_fpoly 'static '(r0 r1 r2 r3) '(r0))

(def-method 'blend_span_noclip 'class/canvas/blend_span_noclip 'static '(r0 r1 r7 r8 r9) '(r0))
(def-method 'blend_span 'class/canvas/blend_span 'static '(r0 r1 r7 r8 r9) '(r0))
(def-method 'blend_fbox 'class/canvas/blend_fbox 'static '(r0 r1 r7 r8 r9 r10) '(r0))
(def-method 'blend_fpoly 'class/canvas/blend_fpoly 'static '(r0 r1 r2 r3) '(r0))

(def-method 'load 'class/canvas/load 'static '(r0) '(r0))
(def-method 'load_cpm 'class/canvas/load_cpm 'static '(r0) '(r0))

(def-method 'deinit 'class/canvas/deinit 'override)
(def-method 'pref_size 'class/canvas/pref_size 'override)
(def-method 'draw 'class/canvas/draw 'override)

(def-method 'lisp_create 'class/canvas/lisp_create 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_load 'class/canvas/lisp_load 'static '(r0 r1) '(r0 r1))

(def-struct 'canvas 'view)
	(ulong 'texture)
	(ptr 'array)
	(ptr 'edges)
	(ptr 'edge_tracker)
	(ptr 'stack)
	(puint 'resize_buf)
	(uint 'resize_buf_size)
	(int 'aa_scale)
	(int 'aa_scale_r)
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
	(int 'dda)
	(int 'w)
	(local-align)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/canvas/set_fbox ()
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
	(f-call 'canvas 'fbox `(r0 r1 (@ ,(f-path 'canvas 'set_span_noclip)) r7 r8 r9 r10)))

(defcfun class/canvas/set_fpoly ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = vector of points objects (ptr)
	;r2 = color (argb)
	;r3 = winding mode (0/1)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(f-call 'canvas 'fpoly `(r0 r1 r2 r3 (@ ,(f-path 'canvas 'set_span)))))

(defcfun class/canvas/blend_fbox ()
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
	(f-call 'canvas 'fbox `(r0 r1 (@ ,(f-path 'canvas 'blend_span_noclip)) r7 r8 r9 r10)))

(defcfun class/canvas/blend_fpoly ()
	;inputs
	;r0 = canvas object (ptr)
	;r1 = vector of points objects (ptr)
	;r2 = color (argb)
	;r3 = winding mode (0/1)
	;outputs
	;r0 = canvas object (ptr)
	;trashes
	;all but r0
	(f-call 'canvas 'fpoly `(r0 r1 r2 r3 (@ ,(f-path 'canvas 'blend_span)))))
