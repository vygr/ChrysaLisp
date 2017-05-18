(import 'class/view/view.inc)

(def-class 'canvas 'view)
(def-method 'create 'class/canvas/create 'static '(r0 r1 r2) '(r0))
(def-method 'new 'class/canvas/new 'static nil '(r0))
(def-method 'init 'class/canvas/init 'static '(r0 r1 r2 r3 r5) '(r0 r1))
(def-method 'swap 'class/canvas/swap 'static '(r0) '(r0))
(def-method 'resize 'class/canvas/resize 'static '(r0 r1) '(r0))
(def-method 'fill 'class/canvas/fill 'static '(r0 r1) '(r0))
(def-method 'norm 'class/canvas/norm 'static '(r0) '(r0))
(def-method 'hline_noclip 'class/canvas/hline_noclip 'static '(r0 r1 r8 r9 r10) '(r0))
(def-method 'hline 'class/canvas/hline 'static '(r0 r1 r8 r9 r10) '(r0))
(def-method 'fbox 'class/canvas/fbox 'static '(r0 r1 r2 r8 r9 r10 r11) '(r0))
(def-method 'fpoly 'class/canvas/fpoly 'static '(r0 r1 r2 r3 r5) '(r0))
(def-method 'set_clip 'class/canvas/set_clip 'static '(r0 r8 r9 r10 r11) '(r0))
(def-method 'set_fbox 'class/canvas/set_fbox 'static '(r0 r1 r8 r9 r10 r11) '(r0))
(def-method 'set_fpoly 'class/canvas/set_fpoly 'static '(r0 r1 r2 r3) '(r0))

(def-method 'deinit 'class/canvas/deinit 'override)
(def-method 'pref_size 'class/canvas/pref_size 'override)
(def-method 'draw 'class/canvas/draw 'override)

(def-struct 'canvas 'view)
	(ulong 'texture)
	(ptr 'data)
	(ptr 'edges)
	(ptr 'edge_tracker)
	(ptr 'intervals)
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
	;r0 = canvas object
	;r1 = colour
	;r8 = x
	;r9 = y
	;r10 = w
	;r11 = h
	;outputs
	;r0 = canvas object
	;trashes
	;all but r0, r4
	(f-call 'canvas 'fbox `(r0 r1 (@ ,(f-path 'canvas 'hline_noclip)) r8 r9 r10 r11)))

(defcfun class/canvas/set_fpoly ()
	;inputs
	;r0 = canvas object
	;r1 = vector of polygon arrays
	;r2 = fill color
	;r3 = fill mode
	;outputs
	;r0 = canvas object
	;trashes
	;all but r0, r4
	(f-call 'canvas 'fpoly `(r0 r1 r2 r3 (@ ,(f-path 'canvas 'hline)))))
