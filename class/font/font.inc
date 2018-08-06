(import 'class/obj/obj.inc)

(def-class 'font 'obj)
(def-method 'vtable 'class/class_font)
(def-method 'open 'class/font/open 'static '(r0 r1) '(r0))
(def-method 'create 'class/font/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/font/new 'static nil '(r0))
(def-method 'init 'class/font/init 'static '(r0 r1 r2 r3))
(def-method 'ref_word 'class/font/ref_word 'static '(r0 r1) '(r0 r1))
(def-method 'get_metrics 'class/font/get_metrics 'static '(r0) '(r0 r1 r2 r3))

(def-method 'deinit 'class/font/deinit 'final)

(def-method 'lisp_create 'class/font/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'font 'obj)
	(ulong 'handle)
	(ptr 'word_map)
	(ptr 'name)
	(uint 'points)
	(int 'ascent)
	(int 'descent)
	(int 'height)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/font/get_metrics ()
	;inputs
	;r0 = font object (ptr)
	;outputs
	;r0 = font object (ptr)
	;r1 = ascent (pixels)
	;r2 = descent (pixels)
	;r3 = height (pixels)
	(vp-cpy-ir-i r0 font_ascent r1)
	(vp-cpy-ir-i r0 font_descent r2)
	(vp-cpy-ir-i r0 font_height r3))
