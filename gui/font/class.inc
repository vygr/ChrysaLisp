(import 'class/obj/class.inc)

(def-class 'font 'obj)
(dec-method 'vtable 'gui/class_font)
(dec-method 'statics 'gui/font/statics)
(dec-method 'open 'gui/font/open 'static '(r0 r1) '(r0))
(dec-method 'create 'gui/font/create 'static '(r0 r1) '(r0))
(dec-method 'new 'gui/font/new 'static nil '(r0))
(dec-method 'init 'gui/font/init 'static '(r0 r1 r2 r3))
(dec-method 'flush_cache 'gui/font/flush_cache 'static '(r0) '(r0))
(dec-method 'ref_word 'gui/font/ref_word 'static '(r0 r1) '(r0 r1))
(dec-method 'ref_chars 'gui/font/ref_chars 'static '(r0) '(r0 r1))
(dec-method 'get_metrics 'gui/font/get_metrics 'static '(r0) '(r0 r1 r2 r3))

(dec-method 'deinit 'gui/font/deinit 'final)

(dec-method 'lisp_create 'gui/font/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'font_statics)
	(ptr 'intern)
	(ulong 'count)
(def-struct-end)

(def-struct 'font 'obj)
	(ulong 'handle)
	(ptr 'word_map)
	(ptr 'name)
	(ptr 'chars)
	(uint 'points)
	(int 'ascent)
	(int 'descent)
	(int 'height)
(def-struct-end)

(defcvar 'font_max_word_cache 1024)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun gui/font/get_metrics ()
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
