(import 'class/ref/ref.inc)

(def-class 'font 'ref)
(def-method 'open 'class/font/open 'static '(r0 r1) '(r0))
(def-method 'create 'class/font/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/font/new 'static nil '(r0))
(def-method 'init 'class/font/init 'static '(r0 r1 r2 r3))
(def-method 'get_word 'class/font/get_word 'static '(r0 r1) '(r0 r1 r2 r3))
(def-method 'get_metrics 'class/font/get_metrics 'static '(r0) '(r0 r1 r2 r3))

(def-method 'deinit 'class/font/deinit 'final)

(def-struct 'font 'ref)
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
	;r0 = font object
	;outputs
	;r0 = font object
	;r1 = ascent
	;r2 = descent
	;r3 = height
	(vp-cpy-ir-i r0 font_ascent r1)
	(vp-cpy-ir-i r0 font_descent r2)
	(vp-cpy-ir-i r0 font_height r3))
