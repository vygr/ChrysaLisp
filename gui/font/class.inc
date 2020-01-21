(include 'class/obj/class.inc)

(def-class 'font 'obj)
(dec-method 'vtable 'gui/font/vtable)
(dec-method 'statics 'gui/font/statics)
(dec-method 'open 'gui/font/open 'static '(r0 r1) '(r0))
(dec-method 'create 'gui/font/create 'static '(r0 r1 r2) '(r0))
(dec-method 'init 'gui/font/init 'static '(r0 r1 r2 r3 r4))
(dec-method 'flush 'gui/font/flush)
(dec-method 'ref_word 'gui/font/ref_word 'static '(r0 r1) '(r0 r1))
(dec-method 'ref_chars 'gui/font/ref_chars 'static '(r0) '(r0 r1))
(dec-method 'get_metrics 'gui/font/get_metrics 'static '(r0) '(r0 r1 r2 r3))
(dec-method 'glyph_data 'gui/font/glyph_data 'static '(r0 r1) '(r0 r1))
(dec-method 'glyph_info 'gui/font/glyph_info 'static '(r0 r1) '(r0 r1))
(dec-method 'glyph_paths 'gui/font/glyph_paths 'static '(r0 r1 r2) '(r0 r1 r2 r3))

(dec-method 'deinit 'gui/font/deinit 'final)

(dec-method 'lisp_create 'gui/font/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_glyph_paths 'gui/font/lisp_glyph_paths 'static '(r0 r1) '(r0 r1))

(def-struct 'font_statics)
	(ptr 'intern)
	(ulong 'count)
(def-struct-end)

(def-struct 'font 'obj)
	(ptr 'data 'word_map 'name 'chars)
	(uint 'pixels)
(def-struct-end)

(def-struct 'font_data)
	(int 'ascent 'descent)
(def-struct-end)

(def-struct 'font_page)
	(uint 'end 'start)
(def-struct-end)

(def-struct 'font_path)
	(uint 'width 'len)
(def-struct-end)

(def-struct 'font_path_element)
	(uint 'type)
(def-struct-end)

(def-struct 'font_line_element 'font_path_element)
	(int 'x 'y)
(def-struct-end)

(def-struct 'font_curve_element 'font_line_element)
	(int 'x1 'y1 'x2 'y2)
(def-struct-end)

(defcvar 'font_max_word_cache 1024)
