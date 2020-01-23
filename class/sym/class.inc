(include 'class/str/class.inc)

(def-class 'sym 'str)
(dec-method 'vtable 'class/sym/vtable)
(dec-method 'statics 'class/sym/statics)
(dec-method 'statics_init 'class/sym/statics_init)
(dec-method 'get_static_sym 'class/sym/get_static_sym 'static '(r1) '(r1))
(dec-method 'flush 'class/sym/flush)
(dec-method 'intern 'class/sym/intern 'static '(r0) '(r0))
(dec-method 'intern_str 'class/sym/intern_str 'static '(r0) '(r0))
(dec-method 'intern_cstr 'class/sym/intern_cstr 'static '(r0) '(r0))
(dec-method 'intern_strs 'class/sym/intern_strs 'static '(r1) '(r1))

(dec-method 'lisp_sym 'class/sym/lisp_sym 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gensym 'class/sym/lisp_gensym 'static '(r0 r1) '(r0 r1))

(def-struct 'sym_statics)
	(ptr 'intern 'syms)
(def-struct-end)

(def-struct 'sym 'str)
(def-struct-end)

(defcvar 'num_sym_buckets 199)

;;;;;;;;;;;;;;;;
; static symbols
;;;;;;;;;;;;;;;;

(defcvar '*static_syms* (list))
(defun def-static-sym (_)
	(defcvar (sym (cat "static_sym_" _)) (length *static_syms*))
	(push *static_syms* _))

(def-static-sym "font")
(def-static-sym "text")
(def-static-sym "color")
(def-static-sym "border")
(def-static-sym "value")
(def-static-sym "maximum")
(def-static-sym "minimum")
(def-static-sym "portion")
(def-static-sym "min_width")
(def-static-sym "min_height")
(def-static-sym "ink_color")
(def-static-sym "flow_flags")
(def-static-sym "grid_width")
(def-static-sym "grid_height")
(def-static-sym "vdu_width")
(def-static-sym "vdu_height")
