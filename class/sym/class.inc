(include 'class/str/class.inc)

(def-class 'sym 'str)
(dec-method 'vtable 'class/sym/vtable)
(dec-method 'statics 'class/sym/statics)
(dec-method 'flush 'class/sym/flush)
(dec-method 'intern 'class/sym/intern 'static '(r0) '(r0))
(dec-method 'intern_cstr 'class/sym/intern_cstr 'static '(r0) '(r0))

(dec-method 'lisp_sym 'class/sym/lisp_sym 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gensym 'class/sym/lisp_gensym 'static '(r0 r1) '(r0 r1))

(def-struct 'sym_statics)
	(ptr 'intern)
(def-struct-end)

(def-struct 'sym 'str)
(def-struct-end)
