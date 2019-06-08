(include 'class/string/class.inc)

(def-class 'symbol 'string)
(dec-method 'vtable 'class/symbol/vtable)
(dec-method 'statics 'class/symbol/statics)
(dec-method 'flush 'class/symbol/flush)
(dec-method 'intern 'class/symbol/intern 'static '(r0) '(r0))
(dec-method 'intern_cstr 'class/symbol/intern_cstr 'static '(r0) '(r0))

(dec-method 'lisp_sym 'class/symbol/lisp_sym 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gensym 'class/symbol/lisp_gensym 'static '(r0 r1) '(r0 r1))

(def-struct 'symbol_statics)
	(ptr 'intern)
(def-struct-end)

(def-struct 'symbol 'string)
(def-struct-end)
