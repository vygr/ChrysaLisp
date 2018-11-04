(include 'class/string/class.inc)

(def-class 'symbol 'string)
(dec-method 'vtable 'class/class_symbol)
(dec-method 'statics 'class/symbol/statics)
(dec-method 'flush 'class/symbol/flush)
(dec-method 'intern 'class/symbol/intern 'static '(r0) '(r0))
(dec-method 'intern_cstr 'class/symbol/intern_cstr 'static '(r0) '(r0))

(def-struct 'symbol_statics)
	(ptr 'intern)
(def-struct-end)

(def-struct 'symbol 'string)
(def-struct-end)
