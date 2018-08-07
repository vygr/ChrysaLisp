(import 'class/string/class.inc)

(def-class 'symbol 'string)
(dec-method 'vtable 'class/class_symbol)
(dec-method 'intern 'class/symbol/intern 'static '(r0) '(r0))
(dec-method 'intern_cstr 'class/symbol/intern_cstr 'static '(r0) '(r0))

(def-struct 'symbol 'string)
(def-struct-end)
