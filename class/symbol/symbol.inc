(import 'class/string/string.inc)

(def-class 'symbol 'string)
(def-method 'intern 'class/symbol/intern 'static '(r0) '(r0))
(def-method 'intern_cstr 'class/symbol/intern_cstr 'static '(r0) '(r0))

(def-struct 'symbol 'string)
(def-struct-end)
