(import 'class/string/string.inc)

(def-class 'symbol 'string)
(def-method 'create_from_buffer 'class/symbol/create_from_buffer 'static '(r0 r1) '(r0))
(def-method 'create_from_cstr 'class/symbol/create_from_cstr 'static '(r0) '(r0))
(def-method 'create_from_file 'class/symbol/create_from_file 'static '(r0) '(r0))
(def-method 'create_from_long 'class/symbol/create_from_long 'static '(r0 r1) '(r0))
(def-method 'append 'class/symbol/append 'static '(r0 r1) '(r0))

(def-struct 'symbol 'string)
(def-struct-end)
