(import 'class/integer/class.inc)

(def-class 'function 'integer)
(dec-method 'vtable 'class/class_function)
(dec-method 'create 'class/function/create 'static '(r0) '(r0))

(def-struct 'function 'obj)
(def-struct-end)
