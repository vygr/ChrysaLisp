(import 'class/function/class.inc)

(def-class 'integer 'function)
(dec-method 'vtable 'class/class_integer)
(dec-method 'create 'class/integer/create)

(def-struct 'integer 'function)
(def-struct-end)
