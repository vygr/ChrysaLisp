(import 'class/function/function.inc)

(def-class 'integer 'function)
(def-method 'vtable 'class/class_integer)
(def-method 'create 'class/integer/create)

(def-struct 'integer 'function)
(def-struct-end)
