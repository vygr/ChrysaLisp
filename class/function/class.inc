(include 'class/integer/class.inc)

(def-class 'function 'integer)
(dec-method 'vtable 'class/function/vtable)
(dec-method 'create 'class/function/create 'static '(r0) '(r0))

(def-struct 'function 'obj)
(def-struct-end)
