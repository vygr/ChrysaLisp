(include 'class/num/class.inc)

(def-class 'func 'num)
(dec-method 'vtable 'class/func/vtable)
(dec-method 'create 'class/func/create 'static '(r0) '(r0))

(def-struct 'func 'num)
(def-struct-end)
