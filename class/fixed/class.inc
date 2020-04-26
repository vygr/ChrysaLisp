(include 'class/num/class.inc)

(def-class 'fixed 'num)
(dec-method 'vtable 'class/fixed/vtable)
(dec-method 'create 'class/fixed/create 'static '(r0) '(r0))

(dec-method 'print 'class/fixed/print 'override)
(dec-method 'vcreate 'class/fixed/create 'override)

(dec-method 'mul 'class/fixed/mul 'override)
(dec-method 'div 'class/fixed/div 'override)
(dec-method 'mod 'class/fixed/mod 'override)

(def-struct 'fixed 'num)
(def-struct-end)
