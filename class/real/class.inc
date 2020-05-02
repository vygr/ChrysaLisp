(include 'class/fixed/class.inc)

(def-class 'real 'fixed)
(dec-method 'vtable 'class/real/vtable)
(dec-method 'create 'class/real/create 'static '(r0) '(r0))

(dec-method 'print 'class/real/print 'override)
(dec-method 'vcreate 'class/real/create 'override)

(dec-method 'add 'class/real/add 'override)
(dec-method 'sub 'class/real/sub 'override)
(dec-method 'mul 'class/real/mul 'override)
(dec-method 'div 'class/real/div 'override)
(dec-method 'mod 'class/real/mod 'override)
(dec-method 'min 'class/real/min 'override)
(dec-method 'max 'class/real/max 'override)
(dec-method 'sqrt 'class/real/sqrt 'override)
(dec-method 'frac 'class/real/frac 'override)
(dec-method 'floor 'class/real/floor 'override)
(dec-method 'recip 'class/real/recip 'override)

(dec-method 'lt 'class/real/lt 'override)
(dec-method 'gt 'class/real/gt 'override)
(dec-method 'le 'class/real/le 'override)
(dec-method 'ge 'class/real/ge 'override)

(def-struct 'real 'fixed)
(def-struct-end)
