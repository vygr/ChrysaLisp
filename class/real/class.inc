(include 'class/num/class.inc)

(def-class 'real 'num)
(dec-method 'vtable 'class/real/vtable)
(dec-method 'create 'class/real/create 'static '(r0) '(r0))

(dec-method 'print 'class/real/print 'override)

(dec-method 'add 'class/real/add 'override)
(dec-method 'sub 'class/real/sub 'override)
(dec-method 'mul 'class/real/mul 'override)
(dec-method 'div 'class/real/div 'override)
;(dec-method 'mod 'class/real/mod 'override)
;(dec-method 'min 'class/real/min 'override)
;(dec-method 'max 'class/real/max 'override)

(dec-method 'lt 'class/real/lt 'override)
(dec-method 'gt 'class/real/gt 'override)
(dec-method 'le 'class/real/le 'override)
(dec-method 'ge 'class/real/ge 'override)

(dec-method 'lisp_i2r 'class/real/lisp_i2r 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_f2r 'class/real/lisp_f2r 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_r2i 'class/real/lisp_r2i 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_r2f 'class/real/lisp_r2f 'static '(r0 r1) '(r0 r1))

(def-struct 'real 'num)
(def-struct-end)
