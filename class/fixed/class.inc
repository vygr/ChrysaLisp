(include 'class/num/class.inc)

(def-class 'fixed 'num)
(dec-method 'vtable 'class/fixed/vtable)
(dec-method 'create 'class/fixed/create 'static '(r0) '(r0))

(dec-method 'print 'class/fixed/print 'override)
(dec-method 'vcreate 'class/fixed/create 'override)

(dec-method 'mul 'class/fixed/mul 'override)
(dec-method 'div 'class/fixed/div 'override)
(dec-method 'mod 'class/fixed/mod 'override)
(dec-method 'sqrt 'class/fixed/sqrt 'override)

(dec-method 'sin 'class/fixed/sin 'virtual '(r0) '(r0 r1))
(dec-method 'cos 'class/fixed/cos 'virtual '(r0) '(r0 r1))
(dec-method 'frac 'class/fixed/frac 'virtual '(r0) '(r0 r1))
(dec-method 'floor 'class/fixed/floor 'virtual '(r0) '(r0 r1))
(dec-method 'recip 'class/fixed/recip 'virtual '(r0) '(r0 r1))

(dec-method 'lisp_sin 'class/fixed/lisp_sin 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_cos 'class/fixed/lisp_cos 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_frac 'class/fixed/lisp_frac 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_floor 'class/fixed/lisp_floor 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_recip 'class/fixed/lisp_recip 'static '(r0 r1) '(r0 r1))

(def-struct 'fixed 'num)
(def-struct-end)
