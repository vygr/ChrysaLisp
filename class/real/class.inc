(include 'class/num/class.inc)

(def-class 'real 'obj)
(dec-method 'vtable 'class/real/vtable)
(dec-method 'create 'class/real/create 'static '(r0) '(r0))
(dec-method 'init 'class/num/init 'static '(r0 r1 r2))

(dec-method 'lisp_add 'class/real/lisp_add 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_sub 'class/real/lisp_sub 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_mul 'class/real/lisp_mul 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_div 'class/real/lisp_div 'static '(r0 r1) '(r0 r1))

(dec-method 'lisp_eq 'class/real/lisp_eq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_ne 'class/real/lisp_ne 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_lt 'class/real/lisp_lt 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gt 'class/real/lisp_gt 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_le 'class/real/lisp_le 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_ge 'class/real/lisp_ge 'static '(r0 r1) '(r0 r1))

(dec-method 'lisp_i2r 'class/real/lisp_i2r 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_f2r 'class/real/lisp_f2r 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_r2i 'class/real/lisp_r2i 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_r2f 'class/real/lisp_r2f 'static '(r0 r1) '(r0 r1))

(def-struct 'real 'num)
(def-struct-end)
