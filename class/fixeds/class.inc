(include 'class/nums/class.inc)

(def-class 'fixeds 'nums)
(dec-method 'vtable 'class/fixeds/vtable)
(dec-method 'create 'class/fixeds/create)

(dec-method 'vcreate 'class/fixeds/create :override)
(dec-method 'velement 'class/fixed/create :override)

(dec-method 'mul 'class/fixeds/mul :override)
(dec-method 'div 'class/fixeds/div :override)
(dec-method 'mod 'class/fixeds/mod :override)
(dec-method 'scale 'class/fixeds/scale :override)

(dec-method 'frac 'class/fixeds/frac :virtual '(r0 r1) '(r0))
(dec-method 'floor 'class/fixeds/floor :virtual '(r0 r1) '(r0))

(dec-method 'lisp_frac 'class/fixeds/lisp_frac :static '(r0 r1) '(r0 r1))
(dec-method 'lisp_floor 'class/fixeds/lisp_floor :static '(r0 r1) '(r0 r1))

(def-struct 'fixeds 'nums)
(def-struct-end)
