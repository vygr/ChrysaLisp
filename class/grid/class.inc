(import 'class/view/class.inc)

(def-class 'grid 'view)
(dec-method 'vtable 'class/class_grid)
(dec-method 'create 'class/grid/create 'static '() '(r0))
(dec-method 'new 'class/grid/new)

(dec-method 'pref_size 'class/grid/pref_size 'final)
(dec-method 'layout 'class/grid/layout 'final)

(dec-method 'lisp_create 'class/grid/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'grid 'view)
(def-struct-end)
