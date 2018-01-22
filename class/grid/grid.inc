(import 'class/view/view.inc)

(def-class 'grid 'view)
(def-method 'create 'class/grid/create 'static '() '(r0))
(def-method 'new 'class/grid/new)

(def-method 'pref_size 'class/grid/pref_size 'final)
(def-method 'layout 'class/grid/layout 'final)

(def-method 'lisp_create 'class/grid/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'grid 'view)
(def-struct-end)
