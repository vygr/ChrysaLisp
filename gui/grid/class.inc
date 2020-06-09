(include 'gui/view/class.inc)

(def-class 'grid 'view)
(dec-method 'vtable 'gui/grid/vtable)
(dec-method 'create 'gui/grid/create :static '() '(r0))

(dec-method 'pref_size 'gui/grid/pref_size :final)
(dec-method 'layout 'gui/grid/layout :final)

(dec-method 'lisp_create 'gui/grid/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'grid 'view)
(def-struct-end)
