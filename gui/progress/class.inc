(include 'gui/view/class.inc)

(def-class 'progress 'view)
(dec-method 'vtable 'gui/progress/vtable)
(dec-method 'create 'gui/progress/create 'static '() '(r0))
(dec-method 'new 'gui/progress/new 'static nil '(r0))

(dec-method 'pref_size 'gui/progress/pref_size 'final)
(dec-method 'draw 'gui/progress/draw 'final)
(dec-method 'layout 'gui/view/opaque 'final)

(dec-method 'lisp_create 'gui/progress/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'progress 'view)
(def-struct-end)
