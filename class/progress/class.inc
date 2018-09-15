(import 'class/view/class.inc)

(def-class 'progress 'view)
(dec-method 'vtable 'class/class_progress)
(dec-method 'create 'class/progress/create 'static '() '(r0))
(dec-method 'new 'class/progress/new 'static nil '(r0))

(dec-method 'pref_size 'class/progress/pref_size 'final)
(dec-method 'draw 'class/progress/draw 'final)
(dec-method 'layout 'class/view/opaque 'final)

(dec-method 'lisp_create 'class/progress/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'progress 'view)
(def-struct-end)
