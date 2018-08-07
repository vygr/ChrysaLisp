(import 'class/view/class.inc)

(def-class 'label 'view)
(dec-method 'vtable 'class/class_label)
(dec-method 'create 'class/label/create 'static '() '(r0))
(dec-method 'new 'class/label/new 'static nil '(r0))
(dec-method 'init 'class/label/init)

(dec-method 'pref_size 'class/label/pref_size 'override)
(dec-method 'draw 'class/label/draw 'override)
(dec-method 'layout 'class/label/layout 'override)

(dec-method 'lisp_create 'class/label/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'label 'view)
	(ptr 'flow)
	(ptr 'text)
(def-struct-end)
