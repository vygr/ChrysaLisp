(import 'gui/view/class.inc)

(def-class 'label 'view)
(dec-method 'vtable 'gui/class_label)
(dec-method 'create 'gui/label/create 'static '() '(r0))
(dec-method 'new 'gui/label/new 'static nil '(r0))
(dec-method 'init 'gui/label/init)

(dec-method 'pref_size 'gui/label/pref_size 'override)
(dec-method 'draw 'gui/label/draw 'override)
(dec-method 'layout 'gui/label/layout 'override)

(dec-method 'lisp_create 'gui/label/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'label 'view)
	(ptr 'flow)
	(ptr 'text)
(def-struct-end)
