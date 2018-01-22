(import 'class/view/view.inc)

(def-class 'label 'view)
(def-method 'create 'class/label/create 'static '() '(r0))
(def-method 'new 'class/label/new 'static nil '(r0))
(def-method 'init 'class/label/init)

(def-method 'pref_size 'class/label/pref_size 'override)
(def-method 'draw 'class/label/draw 'override)
(def-method 'layout 'class/label/layout 'override)

(def-method 'lisp_create 'class/label/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'label 'view)
	(ptr 'flow)
	(ptr 'text)
(def-struct-end)
