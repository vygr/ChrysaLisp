(import 'class/view/view.inc)

(def-class 'label 'view)
(def-method 'create 'class/label/create 'static '() '(r0))
(def-method 'new 'class/label/new 'static nil '(r0))
(def-method 'init 'class/label/init)
(def-method 'set_text 'class/label/set_text 'static '(r0 r1) '(r0))
(def-method 'get_text 'class/label/get_text 'static '(r0) '(r0 r1))
(def-method 'ref_text 'class/label/ref_text 'static '(r0) '(r0 r1))

(def-method 'pref_size 'class/label/pref_size 'override)
(def-method 'draw 'class/label/draw 'override)
(def-method 'layout 'class/label/layout 'override)

(def-struct 'label 'view)
	(ptr 'flow)
	(ptr 'text)
(def-struct-end)
