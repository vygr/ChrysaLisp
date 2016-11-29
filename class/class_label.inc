(import 'class/class_view.inc)

(defcvar 'label_border_size 0)

(def-class 'label 'view)
(def-method 'create 'class/label/create)
(def-method 'new 'class/label/new)
(def-method 'init 'class/label/init)
(def-method 'set_font 'class/label/set_font 'static '(r0 r1 r2))
(def-method 'set_text 'class/label/set_text 'static '(r0 r1))
(def-method 'get_text 'class/label/get_text 'static '(r0) '(r1))
(def-method 'set_text_color 'class/label/set_text_color 'static '(r0 r1))
(def-method 'set_flow_flags 'class/label/set_flow_flags 'static '(r0 r1))

(def-method 'pref_size 'class/label/pref_size 'override)
(def-method 'draw 'class/label/draw 'override)
(def-method 'layout 'class/label/layout 'override)

(def-struct 'label 'view)
	(ptr 'label_flow)
	(ptr 'label_text)
(def-struct-end)
