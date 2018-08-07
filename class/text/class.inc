(import 'class/view/class.inc)

(def-class 'text 'view)
(dec-method 'vtable 'class/class_text)
(dec-method 'create 'class/text/create 'static '() '(r0))
(dec-method 'new 'class/text/new 'static nil '(r0))
(dec-method 'init 'class/text/init)
(dec-method 'switch_text 'class/text/switch_text 'static '(r0) '(r0))

(dec-method 'deinit 'class/text/deinit 'final)
(dec-method 'pref_size 'class/text/pref_size 'final)
(dec-method 'draw 'class/text/draw 'final)

(def-struct 'text 'view)
	(ptr 'string)
	(ptr 'words)
(def-struct-end)
