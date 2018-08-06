(import 'class/view/view.inc)

(def-class 'text 'view)
(def-method 'vtable 'class/class_text)
(def-method 'create 'class/text/create 'static '() '(r0))
(def-method 'new 'class/text/new 'static nil '(r0))
(def-method 'init 'class/text/init)
(def-method 'switch_text 'class/text/switch_text 'static '(r0) '(r0))

(def-method 'deinit 'class/text/deinit 'final)
(def-method 'pref_size 'class/text/pref_size 'final)
(def-method 'draw 'class/text/draw 'final)

(def-struct 'text 'view)
	(ptr 'string)
	(ptr 'words)
(def-struct-end)
