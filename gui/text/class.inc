(include 'gui/view/class.inc)

(def-class 'text 'view)
(dec-method 'vtable 'gui/text/vtable)
(dec-method 'create 'gui/text/create 'static '() '(r0))
(dec-method 'init 'gui/text/init)
(dec-method 'switch_text 'gui/text/switch_text 'static '(r0) '(r0))

(dec-method 'deinit 'gui/text/deinit 'final)
(dec-method 'pref_size 'gui/text/pref_size 'final)
(dec-method 'draw 'gui/text/draw 'final)

(def-struct 'text 'view)
	(ptr 'string)
	(ptr 'words)
(def-struct-end)
