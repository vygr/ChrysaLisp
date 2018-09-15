(import 'class/label/class.inc)

(def-bit 'slider_state)
	(bit 'pressed)
(def-bit-end)

(def-class 'slider 'view)
(dec-method 'vtable 'class/class_slider)
(dec-method 'create 'class/slider/create 'static '() '(r0))
(dec-method 'new 'class/slider/new)
(dec-method 'init 'class/slider/init)
(dec-method 'connect_value 'class/slider/connect_value 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/slider/deinit 'final)
(dec-method 'pref_size 'class/slider/pref_size 'final)
(dec-method 'draw 'class/slider/draw 'final)
(dec-method 'layout 'class/view/opaque 'final)
(dec-method 'mouse_down 'class/slider/mouse_move 'final)
(dec-method 'mouse_up 'class/slider/mouse_up 'final)
(dec-method 'mouse_move 'class/slider/mouse_move 'final)

(dec-method 'lisp_create 'class/slider/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_connect_value 'class/slider/lisp_connect_value 'static '(r0 r1) '(r0 r1))

(def-struct 'slider 'label)
	(ulong 'state)
	(ptr 'value_targets)
(def-struct-end)
