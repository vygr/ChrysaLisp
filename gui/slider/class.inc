(import 'gui/view/class.inc)

(def-bit 'slider_state)
	(bit 'pressed)
(def-bit-end)

(def-class 'slider 'view)
(dec-method 'vtable 'gui/class_slider)
(dec-method 'create 'gui/slider/create 'static '() '(r0))
(dec-method 'new 'gui/slider/new)
(dec-method 'init 'gui/slider/init)
(dec-method 'connect_value 'gui/slider/connect_value 'static '(r0 r1) '(r0))

(dec-method 'deinit 'gui/slider/deinit 'final)
(dec-method 'pref_size 'gui/slider/pref_size 'final)
(dec-method 'draw 'gui/slider/draw 'final)
(dec-method 'layout 'gui/view/opaque 'final)
(dec-method 'mouse_down 'gui/slider/mouse_move 'final)
(dec-method 'mouse_up 'gui/slider/mouse_up 'final)
(dec-method 'mouse_move 'gui/slider/mouse_move 'final)

(dec-method 'lisp_create 'gui/slider/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_connect_value 'gui/slider/lisp_connect_value 'static '(r0 r1) '(r0 r1))

(def-struct 'slider 'view)
	(ulong 'state)
	(ptr 'value_targets)
(def-struct-end)
