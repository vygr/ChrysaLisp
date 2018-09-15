(import 'class/label/class.inc)

(def-bit 'button_state)
	(bit 'pressed)
(def-bit-end)

(def-class 'button 'label)
(dec-method 'vtable 'class/class_button)
(dec-method 'create 'class/button/create)
(dec-method 'new 'class/button/new)
(dec-method 'init 'class/button/init)
(dec-method 'connect_click 'class/button/connect_click 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/button/deinit 'final)
(dec-method 'draw 'class/button/draw 'final)
(dec-method 'layout 'class/button/layout 'final)
(dec-method 'mouse_down 'class/button/mouse_down 'final)
(dec-method 'mouse_up 'class/button/mouse_up 'final)
(dec-method 'mouse_move 'class/button/mouse_move 'final)

(dec-method 'lisp_create 'class/button/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_connect_click 'class/button/lisp_connect_click 'static '(r0 r1) '(r0 r1))

(def-struct 'button 'label)
	(ulong 'state)
	(ptr 'click_targets)
(def-struct-end)
