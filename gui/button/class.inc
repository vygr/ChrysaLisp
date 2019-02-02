(include 'gui/label/class.inc)

(def-class 'button 'label)
(dec-method 'vtable 'gui/button/vtable)
(dec-method 'create 'gui/button/create)
(dec-method 'new 'gui/button/new)
(dec-method 'init 'gui/button/init)
(dec-method 'connect_click 'gui/button/connect_click 'static '(r0 r1) '(r0))

(dec-method 'deinit 'gui/button/deinit 'final)
(dec-method 'draw 'gui/button/draw 'final)
(dec-method 'layout 'gui/button/layout 'final)
(dec-method 'mouse_down 'gui/button/mouse_move 'final)
(dec-method 'mouse_up 'gui/button/mouse_up 'final)
(dec-method 'mouse_move 'gui/button/mouse_move 'final)

(dec-method 'lisp_create 'gui/button/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_connect_click 'gui/button/lisp_connect_click 'static '(r0 r1) '(r0 r1))

(def-bit 'button_state)
	(bit 'pressed)
(def-bit-end)

(def-struct 'button 'label)
	(ulong 'state)
	(ptr 'click_targets)
(def-struct-end)
