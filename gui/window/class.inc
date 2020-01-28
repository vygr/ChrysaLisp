(include 'gui/view/class.inc)

(def-class 'window 'view)
(dec-method 'vtable 'gui/window/vtable)
(dec-method 'create 'gui/window/create 'static '(r0) '(r0))
(dec-method 'init 'gui/window/init 'static '(r0 r1 r2))

(dec-method 'add_child 'gui/window/add_child 'final)
(dec-method 'pref_size 'gui/window/pref_size 'final)
(dec-method 'layout 'gui/window/layout 'final)
(dec-method 'draw 'gui/window/draw 'final)
(dec-method 'mouse_down 'gui/window/mouse_down 'final)
(dec-method 'mouse_move 'gui/window/mouse_move 'final)

(dec-method 'lisp_create 'gui/window/lisp_create 'static '(r0 r1) '(r0 r1))

(def-bit 'window_drag)
	(bit 'left 'right 'top 'bottom)
(def-bit-end)

(def-bit 'window_flag)
	(bit 'status 'close 'max 'min)
(def-bit-end)

(def-struct 'window 'view)
	(offset 'start)
	(ptr 'title_panel 'status_panel 'title 'status 'close 'min 'max)
	(int 'offset_x 'offset_y)
	(uint 'drag_mode)
(def-struct-end)
