(include 'gui/label/class.inc)

(def-class 'title 'label)
(dec-method :vtable 'gui/title/vtable)
(dec-method :create 'gui/title/create)

(dec-method :lisp_create 'gui/title/lisp_create :static '(r0 r1) '(r0 r1))

(dec-method :mouse_down 'gui/title/mouse_down :final)
(dec-method :mouse_move 'gui/title/mouse_move :final)

(def-struct 'title 'label)
	(int 'last_x 'last_y)
(def-struct-end)
