(include "gui/label/class.inc")

(def-class 'textfield 'label)
(dec-method :vtable 'gui/textfield/vtable)
(dec-method :create 'gui/textfield/create)
(dec-method :init 'gui/textfield/init)

(dec-method :draw 'gui/textfield/draw :override)

(dec-method :key_down 'gui/textfield/key_down :final)

(dec-method :lisp_create 'gui/textfield/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'textfield 'label)
	(uint 'cursor)
(def-struct-end)
