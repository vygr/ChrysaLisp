(include "gui/label/class.inc")

(def-class 'button 'label)
(dec-method :vtable 'gui/button/vtable)
(dec-method :create 'gui/button/create)

(dec-method :draw 'gui/button/draw :final)

(dec-method :lisp_create 'gui/button/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'button 'label)
(def-struct-end)
