(include "gui/view/class.inc")

(def-class 'window 'view)
(dec-method :vtable 'gui/window/vtable)
(dec-method :create 'gui/window/create :static nil '(r0))

(dec-method :draw 'gui/window/draw :final)

(dec-method :lisp_create 'gui/window/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'window 'view)
(def-struct-end)
