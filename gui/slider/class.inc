(include "gui/view/class.inc")

(def-class 'slider 'view)
(dec-method :vtable 'gui/slider/vtable)
(dec-method :create 'gui/slider/create :static '() '(r0))

(dec-method :draw 'gui/slider/draw :final)

(dec-method :lisp_create 'gui/slider/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'slider 'view)
(def-struct-end)
