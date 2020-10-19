(include "gui/view/class.inc")

(def-class 'text 'view)
(dec-method :vtable 'gui/text/vtable)
(dec-method :create 'gui/text/create :static '() '(r0))

(dec-method :pref_size 'gui/text/pref_size :final)
(dec-method :draw 'gui/text/draw :final)

(dec-method :lisp_create 'gui/text/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'text 'view)
(def-struct-end)
