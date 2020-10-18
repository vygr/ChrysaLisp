(include "gui/view/class.inc")

(def-class 'text 'view)
(dec-method :vtable 'gui/text/vtable)
(dec-method :create 'gui/text/create :static '() '(r0))

(dec-method :pref_size 'gui/text/pref_size :final)
(dec-method :draw 'gui/text/draw :final)

(def-struct 'text 'view)
(def-struct-end)
