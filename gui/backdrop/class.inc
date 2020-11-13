(include "gui/view/class.inc")

(def-class 'backdrop 'view)
(dec-method :draw 'gui/backdrop/draw :final)
