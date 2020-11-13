(include "gui/view/class.inc")

(def-class 'window 'view)
(dec-method :draw 'gui/window/draw :final)
