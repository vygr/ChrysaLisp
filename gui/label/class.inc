(include "gui/view/class.inc")

(def-class 'label 'view)
(dec-method :draw 'gui/label/draw :override)
