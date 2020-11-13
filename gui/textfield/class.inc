(include "gui/label/class.inc")

(def-class 'textfield 'label)
(dec-method :draw 'gui/textfield/draw :override)
