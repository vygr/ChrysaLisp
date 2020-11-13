(include "gui/label/class.inc")

(def-class 'button 'label)
(dec-method :draw 'gui/button/draw :final)
