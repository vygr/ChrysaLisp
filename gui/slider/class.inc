(include "gui/view/class.inc")

(def-class 'slider 'view)
(dec-method :draw 'gui/slider/draw :final)
