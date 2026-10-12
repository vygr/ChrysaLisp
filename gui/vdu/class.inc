(include "././view/class.inc")
(include "./struct.inc")

(def-class :vdu :view
	(dec-method :vtable gui/vdu/vtable)
	(dec-method :create gui/vdu/create :static () (:r0) (:r0-:r5 :f0-:f15))
	(dec-method :init gui/vdu/init :static :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :deinit gui/vdu/deinit :final :nil :nil (:r1-:r4 :f0-:f15) (:r1-:r4 :f0-:f15))
	(dec-method :draw gui/vdu/draw :final :nil :nil (:r1-:r14 :f0-:f15) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_create gui/vdu/lisp_create :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :f0-:f15))
	(dec-method :lisp_load gui/vdu/lisp_load :static (:r0 :r1) (:r0 :r1) (:r1-:r8))
	(dec-method :lisp_configure gui/vdu/lisp_configure :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))
