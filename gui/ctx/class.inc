(include "sys/class.inc")

(def-class ctx :nil
	(dec-method :set_color gui/ctx/set_color :static (:r0 :r1))
	(dec-method :box gui/ctx/box :static (:r0 :r7 :r8 :r9 :r10))
	(dec-method :filled_box gui/ctx/filled_box :static (:r0 :r7 :r8 :r9 :r10))
	(dec-method :blit gui/ctx/blit :static (:r0 :r1 :r2 :r7 :r8 :r9 :r10))
	(dec-method :lisp_set_color gui/ctx/lisp_set_color :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_box gui/ctx/lisp_box :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_filled_box gui/ctx/lisp_filled_box :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_blit gui/ctx/lisp_blit :static (:r0 :r1) (:r0 :r1)))
