(include "lib/asm/class.inc")

(def-class :ctx :nil
	(dec-method :reserve gui/ctx/reserve :static (:r0 :r1) (:r0 :r1) (:r1-:r3 :f0-:f15))
	(dec-method :set_color gui/ctx/set_color :static (:r0 :r1) :nil (:r1-:r4 :f0-:f15))
	(dec-method :box gui/ctx/box :static (:r0 :r7 :r8 :r9 :r10) :nil (:r1-:r3 :f0-:f15))
	(dec-method :filled_box gui/ctx/filled_box :static (:r0 :r7 :r8 :r9 :r10) :nil (:r1-:r3 :f0-:f15))
	(dec-method :blit gui/ctx/blit :static (:r0 :r1 :r2 :r7 :r8 :r9 :r10 :r11 :r12) :nil (:r1-:r5 :r7-:r8 :f0-:f15))
	(dec-method :lisp_set_color gui/ctx/lisp_set_color :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :f0-:f15))
	(dec-method :lisp_box gui/ctx/lisp_box :static (:r0 :r1) (:r0 :r1) (:r1-:r10 :f0-:f15))
	(dec-method :lisp_filled_box gui/ctx/lisp_filled_box :static (:r0 :r1) (:r0 :r1) (:r1-:r10 :f0-:f15))
	(dec-method :lisp_blit gui/ctx/lisp_blit :static (:r0 :r1) (:r0 :r1) (:r1-:r12 :f0-:f15))
	(dec-method :lisp_filled_region gui/ctx/lisp_filled_region :static (:r0 :r1) (:r0 :r1) (:r1-:r10 :f0-:f15)))

;what a view draws, kept in its list of draws, gui/ctx/class.vp, and put on
;the host by the GUI, service/gui/class.vp. Each is the op and its longs,
;a rect is a host_gui_rect of two longs, the one drawn to first:
;color argb, box rect, filled_box rect, blit rect texture argb srect
(enums +ctx_op 0
	(enum color box filled_box blit))
