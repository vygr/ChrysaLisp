(include "././list/class.inc")
(include "./struct.inc")

(def-class :pset :list
	(dec-method :vtable class/pset/vtable)
	(dec-method :type class/pset/type :override :nil :nil (:r1-:r5 :f0-:f15))
	;a set is itself and no other, whatever the two hold, class/hset/class.inc
	(dec-method :eql class/obj/eql :final :nil :nil (:r1))
	(dec-method :create class/pset/create :static :nil :nil (:r0-:r2 :f0-:f15))
	(dec-method :vcreate class/pset/create :override :nil :nil (:r0-:r2 :f0-:f15))
	(dec-method :pfind class/pset/find :virtual (:r0 :r1) (:r0 :r1 :r7 :r8) (:r1-:r10))
	(dec-method :pinsert class/pset/insert :virtual (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_pset class/pset/lisp_pset :static (:r0 :r1) (:r0 :r1) (:r2))
	(dec-method :lisp_pfindi class/pset/lisp_pfindi :static (:r0 :r1) (:r0 :r1) (:r1-:r11 :r14 :f0-:f15)))
