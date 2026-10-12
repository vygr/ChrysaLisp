(include "././pset/class.inc")
(include "./struct.inc")

(def-class :pmap :pset
	(dec-method :vtable class/pmap/vtable)
	(dec-method :type class/pmap/type :override :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	(dec-method :create class/pmap/create :static :nil :nil (:r0-:r2 :f0-:f15))
	(dec-method :vcreate class/pmap/create :final :nil :nil (:r0-:r2 :f0-:f15) (:r0-:r2 :f0-:f15))
	(dec-method :pfind class/pmap/find :final (:r0 :r1) (:r0 :r1 :r7 :r8) (:r1-:r10) (:r1-:r10))
	(dec-method :pinsert class/pmap/insert :final (:r0 :r1 :r2) (:r0 :r1) (:r1-:r14 :f0-:f15) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_pmap class/pmap/lisp_pmap :static (:r0 :r1) (:r0 :r1) (:r2-:r3))
	(dec-method :lisp_pfind class/pmap/lisp_pfind :static (:r0 :r1) (:r0 :r1) (:r1-:r11))
	(dec-method :lisp_pinsert class/pmap/lisp_pinsert :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_perase class/pmap/lisp_perase :static (:r0 :r1) (:r0 :r1) (:r1-:r10 :r14 :f0-:f15)))
