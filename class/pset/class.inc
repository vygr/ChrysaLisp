(include "././list/class.inc")
(include "./struct.inc")

(def-class :pset :list
	(dec-method :vtable class/pset/vtable)
	(dec-method :type class/pset/type :override)
	(dec-method :create class/pset/create)
	(dec-method :vcreate class/pset/create :override)
	(dec-method :pfind class/pset/find :virtual (:r0 :r1) (:r0 :r1 :r7 :r8))
	(dec-method :pinsert class/pset/insert :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pset class/pset/lisp_pset :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pfindi class/pset/lisp_pfindi :static (:r0 :r1) (:r0 :r1)))
