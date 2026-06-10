(include "././list/class.inc")
(include "./struct.inc")

(def-class :plist :list
	(dec-method :vtable class/plist/vtable)
	(dec-method :create class/plist/create)
	(dec-method :vcreate class/plist/create :final)
	(dec-method :pfind class/plist/find :static (:r0 :r1) (:r0 :r1))
	(dec-method :pinsert class/plist/insert :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_plist class/plist/lisp_plist :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pfind class/plist/lisp_find :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pfindi class/plist/lisp_findi :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_pinsert class/plist/lisp_insert :static (:r0 :r1) (:r0 :r1)))
