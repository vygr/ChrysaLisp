(include "././str/class.inc")
(include "./struct.inc")

(def-class :nodeid :str
	(dec-method :vtable class/nodeid/vtable)
	(dec-method :type class/nodeid/type :override :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :create class/nodeid/create :static (:r0) (:r0) (:r0-:r5 :f0-:f15))
	(dec-method :eql class/nodeid/eql :final :nil :nil (:r1-:r4) (:r1-:r4))
	(dec-method :hash class/nodeid/hash :final :nil :nil (:r1-:r2) (:r1-:r2)))
