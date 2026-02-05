(include "././str/class.inc")
(include "./struct.inc")

(def-class :nodeid :str
	(dec-method :vtable class/nodeid/vtable)
	(dec-method :create class/nodeid/create :static (:r0) (:r0))
	(dec-method :hash class/nodeid/hash :final))
