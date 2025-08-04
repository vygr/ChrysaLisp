(include "././str/class.inc")

(def-class nodeid str
	(dec-method :vtable class/nodeid/vtable)
	(dec-method :create class/nodeid/create :static (:r0) (:r0))
	(dec-method :hash class/nodeid/hash :override))

(def-struct nodeid str_size
	(struct id node_id_size)
	(byte padding_null))
