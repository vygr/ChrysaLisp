(include "class/str/class.inc")

(def-class node str
	(dec-method :vtable class/node/vtable)
	(dec-method :create class/node/create :static (:r0 :r1) (:r0))
	(dec-method :hash class/node/hash :override))

(def-struct node str_size)
