(include "././str/class.inc")

(def-class netid str
	(dec-method :vtable class/netid/vtable)
	(dec-method :create class/netid/create :static () (:r0))
	(dec-method :init class/netid/init :static (:r0 :r1))
	(dec-method :deinit class/netid/deinit :final))

(def-struct netid str_size
	(struct id net_id_size)
	(byte cstr_nul))
