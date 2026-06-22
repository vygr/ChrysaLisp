(include "././str/class.inc")
(include "./struct.inc")

(def-class :netid :str
	(dec-method :vtable class/netid/vtable)
	(dec-method :type class/netid/type :override)
	(dec-method :create class/netid/create :static () (:r0))
	(dec-method :init class/netid/init :static (:r0 :r1))
	(dec-method :deinit class/netid/deinit :final))
