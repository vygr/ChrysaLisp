(include "././str/class.inc")
(include "./struct.inc")

(def-class :entry :str
	(dec-method :vtable class/entry/vtable)
	(dec-method :type class/entry/type :override)
	(dec-method :create class/entry/create :static (:r0) (:r0))
	(dec-method :deinit class/entry/deinit :final))
