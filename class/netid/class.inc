(include "././str/class.inc")
(include "./struct.inc")

(def-class :netid :str
	(dec-method :vtable class/netid/vtable)
	(dec-method :type class/netid/type :override :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :create class/netid/create :static () (:r0) (:r0-:r6 :f0-:f15))
	(dec-method :init class/netid/init :static (:r0 :r1) :nil (:r1-:r6 :f0-:f15))
	(dec-method :deinit class/netid/deinit :final :nil :nil (:r1-:r5) (:r1-:r5)))
