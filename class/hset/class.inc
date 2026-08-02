(include "././list/class.inc")
(include "./struct.inc")

(def-class :hset :list
	(dec-method :vtable class/hset/vtable)
	(dec-method :type class/hset/type :override)
	(dec-method :create class/hset/create :static (:r0) (:r0))
	(dec-method :init class/hset/init :static (:r0 :r1 :r2))
	(dec-method :cfind class/hset/cfind :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :cinsert class/hset/cinsert :static (:r0 :r1 :r2) (:r0 :r1 :r2))
	(dec-method :bucket class/hset/bucket :static (:r0 :r1) (:r0 :r1))
	(dec-method :each class/hset/each :static (:r0 :r1 :r2) (:r0))
	(dec-method :flush class/hset/flush :static (:r0) (:r0))
	(dec-method :each_callback class/obj/null :static (:r0 :r1 :r2)))
