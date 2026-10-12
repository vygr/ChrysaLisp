(include "././num/class.inc")
(include "./struct.inc")

(def-class :func :obj
	(dec-method :vtable class/func/vtable)
	(dec-method :type class/func/type :override :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :create class/func/create :static (:r0) (:r0) (:r0-:r2 :r14 :f0-:f15))
	(dec-method :init class/num/init :static (:r0 :r1 :r2))
	(dec-method :print class/func/print :final :nil :nil (:r1-:r9 :r14 :f0-:f15) (:r1-:r9 :r14 :f0-:f15)))
