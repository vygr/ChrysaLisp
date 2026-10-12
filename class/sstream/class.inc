(include "././stream/class.inc")
(include "./struct.inc")

(def-class :sstream :stream
	(dec-method :vtable class/sstream/vtable)
	(dec-method :type class/sstream/type :override :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :create class/sstream/create :static (:r0) (:r0) (:r0-:r5 :r14 :f0-:f15))
	(dec-method :init class/sstream/init :static (:r0 :r1 :r2) :nil (:r1 :r3-:r5))
	(dec-method :ref_string class/sstream/ref_string :static (:r0) (:r0 :r1) (:r1-:r2))
	(dec-method :claim_string class/sstream/claim_string :static (:r0) (:r0 :r1) (:r1-:r5 :f0-:f15))
	(dec-method :write_next class/sstream/write_next :final :nil :nil (:r1-:r14 :f0-:f15))
	(dec-method :seek class/sstream/seek :final :nil :nil (:r1 :r3-:r6))
	(dec-method :flush class/sstream/flush :final :nil :nil (:r1-:r3 :f0-:f15)))
