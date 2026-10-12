(include "././list/class.inc")
(include "./struct.inc")

(def-class :hset :list
	(dec-method :vtable class/hset/vtable)
	(dec-method :type class/hset/type :override :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	;a set is itself and no other, whatever the two hold. A list is the
	;same as another by what is in it, and this is not a list to those
	;who use it
	(dec-method :eql class/obj/eql :final :nil :nil (:r1) (:r1))
	(dec-method :create class/hset/create :static (:r0) (:r0) (:r0-:r5 :r14 :f0-:f15))
	(dec-method :init class/hset/init :static (:r0 :r1 :r2) :nil (:r1-:r5 :f0-:f15))
	(dec-method :bucket class/hset/bucket :static (:r0 :r1) (:r0 :r2) (:r1-:r6))
	(dec-method :cfind class/hset/cfind :static (:r0 :r1 :r2) (:r0 :r1 :r2) (:r1-:r12))
	(dec-method :cinsert class/hset/cinsert :static (:r0 :r1 :r2) (:r0 :r1 :r2) (:r1-:r13 :f0-:f15))
	(dec-method :each class/hset/each :static (:r0 :r1 :r2) (:r0) (:r1-:r14 :f0-:f15))
	(dec-method :flush class/hset/flush :static (:r0) (:r0) (:r1-:r14 :f0-:f15))
	(dec-method :each_callback class/obj/null :static (:r0 :r1 :r2) :nil ()))
