(include "././obj/class.inc")
(include "./struct.inc")

(def-class :stdio :obj
	(dec-method :vtable class/stdio/vtable)
	(dec-method :type class/stdio/type :override :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :create class/stdio/create :static :nil (:r0) (:r0-:r6 :r14 :f0-:f15))
	(dec-method :init class/stdio/init :static (:r0 :r1) :nil (:r1-:r6 :r14 :f0-:f15))
	(dec-method :deinit class/stdio/deinit :final :nil :nil (:r1-:r14 :f0-:f15))
	(dec-method :lisp_create class/stdio/lisp_create :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))
