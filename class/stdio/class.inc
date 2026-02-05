(include "././obj/class.inc")
(include "./struct.inc")

(def-class :stdio :obj
	(dec-method :vtable class/stdio/vtable)
	(dec-method :create class/stdio/create :static :nil (:r0))
	(dec-method :init class/stdio/init :static (:r0 :r1))
	(dec-method :deinit class/stdio/deinit :final)
	(dec-method :lisp_create class/stdio/lisp_create :static (:r0 :r1) (:r0 :r1)))
