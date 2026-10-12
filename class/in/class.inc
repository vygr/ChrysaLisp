(include "././stream/class.inc")
(include "./struct.inc")

(def-class :in :stream
	(dec-method :vtable class/in/vtable)
	(dec-method :type class/in/type :override :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :create class/in/create :static (:r0) (:r0) (:r0-:r5 :r14 :f0-:f15))
	(dec-method :init class/in/init :static (:r0 :r1 :r2) :nil (:r1-:r5 :f0-:f15))
	(dec-method :next_msg class/in/next_msg :static (:r0) (:r0) (:r1-:r8 :f0-:f15))
	(dec-method :deinit class/in/deinit :final :nil :nil (:r1-:r14 :f0-:f15))
	(dec-method :read_next class/in/read_next :final :nil :nil (:r1-:r8 :f0-:f15))
	(dec-method :lisp_create class/in/lisp_create :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :r14 :f0-:f15))
	(dec-method :lisp_next_msg class/in/lisp_next_msg :static (:r0 :r1) (:r0 :r1) (:r1-:r8 :f0-:f15)))
