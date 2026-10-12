(include "././stream/class.inc")
(include "./struct.inc")

(def-class :out :stream
	(dec-method :vtable class/out/vtable)
	(dec-method :type class/out/type :override :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :create class/out/create :static (:r0 :r1 :r2) (:r0) (:r0-:r6 :r14 :f0-:f15))
	(dec-method :init class/out/init :static (:r0 :r1 :r2 :r3 :r4) :nil (:r1-:r6 :f0-:f15))
	(dec-method :set_state class/out/set_state :static (:r0 :r1) (:r0) (:r1-:r9 :f0-:f15))
	(dec-method :wait_acks class/out/wait_acks :static (:r0 :r1) (:r0) (:r2-:r3 :f0-:f15))
	(dec-method :deinit class/out/deinit :final :nil :nil (:r1-:r14 :f0-:f15))
	(dec-method :flush class/out/flush :final :nil :nil (:r1-:r9 :f0-:f15))
	(dec-method :write_next class/out/write_next :final :nil :nil (:r1-:r9 :f0-:f15))
	(dec-method :lisp_create class/out/lisp_create :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r14 :f0-:f15)))

(defcvar 'out_ack_shift 5)
