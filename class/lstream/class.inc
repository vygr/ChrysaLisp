(include "././stream/class.inc")
(include "./struct.inc")

(def-class :lstream :stream
	(dec-method :vtable class/lstream/vtable)
	(dec-method :type class/lstream/type :override :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	(dec-method :create class/lstream/create :static (:r0 :r1 :r2) (:r0) (:r0-:r5 :r14 :f0-:f15))
	(dec-method :init class/lstream/init :static (:r0 :r1 :r2 :r3 :r4) :nil (:r1-:r5))
	(dec-method :deinit class/lstream/deinit :final :nil :nil (:r1-:r2 :f0-:f15) (:r1-:r2 :f0-:f15))
	(dec-method :read_next class/lstream/read_next :final :nil :nil (:r1-:r3 :r14 :f0-:f15) (:r1-:r3 :r14 :f0-:f15))
	(dec-method :seek class/lstream/seek :final :nil :nil (:r1-:r3 :r14 :f0-:f15) (:r1-:r3 :r14 :f0-:f15))
	(dec-method :fill class/lstream/fill :static (:r0) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :seek_to class/lstream/seek_to :static (:r0 :r1 :r2) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_create class/lstream/lisp_create :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r14 :f0-:f15)))
