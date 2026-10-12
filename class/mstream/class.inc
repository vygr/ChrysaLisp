(include "././stream/class.inc")
(include "./struct.inc")

(def-class :mstream :stream
	(dec-method :vtable class/mstream/vtable)
	(dec-method :type class/mstream/type :override :nil :nil (:r1-:r5 :f0-:f15))
	(dec-method :create class/mstream/create :static () (:r0) (:r0-:r5 :f0-:f15))
	(dec-method :init class/mstream/init :static (:r0 :r1) :nil (:r1-:r5 :f0-:f15))
	(dec-method :deinit class/mstream/deinit :final :nil :nil (:r1-:r14 :f0-:f15))
	(dec-method :read_next class/mstream/read_next :final :nil :nil (:r1-:r6))
	(dec-method :write_next class/mstream/write_next :final :nil :nil (:r1-:r8 :f0-:f15))
	(dec-method :seek class/mstream/seek :final :nil :nil (:r1-:r11))
	(dec-method :flush class/mstream/flush :final :nil :nil (:r1-:r9))
	(dec-method :ptoi class/mstream/ptoi :static (:r0 :r1) (:r0 :r1 :r2 :r3) (:r2-:r9))
	(dec-method :itop class/mstream/itop :static (:r0 :r1) (:r0 :r1 :r2 :r3 :r4 :r5) (:r2-:r9)))

(defcvar 'mstream_buf_size 4096)
