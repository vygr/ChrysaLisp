(include "././stream/class.inc")

(def-class mstream stream
	(dec-method :vtable class/mstream/vtable)
	(dec-method :create class/mstream/create :static () (:r0))
	(dec-method :init class/mstream/init :static (:r0 :r1))
	(dec-method :deinit class/mstream/deinit :final)
	(dec-method :read_next class/mstream/read_next :final)
	(dec-method :write_next class/mstream/write_next :final)
	(dec-method :seek class/mstream/seek :final)
	(dec-method :flush class/mstream/flush :final)
	(dec-method :ptoi class/mstream/ptoi :static (:r0 :r1) (:r0 :r1 :r2 :r3 :r4))
	(dec-method :itop class/mstream/itop :static (:r0 :r1) (:r0 :r1 :r2 :r3 :r4 :r5)))

(def-struct mstream stream_size
	(ptr chunks)
	(uint pos total index))

(defcvar 'mstream_buf_size 4096)
