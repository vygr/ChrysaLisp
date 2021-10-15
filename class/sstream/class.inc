(include "class/stream/class.inc")

(def-class sstream stream
	(dec-method :vtable class/sstream/vtable)
	(dec-method :create class/sstream/create :static (r0) (r0))
	(dec-method :init class/sstream/init :static (r0 r1 r2))
	(dec-method :ref_string class/sstream/ref_string :static (r0) (r0 r1))
	(dec-method :claim_string class/sstream/claim_string :static (r0) (r0 r1))
	(dec-method :write_next class/sstream/write_next :final)
	(dec-method :flush class/sstream/flush :final))

(def-struct sstream stream_size)
