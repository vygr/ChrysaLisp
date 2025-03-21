(include "././stream/class.inc")

(def-class fstream stream
	(dec-method :vtable class/fstream/vtable)
	(dec-method :create class/fstream/create :static (:r0 :r1) (:r0))
	(dec-method :init class/fstream/init :static (:r0 :r1 :r2 :r3))
	(dec-method :deinit class/fstream/deinit :final)
	(dec-method :read_next class/fstream/read_next :final)
	(dec-method :write_next class/fstream/write_next :final)
	(dec-method :flush class/fstream/flush :final)
	(dec-method :seek class/fstream/seek :final))

(def-struct fstream stream_size
	(long handle)
	(uint mode buf_len))

(defcvar 'fstream_buf_size 4096)
