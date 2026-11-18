(include "././stream/class.inc")
(include "./struct.inc")

(def-class :lstream :stream
	(dec-method :vtable class/lstream/vtable)
	(dec-method :type class/lstream/type :override)
	(dec-method :create class/lstream/create :static (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :init class/lstream/init :static (:r0 :r1 :r2 :r3 :r4 :r5))
	(dec-method :deinit class/lstream/deinit :final)
	(dec-method :read_next class/lstream/read_next :final)
	(dec-method :seek class/lstream/seek :final)
	(dec-method :write_next class/lstream/write_next :final)
	(dec-method :flush class/lstream/flush :final)
	(dec-method :fault class/lstream/fault :final)
	(dec-method :put class/lstream/put :static (:r0 :r1) (:r0))
	(dec-method :fill class/lstream/fill :static (:r0) (:r0 :r1))
	(dec-method :seek_to class/lstream/seek_to :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_create class/lstream/lisp_create :static (:r0 :r1) (:r0 :r1)))

;how much is written to a stream before its write function is given it
(defcvar 'lstream_buf_size 4096)
