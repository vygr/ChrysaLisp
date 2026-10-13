(include "././stream/class.inc")
(include "./struct.inc")

(def-class :fstream :stream
	(dec-method :vtable class/fstream/vtable)
	(dec-method :type class/fstream/type :override :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :create class/fstream/create :static (:r0 :r1) (:r0) (:r0-:r5 :r14 :f0-:f15))
	(dec-method :init class/fstream/init :static (:r0 :r1 :r2 :r3) :nil (:r1-:r5 :f0-:f15))
	(dec-method :deinit class/fstream/deinit :final :nil :nil (:r1-:r2 :f0-:f15) (:r1-:r2 :f0-:f15))
	(dec-method :read_next class/fstream/read_next :final :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :write_next class/fstream/write_next :final :nil :nil (:r1-:r2 :f0-:f15) (:r1-:r2 :f0-:f15))
	(dec-method :flush class/fstream/flush :final :nil :nil (:r1-:r2 :f0-:f15) (:r1-:r2 :f0-:f15))
	(dec-method :seek class/fstream/seek :final :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15)))

(defcvar 'fstream_buf_size 4096)
