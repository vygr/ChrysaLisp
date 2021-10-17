(include "class/stream/class.inc")

(def-class out stream
	(dec-method :vtable class/out/vtable)
	(dec-method :create class/out/create :static (:r0 :r1 :r2) (:r0))
	(dec-method :init class/out/init :static (:r0 :r1 :r2 :r3 :r4))
	(dec-method :set_state class/out/set_state :static (:r0 :r1) (:r0))
	(dec-method :wait_acks class/out/wait_acks :static (:r0 :r1) (:r0))
	(dec-method :deinit class/out/deinit :final)
	(dec-method :flush class/out/flush :final)
	(dec-method :write_next class/out/write_next :final)
	(dec-method :lisp_create class/out/lisp_create :static (:r0 :r1) (:r0 :r1)))

(def-struct out stream_size
	(struct id net_id_size)
	(ptr ack_mbox_addr)
	(uint ack_mbox_id ack_seqnum seqnum state))

(def-struct stream_msg 0
	(struct ack_id net_id_size)
	(uint seqnum state)
	(offset data))

(defcvar 'out_ack_shift 5)

(def-enum stream_mail_state 0
	(enum started stopping stopped))
