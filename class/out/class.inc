(include 'class/stream/class.inc)

(def-class 'out 'stream)
(dec-method 'vtable 'class/out/vtable)
(dec-method 'create 'class/out/create :static '(r0) '(r0))
(dec-method 'init 'class/out/init :static '(r0 r1 r2))
(dec-method 'set_state 'class/out/set_state :static '(r0 r1) '(r0))
(dec-method 'wait_acks 'class/out/wait_acks :static '(r0 r1) '(r0))

(dec-method 'deinit 'class/out/deinit :final)
(dec-method 'write_flush 'class/out/write_flush :final)
(dec-method 'write_next 'class/out/write_next :final)

(dec-method 'lisp_create 'class/out/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'out 'stream)
	(struct 'id 'id)
	(ptr 'ack_mbox_addr)
	(uint 'ack_mbox_id 'ack_seqnum 'seqnum 'state)
(def-struct-end)

(def-struct 'stream_msg)
	(struct 'ack_id 'id)
	(uint 'seqnum 'state)
	(offset 'data)
(def-struct-end)

(defcvar 'out_ack_shift 5)

(def-enum 'stream_mail_state)
	(enum 'started 'stopping 'stopped)
(def-enum-end)
