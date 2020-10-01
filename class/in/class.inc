(include "class/stream/class.inc")

(def-class 'in 'stream)
(dec-method :vtable 'class/in/vtable)
(dec-method :create 'class/in/create :static '(r0) '(r0))
(dec-method :init 'class/in/init :static '(r0 r1 r2))
(dec-method :next_msg 'class/in/next_msg :static '(r0) '(r0))

(dec-method :deinit 'class/in/deinit :final)
(dec-method :read_next 'class/in/read_next :final)

(dec-method :lisp_create 'class/in/lisp_create :static '(r0 r1) '(r0 r1))
(dec-method :lisp_next_msg 'class/in/lisp_next_msg :static '(r0 r1) '(r0 r1))

(def-struct 'in 'stream)
	(struct 'list 'lh_list)
	(ptr 'mbox_addr)
	(uint 'mbox_oid 'mbox_id 'ack_seqnum 'seqnum 'state)
(def-struct-end)
