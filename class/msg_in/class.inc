(include 'class/stream/class.inc)

(def-class 'msg_in 'stream)
(dec-method 'vtable 'class/msg_in/vtable)
(dec-method 'create 'class/msg_in/create 'static '() '(r0))
(dec-method 'init 'class/msg_in/init 'static '(r0 r1))
(dec-method 'next_msg 'class/msg_in/next_msg 'static '(r0) '(r0))

(dec-method 'deinit 'class/msg_in/deinit 'final)
(dec-method 'read_ready 'class/msg_in/read_ready 'final)
(dec-method 'read_next 'class/msg_in/read_next 'final)

(def-struct 'msg_in 'stream)
	(ulong 'seqnum 'state 'ack_seqnum)
	(struct 'list 'lh_list)
	(ptr 'mbox_addr)
	(uint 'mbox_id)
(def-struct-end)
