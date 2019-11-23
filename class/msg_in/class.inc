(include 'class/stream/class.inc)

(def-class 'msg_in 'stream)
(dec-method 'vtable 'class/msg_in/vtable)
(dec-method 'create 'class/msg_in/create 'static '() '(r0))
(dec-method 'init 'class/msg_in/init 'static '(r0 r1))
(dec-method 'next_msg 'class/msg_in/next_msg 'static '(r0) '(r0))

(dec-method 'deinit 'class/msg_in/deinit 'final)
(dec-method 'read_next 'class/msg_in/read_next 'final)

(dec-method 'lisp_next_msg 'class/msg_in/lisp_next_msg 'static '(r0 r1) '(r0 r1))

(def-struct 'msg_in 'stream)
	(struct 'list 'lh_list)
	(ptr 'mbox_addr)
	(uint 'mbox_id 'ack_seqnum 'seqnum 'state)
(def-struct-end)
