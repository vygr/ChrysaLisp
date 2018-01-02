(import 'class/stream/stream.inc)

(def-class 'msg_in 'stream)
(def-method 'create 'class/msg_in/create 'static '() '(r0))
(def-method 'new 'class/msg_in/new 'static nil '(r0))
(def-method 'init 'class/msg_in/init 'static '(r0 r1))
(def-method 'next_seq 'class/msg_in/next_seq 'static '(r0 r1 r2) '(r0 r1))

(def-method 'deinit 'class/msg_in/deinit 'final)
(def-method 'read_ready 'class/msg_in/read_ready 'final)
(def-method 'read_next 'class/msg_in/read_next 'final)

(def-struct 'msg_in 'stream)
	(ulong 'seqnum)
	(ulong 'state)
	(struct 'mailbox 'mailbox)
	(struct 'list 'lh_list)
	(ulong 'ack_seqnum)
(def-struct-end)
