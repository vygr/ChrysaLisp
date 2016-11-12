%ifndef CLASS_STREAM_MSG_IN_1234
%define CLASS_STREAM_MSG_IN_1234

(import 'class/class_stream.inc)

(def-class 'stream_msg_in 'stream)
(def-method 'create 'class/stream_msg_in/create 'static '(r0) '(r0))
(def-method 'new 'class/stream_msg_in/new 'static '() '(r0))
(def-method 'init 'class/stream_msg_in/init 'static '(r0 r1 r2) '(r1))

(def-method 'read_ready 'class/stream_msg_in/read_ready 'override)
(def-method 'read_next 'class/stream_msg_in/read_next 'override)

(def-struct 'stream_msg_in 'stream)
	(ulong 'stream_msg_in_seqnum)
	(ulong 'stream_msg_in_state)
	(ptr 'stream_msg_in_mailbox)
	(struct 'stream_msg_in_list 'lh_list)
	(ulong 'stream_msg_in_ack_seqnum)
(def-struct-end)

%endif
