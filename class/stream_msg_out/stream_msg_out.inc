(import 'class/stream/stream.inc)

(def-class 'stream_msg_out 'stream)
(def-method 'create 'class/stream_msg_out/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/stream_msg_out/new 'static '() '(r0))
(def-method 'init 'class/stream_msg_out/init 'static '(r0 r1 r2 r3) '(r1))
(def-method 'next_seq 'class/stream_msg_out/next_seq 'static '(r0 r1 r2) '(r0))
(def-method 'set_state 'class/stream_msg_out/set_state 'static '(r0 r1))

(def-method 'deinit 'class/stream_msg_out/deinit 'override)
(def-method 'write_flush 'class/stream_msg_out/write_flush 'override)
(def-method 'write_next 'class/stream_msg_out/write_next 'override)

(def-struct 'stream_msg_out 'stream)
	(ulong 'stream_msg_out_seqnum)
	(ulong 'stream_msg_out_state)
	(struct 'stream_msg_out_id 'id)
	(ulong 'stream_msg_out_ack_seqnum)
	(struct 'stream_msg_out_ack_mailbox 'mailbox)
(def-struct-end)

(def-struct 'stream_mail 'msg_header)
	(ulong 'stream_mail_seqnum)
	(ulong 'stream_mail_state)
	(struct 'stream_mail_ack_id 'id)
	(offset 'stream_mail_data)
(def-struct-end)

(defcvar 'stream_msg_out_ack_shift 6)

(def-enum)
	(enum 'stream_mail_state_started)
	(enum 'stream_mail_state_stopping)
	(enum 'stream_mail_state_stopped)
(def-enum-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/stream_msg_out/set_state ()
	;inputs
	;r0 = stream_msg_out object
	;r1 = stream state
	(vp-cpy-ri r1 r0 stream_msg_out_state))
