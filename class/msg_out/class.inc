(import 'class/stream/class.inc)

(def-class 'msg_out 'stream)
(dec-method 'vtable 'class/class_msg_out)
(dec-method 'create 'class/msg_out/create 'static '(r0) '(r0))
(dec-method 'new 'class/msg_out/new 'static nil '(r0))
(dec-method 'init 'class/msg_out/init 'static '(r0 r1 r2))
(dec-method 'set_state 'class/msg_out/set_state 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/msg_out/deinit 'final)
(dec-method 'write_flush 'class/msg_out/write_flush 'final)
(dec-method 'write_next 'class/msg_out/write_next 'final)

(def-struct 'msg_out 'stream)
	(ulong 'seqnum)
	(ulong 'state)
	(struct 'id 'id)
	(ulong 'ack_seqnum)
	(ptr 'ack_mbox_addr)
	(uint 'ack_mbox_id)
(def-struct-end)

(def-struct 'stream_msg)
	(ulong 'seqnum)
	(ulong 'state)
	(struct 'ack_id 'id)
	(offset 'data)
(def-struct-end)

(defcvar 'msg_out_ack_shift 6)

(def-enum 'stream_mail_state)
	(enum 'started 'stopping 'stopped)
(def-enum-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/msg_out/set_state ()
	;inputs
	;r0 = msg_out object (ptr)
	;r1 = stream state (ulong)
	(vp-cpy-ri r1 r0 msg_out_state))
