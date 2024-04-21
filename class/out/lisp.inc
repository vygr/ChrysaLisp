;lisp bindings
(ffi out-stream "class/out/lisp_create" 0)
; (out-stream mbox) -> out_stream

(enums +stream_mail_state 0
	(enum started stopping stopped))
