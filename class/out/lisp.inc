;lisp bindings
(ffi "class/out/lisp_create" out-stream)
; (out-stream mbox) -> out_stream

(enums +stream_mail_state 0
	(enum started stopping stopped))
