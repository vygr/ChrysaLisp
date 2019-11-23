;import class settings
(bind (defq _syms_ '(msg_in_mbox_id msg_in_state msg_out_state
	stream_mail_state_started stream_mail_state_stopping stream_mail_state_stopped))
	(within-compile-env (lambda ()
		(include 'sys/list/class.inc)
		(include 'sys/mail/class.inc)
		(include 'class/msg_in/class.inc)
		(include 'class/msg_out/class.inc)
		(map eval _syms_))))

;class lisp bindings
(ffi get-field "class/obj/lisp_get_field" 0)
(ffi set-field "class/obj/lisp_set_field" 0)
(ffi create-slave "class/slave/lisp_create" 0)
(ffi slave-get-args "class/slave/lisp_get_args" 0)
(ffi stream-avail "class/stream/lisp_available" 0)
(ffi stream-flush "class/stream/lisp_write_flush" 0)
(ffi stream-next "class/stream/lisp_write_next" 0)
(import 'class/msg_out/lisp.inc)
(import 'class/msg_in/lisp.inc)
