;import class settings
(bind (defq _syms_ '(
	byte_size short_size int_size long_size ptr_size in_mbox_id in_state out_state
	slave_args stream_mail_state_started stream_mail_state_stopping
	stream_mail_state_stopped))
	(within-compile-env (lambda ()
		(include 'sys/list/class.inc)
		(include 'sys/mail/class.inc)
		(include 'class/in/class.inc)
		(include 'class/out/class.inc)
		(include 'class/slave/class.inc)
		(map eval _syms_))))

;class lisp bindings
(ffi get-field "class/obj/lisp_get_field" 0)
(ffi set-field "class/obj/lisp_set_field" 0)
(ffi stream-avail "class/stream/lisp_available" 0)
(ffi stream-flush "class/stream/lisp_write_flush" 0)
(import 'class/num/lisp.inc)
(import 'class/out/lisp.inc)
(import 'class/in/lisp.inc)
(import 'class/slave/lisp.inc)
