;import class settings
(bind (defq _syms_ '(msg_in_mbox_id))
	(within-compile-env (lambda ()
		(include 'sys/list/class.inc)
		(include 'class/msg_in/class.inc)
		(map eval _syms_))))

;class lisp bindings
(ffi get-field "class/obj/lisp_get_field" 0)
(ffi create-slave "class/slave/lisp_create" 0)
(ffi msg-out-stream "class/stream/lisp_msgoutstream" 0)
(ffi slave-get-args "class/slave/lisp_get_args" 0)
(ffi stream-avail "class/stream/lisp_available" 0)
(ffi stream-flush "class/stream/lisp_write_flush" 0)
(import 'class/msg_in/lisp.inc)
