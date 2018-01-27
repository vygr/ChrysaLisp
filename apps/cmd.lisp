;import cmd settings
(bind '(slot_get_args slot_available slot_write_flush)
	(within-compile-env (lambda ()
		(import 'sys/code.inc)
		(import 'sys/mail/mail.inc)
		(import 'class/stream/stream.inc)
		(import 'class/slave/slave.inc)
		(list (method-slot 'slave 'get_args) (method-slot 'stream 'available)
			(method-slot 'stream 'write_flush)))))

;some helpful macros
(defmacro slot (_ &rest b) `(call ,(sym-cat 'slot_ _) ~b))

;system lisp bindings
(ffi create-slave "class/slave/lisp_create" 0)
